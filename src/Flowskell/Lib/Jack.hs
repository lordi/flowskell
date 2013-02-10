module Flowskell.Lib.Jack where

import qualified Sound.JACK as Jack
import qualified Sound.JACK.Audio as JA
import qualified Sound.JACK.Exception as JackExc
import Sound.JACK (NFrames(NFrames), Process, Client, Port)

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Cont as MC
import qualified Control.Monad.Trans.Class as Trans
import Data.Foldable (forM_, )

import Foreign.Storable (sizeOf, peek, )
import Foreign.Ptr (nullPtr, )
import Foreign.C.Error (eOK, )

import qualified System.IO as IO
import Data.Array.Base (getNumElements, )
import Data.Array.Storable (readArray)

import Control.Concurrent
import Control.Monad hiding (forM_)

import System.Posix.Process
import System.Posix.IO
import System.Posix.Types (Fd)

import Data.Char (chr, ord)
import Control.Applicative (pure)

import Language.Scheme.Types

encode :: Double -> Char
encode dbl = chr (truncate ((dbl + 1.0) / 2.0 * 255.0))

decode :: Char -> Double
decode c = realToFrac ((toRational (ord c) / 255.0 * 2) - 1.0)

getActivation :: (MVar Double) -> [LispVal] -> IO LispVal
getActivation stvar [] = fmap Float (readMVar stvar)

initJack :: IO [(String, [LispVal] -> IO LispVal)]
initJack = do
    -- In this function we fork twice.
    --
    -- The first fork is a real system fork to provide us with two seperate
    -- processes. This is needed because the Jack clients run unstable when it
    -- runs together with OpenGLs mainLoop.
    -- For now, both proccesses communicate with a pipe.
    -- TODO: Exit process when pipe is closed/broken
    (rfd, wfd) <- createPipe
    forkProcess $ capture wfd "Flowskell" ["input"]

    -- The following fork just creates a thread which will read from the pipe
    -- and store the result in an MVar.
    stvar <- newMVar 0.0
    forkIO $ forever $ do
        (str, cnt) <- fdRead rfd 10
        case str of
            "" -> return 0.0
            _  -> swapMVar stvar (decode $ head str)

    -- Finally, return a Scheme function to access the MVar
    return [("snd-level", getActivation stvar)]

capture :: Fd -> String -> [String] -> IO ()
capture wfd name portNames =
    Jack.handleExceptions $ flip MC.runContT return $ do
        client <- MC.ContT $ Jack.withClientDefault name
        inputs <- mapM (MC.ContT . Jack.withPort client) portNames
        Trans.lift $ setProcess wfd client inputs
        Trans.lift $ Jack.withActivation client $ Trans.lift $ do
            putStr $ "started " ++ name ++ "..."
            Jack.waitForBreak

setProcess ::
    (JackExc.ThrowsErrno e) =>
    Fd ->
    Client ->
    [Port JA.Sample Jack.Input] ->
    Sync.ExceptionalT e IO ()
setProcess wfd client input =
    flip (Jack.setProcess client) nullPtr =<<
    (Trans.lift $ Jack.makeProcess $
     wrapFun wfd input)

wrapFun ::
    Fd ->
    [Port JA.Sample Jack.Input] ->
    Process a
wrapFun wfd inputs nframes _args = do
    let send = fdWrite wfd . pure . encode
    inArrs <- mapM (flip JA.getBufferArray nframes) inputs
    forM_ inArrs $ \i -> do
        samples <- mapM (readArray i) (Jack.nframesIndices nframes)
        send $ maximum (map realToFrac samples)
    return eOK
