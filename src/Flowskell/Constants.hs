module Flowskell.Constants where
import qualified Paths_Flowskell as Flowskell
import Data.Version (showVersion)

programName = "Flowskell"
version = showVersion Flowskell.version
programNameWithVersion = programName ++ " " ++ version
helpText = programNameWithVersion ++ " | F1 Help | F2 REPL | F3 FPS | F5 Reload | F6 Reset view | F7 Screenshot"
prompt = ">>> "

