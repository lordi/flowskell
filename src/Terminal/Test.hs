import Test.QuickCheck
import Control.Applicative ((<$>))
import Parser
import Terminal
import Types

instance Arbitrary TerminalAction where
    arbitrary = oneof [
        CharInput <$> choose ('a', 'Z'),
        CursorUp <$> choose (1,5),
        CursorDown <$> choose (1,5),
        CursorForward <$> choose (1,5),
        CursorBackward <$> choose (1,5)
        ]

handleActions [] t = t
handleActions (a : as) t = handleActions as (handleAction a t)

prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

main = quickCheck prop_SafeCursor
