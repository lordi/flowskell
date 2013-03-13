
import Test.QuickCheck
import Parser
import Terminal

instance Arbitrary TerminalAction where
    arbitrary = \ _ -> (CharInput 'a')
