import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = do
    hints <- hlint [ "app", "src", "test" ]
    if null hints then exitSuccess else exitFailure
