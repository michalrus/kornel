import Language.Haskell.HLint
import System.Exit
import System.FilePath.Glob (glob)

main :: IO ()
main = do
  putStrLn ""
  hints <- glob "**/*.hs" >>= hlint
  if null hints
    then exitSuccess
    else exitFailure
