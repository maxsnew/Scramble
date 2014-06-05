import Data.Char
import Data.List
import qualified Data.Set as Set

main = file >>= writeFile "Scramble/Words.elm"

prelude = unlines [
    "module Scramble.Words where"
  , ""
  , "import Scramble.Trie as T"
  , ""
  ] ++ "words = T.fromList "

wordsF = "/usr/share/dict/words"

isScramble :: String -> Bool
isScramble = allM [ all isAsciiLower
                  , ((> 2) . length)
                  ]

file :: IO String
file = do
  rawWords <- readFile wordsF
  return . (prelude ++) . show . filter isScramble . lines $ rawWords

allM :: Monad m => [m Bool] -> m Bool
allM = foldr andM (return True) 

andM m1 m2 = do
  b <- m1
  if b
    then m2
    else return False
