module Scramble.Test where

import ElmTest.Assertion (..)
import ElmTest.Run as R
import ElmTest.Runner.Element (runDisplay)
import ElmTest.Test (..)
-- import Json
-- import IO.IO (IO)
-- import IO.Runner (Request, Response)
-- import IO.Runner as Run

import Scramble.Trie
import Scramble.Trie as T

wordsL = [ "a", "to", "dot", "fan", "vat", "late", "cot", "fib"
         , "let", "not", "note", "eat", "ate", "pen", "ten"
         , "con", "cone", "geld", "tan", "if", "bed"
         ]
words = T.fromList wordsL
    
tests : Test    
tests = suite "Trie tests" [
         test "empty" <| assertEqual T.empty (T.fromList [])
        , test "empty 2" <| assertEqual [] (T.toList T.empty)
        , test "larger" <| assertEqual (T.insert "what" T.empty) (T.fromList ["what"]) 
        , suite "all members" <| map (\w -> test ("member " ++ w) <| assert <| T.member w words) wordsL
        , suite "insert idempotent" <| map (\w -> test ("insert " ++ w) <| assertEqual words (T.insert w words)) wordsL
        ]

main = runDisplay tests

-- console : IO ()
-- console = runDisplay tests

-- port requests : Signal Json.Value
-- port requests = Run.run responses console

-- port responses : Signal (Maybe String)
