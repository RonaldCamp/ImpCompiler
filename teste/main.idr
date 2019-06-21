module Main

import tipos
import lexer
import parser
import piAutomata

removeJust: (Maybe BExp, List Token) -> BExp
removeJust (Just x, l) = x

chamada: ((List Ctrl, List Val, SortedMap Val Bindable , SortedMap Loc Val, List Loc), List (List Ctrl, List Val, SortedMap Val Bindable , SortedMap Loc Val, List Loc)) -> String
chamada (pi, lista) = show lista

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    -- read_input x = show (transformPi(parse(arithExp (read_token (unpack x) "" [])))) ++ "\n"
    read_input x = let exp = (removeJust (boolExp (read_token (unpack x) "" []))) in
    (show chamada (process ([exp],[],empty,empty,[]) ([]) )) ++ "\n"
