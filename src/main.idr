module Main

import tipos
import lexer
import parser
import piAutomata
import Data.SortedMap

removeJust: (Maybe a, List Token) -> a
removeJust (Just x, l) = x

printPi: (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> String
printPi (c, v, e, s, l) = "Pilha de Controle: " ++ show c ++ "\n" ++ "Pilha de Valores: " ++ show v ++ "\n" ++ "Enviroment: " ++ show (toList e) ++ "\n" ++ "Stored" ++ show (toList s) ++ "\n" ++ "Lista de Locations: " ++ show l ++ "\n"

-- reduce: ((List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc), List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc)) -> List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc)
-- reduce ((c, v, env , stored, l), lista) = lista

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = let e = removeJust(exp (read_token (unpack x) "" [])) in
      let (resp, list) = process ([CtExp e], [], empty, empty, []) [] in show (map printPi list)

    -- read_input x = show (exp (read_token (unpack x) "" [])) ++ "\n"
    -- read_input x = let e = removeJust(exp (read_token (unpack x) "" [])) in show e ++ "\n"
    -- read_input x = let e = removeJust(exp (read_token (unpack x) "" [])) in (printPi ([CtExp e], [], empty, empty, [])) ++ "\n"
