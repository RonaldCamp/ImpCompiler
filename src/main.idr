module Main

import tipos
import lexer
import parser
import piAutomata
import Data.SortedMap

verificaList : List Token -> Bool
verificaList [] = True
verificaList l = False

removeParam : String -> String -> String
removeParam str p = let list = words str in removeAux list p where
  removeAux : List String -> String -> String
  removeAux x p' = unwords (filter ((/=) p') x)

printPi: (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> String
printPi (c, v, e, s, l) = "\n##############################################################\n\nPilha de Controle: " ++ show c ++ "\n\n" ++ "Pilha de Valores: " ++ show v ++ "\n\n" ++ "Enviroment: " ++ show (toList e) ++ "\n\n" ++ "Stored: " ++ show (toList s) ++ "\n\n" ++ "Lista de Locations: " ++ show l ++ "\n"

read_input : String -> String -> String
read_input x "" = let (e, l) = (ctrlParser (read_token (unpack x) "" [])) in case e of
  Nothing  => "Parser Error! \n" ++ show l ++ "\n"
  Just k => if (verificaList l) then let (resp, (x1::x2::xs)) = process ([k], [], empty, empty, []) []
     in unwords(map printPi (x1::[x2])) else "Parser Error! \n" ++ show l ++ "\n"
read_input x "--all" = let (e, l) = (ctrlParser (read_token (unpack x) "" [])) in case e of
 Nothing  => "Parser Error! \n" ++ show l ++ "\n"
 Just k => if (verificaList l) then let (resp, (x::xs)) = process ([k], [], empty, empty, []) []
    in unwords(map printPi (x::xs)) else "Parser Error! \n" ++ show l ++ "\n"
read_input x "--a" = let (e, l) = (ctrlParser (read_token (unpack x) "" [])) in case e of
 Nothing  => "Parser Error! \n" ++ show l ++ "\n"
 Just k => if (verificaList l) then let (resp, (x1::x2::xs)) = process ([k], [], empty, empty, []) []
    in "\n" ++ show e ++ "\n" ++ unwords(map printPi (x1::[x2])) else "Parser Error! \n" ++ show l ++ "\n"

main : IO ()
main = repl "Enter a program: " leParametro where
  leParametro : String -> String
  leParametro str = let palavras = words str in case palavras of
    [] => ""
    (x::xs) => let p = (last (x::xs)) in case p of
      "--all" => read_input (removeParam str "--all") "--all"
      "--a" => read_input (removeParam str "--a") "--a"
      x => read_input str ""

    -- read_input x = show (ctrlParser (read_token (unpack x) "" [])) ++ "\n"
