module Main

import tipos
import lexer
import parser
import piAutomata
import Data.SortedMap

verificaList : List Token -> Bool
verificaList [] = True
verificaList l = False

--removeLast:

printPi: (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> String
printPi (c, v, e, s, l) = "Pilha de Controle: " ++ show c ++ "\n" ++ "Pilha de Valores: " ++ show v ++ "\n" ++ "Enviroment: " ++ show (toList e) ++ "\n" ++ "Stored: " ++ show (toList s) ++ "\n" ++ "Lista de Locations: " ++ show l ++ "\n \n##############################################################\n \n"

main : IO ()
main = repl "Enter a program: " read_input
  -- where
  --  leParametro : String -> String
  --  leParametro str = let palavras = words str in let p = elem "--last" palavras case p of
  --    True => read_input (removeLast str)
  --    False => read_input str
  where
    read_input : String -> String
    read_input x = let (e, l) = (ctrlParser (read_token (unpack x) "" [])) in case e of
      Nothing  => "Parser Error! \n" ++ show l ++ "\n"
      Just k => if (verificaList l) then let (resp, (x::xs)) = process ([k], [], empty, empty, []) []
         in unwords(map printPi (x::xs)) else "Parser Error! \n" ++ show l ++ "\n"
    -- read_input x = show (ctrlParser (read_token (unpack x) "" [])) ++ "\n"
