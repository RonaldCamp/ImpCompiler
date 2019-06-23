module Main

import tipos
import lexer
import parser
import piAutomata
import Data.SortedMap

removeJust: (Maybe a, List Token) -> a
removeJust (Just x, l) = x

-- chamada: ((List Ctrl, List Val, SortedMap Val Bindable , SortedMap Loc Val, List Loc), List (List Ctrl, List Val, SortedMap Val Bindable , SortedMap Loc Val, List Loc)) -> String
-- chamada (((c, v, env , stored, l)), lista) = (show l)

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = x ++ "\n" ++ show ([CtCmd (Blk (DSeq (Bind (ValID "x")  (AExpR (N 5))) (Bind (ValID "y")  (Ref (AExpR (N 0))))) (Assign (ValID "y") (AExpR (Sum (N 1) (ID (ValID "x"))))))]) ++ "\n"
    -- read_input x = show (boolExp (read_token (unpack x) "" [])) ++ "\n"
    -- read_input x = show (printPi ([CtCmd (Blk (DSeq (Bind (ValID "x")  (AExpR (N 5))) (Bind (ValID "y")  (Ref (AExpR (N 0))))) (Assign (ValID "y") (AExpR (Sum (N 1) (ID (ValID "x"))))))], [], empty, empty, [])) ++ "\n"
    -- read_input x = let exp = (removeJust (boolExp (read_token (unpack x) "" []))) in
    -- (show (chamada (process ([CtExp (BExpR exp)],[],empty,empty,[]) ([]) )))
