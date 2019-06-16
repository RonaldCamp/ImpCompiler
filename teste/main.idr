module Main

import parser

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = show (removeParen (parse(arithExp (read_token (unpack x) "" [])))) ++ "\n"

-- main : IO ()
-- main = repl "Enter a program: " read_input
--   where
--     read_input : String -> String
--     read_input x = show (read_token (unpack x) "" []) ++ "\n"
