module Main

import tipos
import lexer
import parser

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = show (transformPi(parse(arithExp (read_token (unpack x) "" [])))) ++ "\n"
