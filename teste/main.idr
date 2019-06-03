module Main

data Token = TokenVarId String
       | TokenAssign
       | TokenEq
       | TokenPlus
       | TokenMinus
       | TokenTimes
       | TokenDiv
       | TokenLParen
       | TokenRParen
       | TokenNot
       | TokenInt Int
       | TokenMaior
       | TokenMenor
       | TokenMaiorIgual
       | TokenMenorIgual
       | TokenAnd
       | TokenOr
       | TokenWhile
       | TokenLBrace
       | TokenRBrace
       | TokenTrue
       | TokenFalse
       | TokenNop

implementation Show Token where
  show (TokenInt n) = "TokenInt " ++ show n
  show (TokenNop) = "TokenNop"
  show (TokenPlus) = "TokenPlus"
  show (TokenMinus) = "TokenMinus"
  show (TokenTimes) = "TokenTimes"
  show (TokenDiv) = "TokenDiv"
  show (TokenLParen) = "TokenLParen"
  show (TokenRParen) = "TokenRParen"
  show (TokenTrue) = "TokenTrue"
  show (TokenFalse) = "TokenFalse"

tokenizer : String -> Token
tokenizer "" = TokenNop
tokenizer "+" = TokenPlus
tokenizer "-" = TokenMinus
tokenizer "*" = TokenTimes
tokenizer "/" = TokenDiv
tokenizer "(" = TokenLParen
tokenizer ")" = TokenRParen
tokenizer "True" = TokenTrue
tokenizer "False" = TokenFalse
tokenizer str = case all isDigit (unpack str) of
  True => TokenInt (cast str)
  False => TokenNop

read_token: List Char -> String -> List Token -> List Token
read_token [] v list = if v == "" then reverse list else reverse (tokenizer v ::list)
read_token (' ' :: xs) v list = read_token xs v list
read_token ('(':: xs) v list = if v=="" then read_token xs v (TokenLParen :: list) else read_token xs "" (TokenLParen :: (tokenizer v :: list))
read_token (')':: xs) v list = if v=="" then read_token xs v (TokenRParen :: list) else read_token xs "" (TokenRParen :: (tokenizer v :: list))
read_token ('+':: xs) v list = if v=="" then read_token xs v (TokenPlus :: list) else read_token xs "" (TokenPlus :: (tokenizer v :: list))
read_token ('-':: xs) v list = if v=="" then read_token xs v (TokenMinus :: list) else read_token xs "" (TokenMinus :: (tokenizer v :: list))
read_token ('*':: xs) v list = if v=="" then read_token xs v (TokenTimes :: list) else read_token xs "" (TokenTimes :: (tokenizer v :: list))
read_token ('/':: xs) v list = if v=="" then read_token xs v (TokenDiv :: list) else read_token xs "" (TokenDiv :: (tokenizer v :: list))
read_token (x :: xs) v list = read_token xs ( v ++ (singleton x)) list

main : IO ()
main = repl "Enter a string: " read_input
  where
    read_input : String -> String
    read_input x = show (read_token (unpack x) "" []) ++ "\n"
