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
       | TokenTrue
       | TokenFalse
       | TokenIf
       | TokenThen
       | TokenElse
       | TokenWhile
       | TokenLet
       | TokenIn
       | TokenVar
       | TokenLBrace
       | TokenRBrace
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
  show (TokenMaior) = "TokenMaior"
  show (TokenMenor) = "TokenMenor"
  show (TokenMaiorIgual) = "TokenMaiorIgual"
  show (TokenMenorIgual) = "TokenMenorIgual"
  show (TokenEq) = "TokenEq"
  show (TokenAnd) = "TokenAnd"
  show (TokenOr) = "TokenOr"
  show (TokenNot) = "TokenNot"
  show (TokenVarId id) = "TokenVarId " ++ show id
  show (TokenLBrace) = "TokenLBrace"
  show (TokenRBrace) = "TokenRBrace"
  show (TokenAssign) = "TokenAssign"


isVarId : List Char -> Bool
isVarId [] = False
isVarId (x::[]) = if isAlpha x then True else False
isVarId (x::xs) =  if isAlpha x && all isAlphaNum xs then True else False

tokenizer : String -> Token
tokenizer "" = TokenNop
tokenizer "+" = TokenPlus
tokenizer "-" = TokenMinus
tokenizer "*" = TokenTimes
tokenizer "/" = TokenDiv
tokenizer "(" = TokenLParen
tokenizer ")" = TokenRParen
tokenizer ">" = TokenMaior
tokenizer "<" = TokenMenor
tokenizer ">=" = TokenMaiorIgual
tokenizer "<=" = TokenMenorIgual
tokenizer "==" = TokenEq
tokenizer "&&" = TokenAnd
tokenizer "||" = TokenOr
-- tokenizer "and" = TokenAnd
-- tokenizer "or" = TokenOr
tokenizer "not" = TokenNot
tokenizer "True" = TokenTrue
tokenizer "False" = TokenFalse
tokenizer ":=" = TokenAssign
tokenizer "{" = TokenLBrace
tokenizer "}" = TokenRBrace
tokenizer str = case all isDigit (unpack str) of
  True => TokenInt (cast str)
  False => if isVarId (unpack str) then TokenVarId str else TokenNop


read_token: List Char -> String -> List Token -> List Token
read_token [] v list = if v == "" then reverse list else reverse (tokenizer v ::list)
read_token (' ' :: xs) v list = read_token xs v list
read_token ('(':: xs) v list = if v=="" then read_token xs v (TokenLParen :: list) else read_token xs "" (TokenLParen :: (tokenizer v :: list))
read_token (')':: xs) v list = if v=="" then read_token xs v (TokenRParen :: list) else read_token xs "" (TokenRParen :: (tokenizer v :: list))
read_token ('+':: xs) v list = if v=="" then read_token xs v (TokenPlus :: list) else read_token xs "" (TokenPlus :: (tokenizer v :: list))
read_token ('-':: xs) v list = if v=="" then read_token xs v (TokenMinus :: list) else read_token xs "" (TokenMinus :: (tokenizer v :: list))
read_token ('*':: xs) v list = if v=="" then read_token xs v (TokenTimes :: list) else read_token xs "" (TokenTimes :: (tokenizer v :: list))
read_token ('/':: xs) v list = if v=="" then read_token xs v (TokenDiv :: list) else read_token xs "" (TokenDiv :: (tokenizer v :: list))
read_token ('='::'='::xs) v list = if v=="" then read_token xs v (TokenEq :: list) else read_token xs "" (TokenEq :: (tokenizer v :: list))
read_token ('>'::'='::xs) v list = if v=="" then read_token xs v (TokenMaiorIgual :: list) else read_token xs "" (TokenMaiorIgual :: (tokenizer v :: list))
read_token ('<'::'='::xs) v list = if v=="" then read_token xs v (TokenMenorIgual :: list) else read_token xs "" (TokenMenorIgual :: (tokenizer v :: list))
read_token ('>':: xs) v list = if v=="" then read_token xs v (TokenMaior :: list) else read_token xs "" (TokenMaior :: (tokenizer v :: list))
read_token ('<':: xs) v list = if v=="" then read_token xs v (TokenMenor :: list) else read_token xs "" (TokenMenor :: (tokenizer v :: list))
-- read_token ('a'::'n'::'d'::xs) v list = if v=="" then read_token xs v (TokenAnd :: list) else read_token xs "" (TokenAnd :: (tokenizer v :: list))
-- read_token ('o'::'r'::xs) v list = if v=="" then read_token xs v (TokenOr :: list) else read_token xs "" (TokenOr :: (tokenizer v :: list))
read_token ('&'::'&'::xs) v list = if v=="" then read_token xs v (TokenAnd :: list) else read_token xs "" (TokenAnd :: (tokenizer v :: list))
read_token ('|'::'|'::xs) v list = if v=="" then read_token xs v (TokenOr :: list) else read_token xs "" (TokenOr :: (tokenizer v :: list))
read_token (':'::'='::xs) v list = if v=="" then read_token xs v (TokenAssign :: list) else read_token xs "" (TokenAssign :: (tokenizer v :: list))
read_token ('{':: xs) v list = if v=="" then read_token xs v (TokenLBrace :: list) else read_token xs "" (TokenLBrace :: (tokenizer v :: list))
read_token ('}':: xs) v list = if v=="" then read_token xs v (TokenRBrace :: list) else read_token xs "" (TokenRBrace :: (tokenizer v :: list))
read_token (x :: xs) v list = read_token xs ( v ++ (singleton x)) list

main : IO ()
main = repl "Enter a string: " read_input
  where
    read_input : String -> String
    read_input x = show (read_token (unpack x) "" []) ++ "\n"
