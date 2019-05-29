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

main : IO ()
main = repl "Enter a string: " show_palindrome
  where
    show_palindrome : String -> String
    show_palindrome x = show (map tokenizer (words x)) ++ "\n"


readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
      then pure (Just (cast input))
      else pure Nothing
