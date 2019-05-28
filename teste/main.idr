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

transformaString: Show a => a -> String
transformaString n = show n

implementation Show Token where
  show (TokenInt n) = "TokenInt " ++ transformaString n
  show (TokenNop) = "TokenNop"

retiraMaybe: Maybe Int -> Int
retiraMaybe (Just x) = x
retiraMaybe Nothing = 0


tokenizer : String -> Token
tokenizer "" = TokenNop
tokenizer str = if all isDigit (unpack str) then TokenInt (cast str) else TokenNop


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
