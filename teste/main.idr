module Main

import lexer

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int

implementation Show AExp where
  show (Sum a b) = "Sum " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Sub a b) = "Sub " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Mul a b) = "Mul " ++ show a ++ " " ++ show b
  show (Div a b) = "Div " ++ show a ++ " " ++ show b
  show (N a) = "N " ++ show a
  -- show (ID a) = "ID " ++ show a

-- data ExpArit = (TokenInt n) TokenPlus (TokenInt n)

getToken : List Token -> Token
getToken [] = TokenNop
getToken (x::xs) = x

-- getToken [TokenInt 2, TokenPlus, TokenInt 3]

parseExp : List Token -> AExp
parseExp ((TokenInt  n1)::TokenPlus::(TokenInt n2)::xs) = Sum (N n1)  (N n2)
parseExp ((TokenInt  n)::xs) = N n
-- parseExp ((TokenInt n1)::TokenPlus::(TokenInt n2)::xs) = Sum (N n1) (N n2)
-- parse TokenPlus = Sum parseExp
--
-- parse : List Token -> List AExp -> List AExp
-- parse [] lAexp = lAexp
-- parse (x::xs) lAexp = if x == (TokenInt) then parse xs (x::lAexp) else parse xs lAexp

-- parse : List Token -> Aexp

parse : List Token -> List Int -> AExp
parse [] (l::[]) = N l
parse ((TokenInt n)::xs) list = if list == [] then parse xs (n::list) else N n
parse (TokenPlus::xs) (l::ls) = Sum (parse xs []) (N l)
parse (TokenMinus::xs) (l::ls) = Sub (parse xs []) (N l)
parse (TokenTimes::xs) (l::ls) = Mul (parse xs []) (N l)
parse (TokenDiv::xs) (l::ls) = Div (parse xs []) (N l)


-- parse [TokenInt 2, TokenPlus, TokenInt 3, TokenMinus, TokenInt 1] []


-- main : IO ()
-- main = repl "Enter a program: " read_input
--   where
--     read_input : String -> String
--     read_input x = show (read_token (unpack x) "" []) ++ "\n"


main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = show (parse list []) ++ "\n" where
      list = (read_token (unpack x) "" [])
