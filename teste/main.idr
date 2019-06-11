module Main

import lexer

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | Paren AExp

implementation Show AExp where
  show (Sum a b) = "Sum " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Sub a b) = "Sub " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Mul a b) = "Mul " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Div a b) = "Div " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (N a) = "N " ++ show a
  show (Paren a) = "(" ++ show a ++ ")"
  -- show (ID a) = "ID " ++ show a

-- data ExpArit = (TokenInt n) TokenPlus (TokenInt n)

-- getToken : List Token -> Token
-- getToken [] = TokenNop
-- getToken (x::xs) = x

-- getToken [TokenInt 2, TokenPlus, TokenInt 3]

-- parseExp : List Token -> AExp
-- parseExp ((TokenInt  n1)::TokenPlus::(TokenInt n2)::xs) = Sum (N n1)  (N n2)
-- parseExp ((TokenInt  n)::xs) = N n
-- parseExp ((TokenInt n1)::TokenPlus::(TokenInt n2)::xs) = Sum (N n1) (N n2)
-- parse TokenPlus = Sum parseExp
--
-- parse : List Token -> List AExp -> List AExp
-- parse [] lAexp = lAexp
-- parse (x::xs) lAexp = if x == (TokenInt) then parse xs (x::lAexp) else parse xs lAexp

-- parse : List Token -> Aexp

-- parse : List Token -> List Int -> AExp
-- parse [] (l::[]) = N l
-- parse ((TokenInt n)::xs) list = if list == [] then parse xs (n::list) else N n
-- parse (TokenPlus::xs) (l::ls) = Sum (parse xs []) (N l)
-- parse (TokenMinus::xs) (l::ls) = Sub (parse xs []) (N l)
-- parse (TokenTimes::xs) (l::ls) = Mul (parse xs []) (N l)
-- parse (TokenDiv::xs) (l::ls) = Div (parse xs []) (N l)


-- arithExpression : List Token -> (AExp, List Token)
-- arithExpression = sum

-- sum : List Token -> (AExp, List Token)
-- sum l =  case r of
--           [] => exp
--           (TokenPlus::l') => Sum exp exp'
--                                 where (exp', _) = mul l'
--           otherwise => exp
--             where (exp, r) = mul l

reduce : List AExp -> (AExp -> AExp -> AExp) -> AExp
reduce [x] f = x
reduce (x::y::xs) f = reduce ((f x y) :: xs) f


mutual

  num : List Token -> (AExp, List Token)
  num ((TokenInt n)::l') = (N n, l')

  factor : List Token -> (AExp, List Token)
  factor ((TokenInt n)::xs) = num ((TokenInt n)::xs)
  factor ((TokenLParen)::xs) = let (e,r) = arithExp xs in
                                factorAux e r
                                where
                                  factorAux : AExp -> List Token -> (AExp, List Token)
                                  factorAux e [] = (e,[])
                                  factorAux e (TokenRParen :: ys) = ((Paren e),ys)

  mul : List Token -> (AExp, List Token)
  mul l = let (exp, r) = factor l in
            mulAux exp r
            where
              mulAux : AExp -> List Token -> (AExp, List Token)
              mulAux e [] = (e,[])
              mulAux e ((TokenTimes)::xs) = let (exp', r2) = mul xs in
                                                (Mul e exp', r2)
              mulAux e l = (e,l)
              --
              -- if r == []
              -- then
              --   (exp, [])
              -- else
              --   if (head r) == TokenTimes
              --   then
              --     let (exp', r2) = mul (tail r) in
              --       (Mul exp exp', r2)
              --   else (exp, r)
              --

  sum : List Token -> (AExp, List Token)
  sum l = let (exp, r) = mul l in
            sumAux exp r
            where
              sumAux : AExp -> List Token -> (AExp, List Token)
              sumAux e [] = (e,[])
              sumAux e ((TokenPlus)::xs) = let (exp', r2) = sum xs in
                                                (Sum e exp', r2)
              sumAux e l = (e,l)
  subToList : AExp -> List AExp
  subToList (Sub a b) = (subToList a) ++ (subToList b)
  subToList a = [a]

  sub : List Token -> (AExp, List Token)
  sub l = let (exp, r) = mul l in
          subAux exp r
          where
            subAux : AExp -> List Token -> (AExp, List Token)
            subAux e [] = (e,[])
            subAux e ((TokenMinus)::xs) = let (exp', r2) = sub xs in
                                              (reduce (subToList (Sub e exp')) Sub, r2)
            subAux e l = (e,l)


  -- parse [TokenInt 2, TokenPlus, TokenInt 3, TokenMinus, TokenInt 1] []
  arithExp : List Token -> (AExp, List Token)
  arithExp = sub

main : IO ()
main = repl "Enter a program: " read_input
  where
    read_input : String -> String
    read_input x = show (arithExp (read_token (unpack x) "" [])) ++ "\n"


-- main : IO ()
-- main = repl "Enter a program: " read_input
--   where
--     read_input : String -> String
--     read_input x = show (parse list []) ++ "\n" where
--       list = (read_token (unpack x) "" [])
