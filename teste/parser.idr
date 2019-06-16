module parser

import lexer

%access public export

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | Paren AExp

implementation Show AExp where
  show (Sum a b) = "Sum " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Sub a b) = "Sub " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Mul a b) = "Mul " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Div a b) = "Div " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (N a) = "N " ++ show a
  show (Paren a) = "(" ++ show a ++ ")"
  -- show (ID a) = "ID " ++ show a

-- Corrige a precedencia das operações, precedencia a esquerda
reduce : List AExp -> (AExp -> AExp -> AExp) -> AExp
reduce [x] f = x
reduce (x::y::xs) f = reduce ((f x y) :: xs) f

mutual

  num : List Token -> (AExp, List Token)
  num ((TokenInt n)::l') = (N n, l')

  factor : List Token -> (AExp, List Token)
  factor ((TokenInt n)::xs) = num ((TokenInt n)::xs)
  factor ((TokenLParen)::xs) = let (e,r) = arithExp xs in factorAux e r where
    factorAux : AExp -> List Token -> (AExp, List Token)
    factorAux e [] = (e,[])
    factorAux e (TokenRParen :: ys) = ((Paren e),ys)

  mul : List Token -> (AExp, List Token)
  mul l = let (exp, r) = factor l in mulAux exp r where
    mulAux : AExp -> List Token -> (AExp, List Token)
    mulAux e [] = (e,[])
    mulAux e ((TokenTimes)::xs) = let (exp', r2) = mul xs in (Mul e exp', r2)
    mulAux e ((TokenDiv)::xs) = let (exp', r2) = mul xs in (reduce (divToList (Div e exp')) Div, r2)
    mulAux e l = (e,l)


  sum : List Token -> (AExp, List Token)
  sum l = let (exp, r) = mul l in sumAux exp r where
    sumAux : AExp -> List Token -> (AExp, List Token)
    sumAux e [] = (e,[])
    sumAux e ((TokenPlus)::xs) = let (exp', r2) = sum xs in (Sum e exp', r2)
    sumAux e ((TokenMinus)::xs) = let (exp', r2) = sum xs in (reduce (subToList (Sub e exp')) Sub, r2)
    sumAux e l = (e,l)

-- funções usadas para o transformar AExps em Listas
  subToList : AExp -> List AExp
  subToList (Sub a b) = (subToList a) ++ (subToList b)
  subToList a = [a]

  divToList : AExp -> List AExp
  divToList (Div a b) = (divToList a) ++ (divToList b)
  divToList a = [a]

  arithExp : List Token -> (AExp, List Token)
  arithExp = sum

removeParen : AExp -> AExp
removeParen (Sum a b) = Sum (removeParen a) (removeParen b)
removeParen (Sub a b) = Sub (removeParen a) (removeParen b)
removeParen (Mul a b) = Mul (removeParen a) (removeParen b)
removeParen (Div a b) = Div (removeParen a) (removeParen b)
removeParen (Paren e) = removeParen e
removeParen (N n) = N n

parse : (AExp, List Token) -> AExp
parse (exp, l) = exp
