module parser

import lexer
import tipos

%access public export

-- data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | Paren AExp
-- data Exp = AExpR AExp
-- data Ctrl = CtExp Exp
--
-- implementation Show AExp where
--   show (Sum a b) = "Sum (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
--   show (Sub a b) = "Sub (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
--   show (Mul a b) = "Mul (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
--   show (Div a b) = "Div (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
--   show (N a) = "N " ++ show a
--   show (Paren a) = "(" ++ show a ++ ")"
--   -- show (ID a) = "ID " ++ show a
--
-- implementation Show Exp where
--   show (AExpR exp) = "AExpR (" ++ show exp ++ ")"
--
-- implementation Show Ctrl where
--   show (CtExp ct) = "CtExp (" ++ show ct ++ ")"

mutual

  num : List Token -> (AExp, List Token)
  num ((TokenInt n)::l) = (N n, l)

  factor : List Token -> (AExp, List Token)
  factor ((TokenInt n)::xs) = num ((TokenInt n)::xs)
  factor ((TokenLParen)::xs) = let (e,r) = arithExp xs in factorAux e r where
    factorAux : AExp -> List Token -> (AExp, List Token)
    -- factorAux e (TokenRParen :: ys) = ((Paren e),ys)
    factorAux e (TokenRParen :: ys) = (e,ys)
    factorAux e l = (e,l)

  mul : List Token -> (AExp, List Token)
  mul l = let (exp, r) = factor l in mulAux exp r where
    mulAux : AExp -> List Token -> (AExp, List Token)
    mulAux e ((TokenTimes)::xs) = let (exp', r2) = factor xs in mulAux (Mul e exp') r2
    mulAux e ((TokenDiv)::xs) = let (exp', r2) = factor xs in mulAux (Div e exp') r2
    mulAux e l = (e,l)

  sum : List Token -> (AExp, List Token)
  sum l = let (exp, r) = mul l in sumAux exp r where
    sumAux : AExp -> List Token -> (AExp, List Token)
    sumAux e ((TokenPlus)::xs) = let (exp', r2) = mul xs in sumAux (Sum e exp') r2
    sumAux e ((TokenMinus)::xs) = let (exp', r2) = mul xs in sumAux (Sub e exp') r2
    sumAux e l = (e,l)

  arithExp : List Token -> (AExp, List Token)
  arithExp = sum

-- removeParen : AExp -> AExp
-- removeParen (Sum a b) = Sum (removeParen a) (removeParen b)
-- removeParen (Sub a b) = Sub (removeParen a) (removeParen b)
-- removeParen (Mul a b) = Mul (removeParen a) (removeParen b)
-- removeParen (Div a b) = Div (removeParen a) (removeParen b)
-- removeParen (Paren e) = removeParen e
-- removeParen (N n) = N n

parse : (AExp, List Token) -> AExp
parse (exp, l) = exp

transformPi : AExp -> Ctrl
transformPi exp = CtExp (AExpR exp)
