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

  bool : List Token -> (BExp, List Token)
  bool (TokenTrue::xs) = (Boo True, xs)
  bool (TokenFalse::xs) = (Boo False, xs)

  parenBExp : List Token -> (BExp, List Token)
  parenBExp (TokenTrue::xs) = bool (TokenTrue::xs)
  parenBExp (TokenFalse::xs) = bool (TokenFalse::xs)
  parenBExp l = let (e,r) = arithExp l | boolExp l in factorBExpAux e r where
    factorBExpAux : AExp -> List Token -> (BExp, List Token)
    factorBExpAux e (TokenEq::xs) = let (exp', r2) = arithExp xs in ((Eq e exp'), r2)
    factorBExpAux e (TokenMaior::xs) = let (exp', r2) = arithExp xs in ((GT e exp'), r2)
    factorBExpAux e (TokenMenor::xs) = let (exp', r2) = arithExp xs in ((LT e exp'), r2)
    factorBExpAux e (TokenMaiorIgual::xs) = let (exp', r2) = arithExp xs in ((GE e exp'), r2)
    factorBExpAux e (TokenMenorIgual::xs) = let (exp', r2) = arithExp xs in ((LE e exp'), r2)

  parenBExp ((TokenLParen)::xs) = let (e,r) = boolExp xs in parenBExpAux e r where
    parenBExpAux : BExp -> List Token -> (BExp, List Token)
    parenBExpAux e (TokenRParen :: ys) = (e,ys)
    parenBExpAux e l = (e,l)

  logic : List Token -> (BExp, List Token)
  logic l = let (exp, r) = parenBExp l in logicAux exp r where
    logicAux : BExp -> List Token -> (BExp, List Token)
    logicAux e (TokenOr::xs) = let (exp', r2) = parenBExp xs in logicAux (OR e exp') r2
    logicAux e (TokenAnd::xs) = let (exp', r2) = parenBExp xs in logicAux (And e exp') r2
    -- logicAux e (TokenNot::xs) = logicAux (Not e) xs
    logicAux e l = (e,l)

  boolExp : List Token -> (BExp, List Token)
  boolExp = logic


parse : (AExp, List Token) -> AExp
parse (exp, l) = exp

transformPi : AExp -> Ctrl
transformPi exp = CtExp (AExpR exp)
