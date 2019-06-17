module parser

import lexer
import tipos

%access public export

mutual

-- Parser de expressões aritmeticas
  num : List Token -> (Maybe AExp, List Token)
  num ((TokenInt n)::l) = (Just (N n), l)
  num l = (Nothing, l)

  factor : List Token -> (Maybe AExp, List Token)
  factor = orParser num parenExp
  -- factor ((TokenInt n)::xs) = num ((TokenInt n)::xs)
  -- factor l = (Nothing, l)

  parenExp : List Token -> (Maybe AExp, List Token)
  parenExp ((TokenLParen)::xs) = let (e,r) = arithExp xs in parenExpAux e r where
    parenExpAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    parenExpAux Nothing r = (Nothing, (TokenLParen::r))
    parenExpAux (Just e) (TokenRParen::xs) = ((Just e),xs)
    parenExpAux (Just e) l = (Nothing,(TokenLParen::xs))
  parenExp l = (Nothing, l)
  --
  mul : List Token -> (Maybe AExp, List Token)
  mul l = let (exp, r) = factor l in mulAux exp r where
    mulAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    mulAux Nothing r = (Nothing, r)
    mulAux (Just e) ((TokenTimes)::xs) = let (exp', r2) = factor xs in case exp' of
      Nothing => (Just e, (TokenTimes::xs))
      Just k => mulAux (Just (Mul e k)) r2
    mulAux (Just e) ((TokenDiv)::xs) = let (exp', r2) = factor xs in case exp' of
      Nothing => (Just e, (TokenDiv::xs))
      Just k => mulAux (Just (Div e k)) r2
    mulAux (Just e) l = ((Just e),l)
  --
  sum : List Token -> (Maybe AExp, List Token)
  sum l = let (exp, r) = mul l in sumAux exp r where
    sumAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    sumAux Nothing r = (Nothing, r)
    sumAux (Just e) ((TokenPlus)::xs) = let (exp', r2) = mul xs in case exp' of
      Nothing => (Just e, (TokenPlus::xs))
      Just k => sumAux (Just (Sum e k)) r2
    sumAux (Just e) ((TokenMinus)::xs) = let (exp', r2) = mul xs in case exp' of
      Nothing => (Just e, (TokenMinus::xs))
      Just k => sumAux (Just (Sub e k)) r2
    sumAux (Just e) l = ((Just e),l)
  --
  arithExp : List Token -> (Maybe AExp, List Token)
  arithExp = sum

-- Parser de expressões booleanas
  -- bool : List Token -> (BExp, List Token)
  -- bool (TokenTrue::xs) = (Boo True, xs)
  -- bool (TokenFalse::xs) = (Boo False, xs)
  --
  -- parenBExp : List Token -> (BExp, List Token)
  -- parenBExp (TokenTrue::xs) = bool (TokenTrue::xs)
  -- parenBExp (TokenFalse::xs) = bool (TokenFalse::xs)
  -- parenBExp l = let (e,r) = arithExp l in factorBExpAux e r where
  --   factorBExpAux : AExp -> List Token -> (BExp, List Token)
  --   factorBExpAux e (TokenEq::xs) = let (exp', r2) = arithExp xs in ((Eq e exp'), r2)
  --   factorBExpAux e (TokenMaior::xs) = let (exp', r2) = arithExp xs in ((GT e exp'), r2)
  --   factorBExpAux e (TokenMenor::xs) = let (exp', r2) = arithExp xs in ((LT e exp'), r2)
  --   factorBExpAux e (TokenMaiorIgual::xs) = let (exp', r2) = arithExp xs in ((GE e exp'), r2)
  --   factorBExpAux e (TokenMenorIgual::xs) = let (exp', r2) = arithExp xs in ((LE e exp'), r2)
  --
  -- parenBExp ((TokenLParen)::xs) = let (e,r) = boolExp xs in parenBExpAux e r where
  --   parenBExpAux : BExp -> List Token -> (BExp, List Token)
  --   parenBExpAux e (TokenRParen :: xs) = (e,xs)
  --   parenBExpAux e l = (e,l)
  --
  -- logic : List Token -> (BExp, List Token)
  -- logic l = let (exp, r) = parenBExp l in logicAux exp r where
  --   logicAux : BExp -> List Token -> (BExp, List Token)
  --   logicAux e (TokenOr::xs) = let (exp', r2) = parenBExp xs in logicAux (OR e exp') r2
  --   logicAux e (TokenAnd::xs) = let (exp', r2) = parenBExp xs in logicAux (And e exp') r2
  --   -- logicAux e (TokenNot::xs) = logicAux (Not e) xs
  --   logicAux e l = (e,l)
  --
  -- boolExp : List Token -> (BExp, List Token)
  -- boolExp = logic

-- funçao para tentar parser de AExp e BExp
-- tenta aplicar o parser1 a lista
  orParser : (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token))
  orParser parser1 parser2 list = let (exp,r) = parser1 list in orParserAux exp r where
-- orParserAux verifica se o parser1 falhou (Nothing) ou retornou com sucesso um (exp, resto da lista)
    orParserAux : Maybe a -> List Token -> (Maybe a, List Token)
    orParserAux Nothing l = parser2 l
    orParserAux (Just exp') l = (Just exp', l)

-- Ajustes para o piAutomata
parse : (AExp, List Token) -> AExp
parse (exp, l) = exp

-- parseBExp : (BExp, List Token) -> BExp
-- parseBExp (exp, l) = exp

transformPi : AExp -> Ctrl
transformPi exp = CtExp (AExpR exp)
