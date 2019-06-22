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

  parenExp : List Token -> (Maybe AExp, List Token)
  parenExp ((TokenLParen)::xs) = let (e,r) = arithExp xs in parenExpAux e r where
    parenExpAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    parenExpAux Nothing r = (Nothing, (TokenLParen::r))
    parenExpAux (Just e) (TokenRParen::xs) = ((Just e),xs)
    parenExpAux (Just e) l = (Nothing,(TokenLParen::xs))
  parenExp l = (Nothing, l)

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

  arithExp : List Token -> (Maybe AExp, List Token)
  arithExp = sum

-- Parser de expressões booleanas
  bool : List Token -> (Maybe BExp, List Token)
  bool (TokenTrue::xs) = (Just (Boo True), xs)
  bool (TokenFalse::xs) = (Just (Boo False), xs)
  bool l = (Nothing, l)

  factorBExp : List Token -> (Maybe BExp, List Token)
  factorBExp = orParser bool parenBExp

  parenBExp : List Token -> (Maybe BExp, List Token)
  parenBExp ((TokenLParen)::xs) = let (e,r) = boolExp xs in parenBExpAux e r where
    parenBExpAux : Maybe BExp -> List Token -> (Maybe BExp, List Token)
    parenBExpAux Nothing r = (Nothing, (TokenLParen::r))
    parenBExpAux (Just e) (TokenRParen::xs) = ((Just e),xs)
    parenBExpAux (Just e) l = (Nothing,(TokenLParen::xs))
  parenBExp l = (Nothing, l)

  conditExp : List Token -> (Maybe BExp, List Token)
  conditExp = orParser aux factorBExp

  aux : List Token -> (Maybe BExp, List Token)
  aux l = let (e,r) = arithExp l in conditExpAux e r where
    conditExpAux : Maybe AExp -> List Token -> (Maybe BExp, List Token)
    conditExpAux Nothing r = (Nothing, r)
    conditExpAux (Just e) ((TokenEqual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenEqual::xs))
      Just k => ((Just (Equal e k)), r2)
    conditExpAux (Just e) ((TokenMaior)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMaior::xs))
      Just k => ((Just (GT e k)), r2)
    conditExpAux (Just e) ((TokenMenor)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMenor::xs))
      Just k => ((Just (LT e k)), r2)
    conditExpAux (Just e) ((TokenMaiorIgual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMaiorIgual::xs))
      Just k => ((Just (GE e k)), r2)
    conditExpAux (Just e) ((TokenMenorIgual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMenorIgual::xs))
      Just k => ((Just (LE e k)), r2)
    conditExpAux (Just e) l' = (Nothing,l)

  logic : List Token -> (Maybe BExp, List Token)
  logic l = let (exp, r) = conditExp l in logicAux exp r where
    logicAux : Maybe BExp -> List Token -> (Maybe BExp, List Token)
    logicAux Nothing r = (Nothing, r)
    logicAux (Just e) ((TokenOr)::xs) = let (exp', r2) = conditExp xs in case exp' of
      Nothing => (Just e, (TokenOr::xs))
      Just k => logicAux (Just (OR e k)) r2
    logicAux (Just e) ((TokenAnd)::xs) = let (exp', r2) = conditExp xs in case exp' of
      Nothing => (Just e, (TokenAnd::xs))
      Just k => logicAux (Just (And e k)) r2
    logicAux (Just e) l = ((Just e),l)

  boolExp : List Token -> (Maybe BExp, List Token)
  boolExp = logic

  parseBExp : List Token -> (Maybe Exp, List Token)
  parseBExp l = let (exp, r) = logic l in case exp of
    Nothing => (Nothing, l)
    Just k => ((Just (BExpR k)), r)

  parseAExp : List Token -> (Maybe Exp, List Token)
  parseAExp l = let (exp, r) = arithExp l in case exp of
    Nothing => (Nothing, l)
    Just k => ((Just (AExpR k)), r)

  exp : List Token -> (Maybe Exp, List Token)
  exp = orParser parseBExp parseAExp


-- funçao para tentar parser de AExp e BExp
-- tenta aplicar o parser1 a lista
  orParser : (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token))
  orParser parser1 parser2 list = let (exp,r) = parser1 list in orParserAux exp r where
-- orParserAux verifica se o parser1 falhou (Nothing) ou retornou com sucesso um (exp, resto da lista)
    orParserAux : Maybe a -> List Token -> (Maybe a, List Token)
    orParserAux Nothing l = parser2 l
    orParserAux (Just exp') l = (Just exp', l)

-- Ajustes para o piAutomata
transformPi : Exp -> Ctrl
transformPi exp = (CtExp exp)