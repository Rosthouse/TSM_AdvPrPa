-- MicroIML V01
-- Edgar F.A. Lederer, FHNW and Uni Basel, 2015

module Parser where

import Scanner
import ParserCombis2
import AbsSyn

type ParserT = Parser Token

tokenP :: Terminal -> ParserT Token
tokenP term = P (\inp ->
  case parse itemP inp of
    [] -> []
    [((term', attrib), out)] ->
      if term' == term then
        [((term', attrib), out)]
      else
        [])

tP :: Terminal -> ParserT ()
tP term = do tokenP term; return ()

aLitP :: ParserT Int
aLitP = do (ALITERAL, Just (ALitAttrib val)) <- tokenP ALITERAL; return val

bLitP :: ParserT Bool
bLitP = do (BLITERAL, Just (BLitAttrib val)) <- tokenP BLITERAL; return val

identP :: ParserT Ident
identP = do (IDENT, Just (IdentAttrib ident)) <- tokenP IDENT; return ident

arithOprP :: ParserT ArithOperator
arithOprP = do (ARITHOPR, Just (AOprAttrib opr)) <- tokenP ARITHOPR; return opr

relOprP :: ParserT RelOperator
relOprP = do (RELOPR, Just (ROprAttrib opr)) <- tokenP RELOPR; return opr

boolOprP :: ParserT BoolOperator
boolOprP = do (BOOLOPR, Just (BOprAttrib opr)) <- tokenP BOOLOPR; return opr

arithExprP :: ParserT ArithExpr
arithExprP =
      litAExprP
  +++ idAExprP
  +++ dyaAExprP

litAExprP = do val <- aLitP; return (LitAExpr val)

idAExprP = do ident <- identP; return (IdAExpr ident)

dyaAExprP :: ParserT ArithExpr
dyaAExprP = dyaAExprP_V04

-- classic version with do notation
dyaAExprP_V01 :: ParserT ArithExpr
dyaAExprP_V01 =
  do tP LPAREN
     aExpr1 <- arithExprP
     aOpr   <- arithOprP
     aExpr2 <- arithExprP
     tP RPAREN
     return (DyaAExpr aOpr aExpr1 aExpr2)

-- desugared version with many parentheses
dyaAExprP_V02 :: ParserT ArithExpr
dyaAExprP_V02 =
  (tP LPAREN  >>= (\_      ->
  (arithExprP >>= (\aExpr1 ->
  (arithOprP  >>= (\aOpr   ->
  (arithExprP >>= (\aExpr2 ->
  (tP RPAREN  >>= (\_      ->
  return (DyaAExpr aOpr aExpr1 aExpr2)))))))))))

-- desugared version with little parentheses
dyaAExprP_V03 :: ParserT ArithExpr
dyaAExprP_V03 =
  tP LPAREN  >>= \_      ->
  arithExprP >>= \aExpr1 ->
  arithOprP  >>= \aOpr   ->
  arithExprP >>= \aExpr2 ->
  tP RPAREN  >>= \_      ->
  return (DyaAExpr aOpr aExpr1 aExpr2)

-- version with do notation, but arbitrarily splitted in two parts
dyaAExprP1_V04 :: ParserT (ArithOperator, ArithExpr)
dyaAExprP1_V04 =
  do (tP LPAREN)
     aExpr1 <- arithExprP
     aOpr   <- arithOprP
     return (aOpr, aExpr1)
funDyaAExprP2_V04 :: (ArithOperator, ArithExpr) -> ParserT ArithExpr
funDyaAExprP2_V04 (aOpr, aExpr1) =
  do aExpr2 <- arithExprP
     (tP RPAREN)
     return (DyaAExpr aOpr aExpr1 aExpr2)
dyaAExprP_V04 :: ParserT ArithExpr
dyaAExprP_V04 =
  dyaAExprP1_V04 >>= funDyaAExprP2_V04

boolExprP :: ParserT BoolExpr
boolExprP =
      litBExprP
  +++ relBExprP
  +++ negBExprP
  +++ dyaBExprP

litBExprP = do val <- bLitP; return (LitBExpr val)

relBExprP =
  do (tP LPAREN)
     aExpr1 <- arithExprP
     rOpr   <- relOprP
     aExpr2 <- arithExprP
     (tP RPAREN)
     return (RelBExpr rOpr aExpr1 aExpr2)

negBExprP =
  do (tP NOT)
     bExpr <- boolExprP
     return (NegBExpr bExpr)

dyaBExprP =
  do (tP LPAREN)
     bExpr1 <- boolExprP
     bOpr   <- boolOprP
     bExpr2 <- boolExprP
     (tP RPAREN)
     return (DyaBExpr bOpr bExpr1 bExpr2)

commandP :: ParserT Command
commandP =
      skipCmdP
  +++ assiCmdP
  +++ condCmdP
  +++ whileCmdP

skipCmdP = do (tP SKIP); return SkipCmd

assiCmdP =
  do ident <- identP
     (tP BECOMES)
     aExpr <- arithExprP
     return (AssiCmd ident aExpr)

condCmdP =
  do (tP IF)
     bExpr <- boolExprP
     (tP THEN)
     cmd1  <- cpsCmdP
     (tP ELSE)
     cmd2  <- cpsCmdP
     (tP ENDIF)
     return (CondCmd bExpr cmd1 cmd2)

whileCmdP =
  do (tP WHILE)
     bExpr <- boolExprP
     (tP DO)
     cmd   <- cpsCmdP
     (tP ENDWHILE)
     return (WhileCmd bExpr cmd)

cpsCmdP :: ParserT Command
cpsCmdP = sepList1C commandP (tP SEMICOLON) CpsCmd

programP :: ParserT Command
programP =
  do cmd <- cpsCmdP
     (tP SENTINEL)
     return cmd

parser :: [Token] -> Command
parser toks =
  case parse programP toks of
    [(cmd, [])] -> cmd
    [_]         -> error "internal error"
    []          -> error "syntax error"
