module Main where

import AbsSyn
import Scanner
import Parser2
import Interpreter

inter :: String -> State -> State
inter = interpreter . parser . scanner

main :: IO ()
main = 
  do
    let
      dirName =
        "/Users/EdgarLederer/Documents/IML/EclipseWS-IML/IML-HS-2010-Interpreter/ExamplePrograms/"
      fileName =
        "intDiv.iml"
    prog <- readFile (dirName ++ fileName)
    putStr "program text:\n"
    putStr prog
    putStr "\ntoken list:\n"
    putStr ((show . scanner) prog)
    putStr "\n\nabstract syntax tree:\n"
    putStr ((show . parser . scanner) prog)
    putStr "\n\nexecute program:\n"
    putStr "m = "
    mString <- getLine
    putStr "n = "
    nString <- getLine
    let
      outputState = inter prog inputState
        where
          m = read mString :: Int
          n = read nString :: Int
          inputState = updateS (updateS iniState ("m", m)) ("n", n)
      q = readS outputState "q"
      r = readS outputState "r"
    putStrLn ("q = " ++ show q)
    putStrLn ("r = " ++ show r)

{- integer division m by n -}
{-
{ m >= 0 & n > 0 }
q := 0;
r := m;
while r >= n do
  q := q + 1;
  r := r - n
endwhile
{ m = q * n + r & 0 <= r & r < n }
-}

divProgConc = "\
\q := 0;\n\
\r := m;\n\
\while (r >= n) do\n\
\  q := (q + 1);\n\
\  r := (r - n)\n\
\endwhile\
\"

divProgAbs =
  CpsCmd [
    AssiCmd "q" (LitAExpr 0),
    AssiCmd "r" (IdAExpr "m"),
    WhileCmd (RelBExpr GreaterEq (IdAExpr "r") (IdAExpr "n"))
      (CpsCmd [
        AssiCmd "q" (DyaAExpr Plus (IdAExpr "q") (LitAExpr 1)),
        AssiCmd "r" (DyaAExpr Minus (IdAExpr "r") (IdAExpr "n"))
      ])
  ]

runDivConc :: Int -> Int -> (Int, Int)
runDivConc m n = (readS outputState "q", readS outputState "r")
  where
    outputState = interCmd (parser (scanner divProgConc)) inputState
    inputState = updateS (updateS iniState ("m", m)) ("n", n)

runDivAbs :: Int -> Int -> (Int, Int)
runDivAbs m n = (readS outputState "q", readS outputState "r")
  where
    outputState = interCmd divProgAbs inputState
    inputState = updateS (updateS iniState ("m", m)) ("n", n)

{- Gauss -}
{-
sum := 0;
for i from 1 to n do
  sum := sum + i
endfor
-}

gaussProgAbs =
  CpsCmd [
    AssiCmd "sum" (LitAExpr 0),
    ForCmd "i" (LitAExpr 1) (IdAExpr "n")
      (AssiCmd "sum" (DyaAExpr Plus (IdAExpr "sum") (IdAExpr "i")))
  ]

runGaussAbs :: Int -> Int
runGaussAbs n = readS outputState "sum"
  where
    outputState = interCmd gaussProgAbs inputState
    inputState = updateS iniState ("n", n)

{-
x, y := y, x
-}

multiProgAbs =
  MultiAssiCmd ["x", "y"] [IdAExpr "y", IdAExpr "x"]

runMultiAbs :: Int -> Int -> (Int, Int)
runMultiAbs x y = (readS outputState "x", readS outputState "y")
  where
    outputState = interCmd multiProgAbs inputState
    inputState = updateS (updateS iniState ("x", x)) ("y", y)

exaScan = "skip;skip;x:=((((x+y)*z)<x)cand(x<5))dimdiwhile while+*div*divmod"

--state1 :: State
--state1 "x" = 9
--state1 "q" = 3
--state1 "r" = -5
--
--state2 :: State
--state2 = updateS state1 ("q", 17)
