{- |
 Module      : AOC_2021_24
 Description : Advent of code 2021 day 24
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_24 (solvers, parseProgram, unparseProgram, run) where

import Data.List qualified as L (foldl')
import Data.Text (Text)

import Data.Text qualified as T (length, lines, pack, replicate)
import Text.Megaparsec.Char qualified as MC (string)

-- parseOrStop,
import Utilities (Parser, pSignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers t =
    ( trace program -- T.pack . show . get Z $ run program
    , "" -- trace program
    )
  where
    program = take 70 . parseProgram inputs $ T.lines t
    inputs = map Input [0 ..]

{-
Assume i0 = 9
(((i0+2)%26)+15)/=i1)  => i0 + 17 == i1 so i1==?

-}

type Instruction = (Reg, Expression)

data Expression
    = Add Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Mod Expression Expression
    | Eql Expression Expression
    | Input Int
    | Register Reg
    | Constant Int

data Reg = W | X | Y | Z deriving (Eq)

data State = State !Expression !Expression !Expression !Expression

-- Run a series of states starting from all registers at zero with given inputs
run :: [Instruction] -> State
run = L.foldl' (flip interpret) initialState
  where
    initialState = State (Constant 0) (Constant 0) (Constant 0) (Constant 0)

trace :: [Instruction] -> Text
trace = snd . L.foldl' f (initialState, "")
  where
    initialState = State (Constant 0) (Constant 0) (Constant 0) (Constant 0)
    f :: (State, Text) -> Instruction -> (State, Text)
    f (s, t) instruction@(Z, _) = (s', t')
      where
        s' = interpret instruction s
        t' = t <> pad (T.pack (show instruction)) <> " " <> T.pack (show (get Z s')) <> "\n"
        pad x = x <> T.replicate (20 - T.length x) " "
    f (s, t) instruction = (interpret instruction s, t)

-- Apply the given instruction to the ALU
-- Instructions replace the given register with the instruction expression
interpret :: Instruction -> State -> State
interpret (r, e) s = put r (simplify (subRegs s e)) s

-- Replace any references to registers in an expression with the register values in the state
subRegs :: State -> Expression -> Expression
subRegs s (Add p q) = Add (subRegs s p) (subRegs s q)
subRegs s (Mul p q) = Mul (subRegs s p) (subRegs s q)
subRegs s (Div p q) = Div (subRegs s p) (subRegs s q)
subRegs s (Mod p q) = Mod (subRegs s p) (subRegs s q)
subRegs s (Eql p q) = Eql (subRegs s p) (subRegs s q)
subRegs _ (Input n) = Input n
subRegs s (Register r) = get r s
subRegs _ (Constant x) = Constant x

simplify :: Expression -> Expression
simplify (Add (Constant x) (Constant y)) = Constant $ x + y
simplify (Add e (Constant 0)) = simplify e
simplify (Add (Constant 0) e) = simplify e
simplify (Add (Constant x) (Add e (Constant y))) = Add e (Constant $ x + y)
simplify (Add p q) = Add (simplify p) (simplify q)
simplify (Mul (Constant x) (Constant y)) = Constant $ x * y
simplify (Mul e (Constant 1)) = simplify e
simplify (Mul (Constant 1) e) = simplify e
simplify (Mul _ (Constant 0)) = Constant 0
simplify (Mul (Constant 0) _) = Constant 0
simplify (Mul p q) = Mul (simplify p) (simplify q)
simplify (Div (Constant x) (Constant y)) = Constant $ x `div` y
simplify (Div e (Constant 1)) = simplify e
simplify (Div p q) = Div (simplify p) (simplify q)
simplify (Mod (Constant x) (Constant y)) = Constant $ x `mod` y
simplify (Mod (Add p (Constant x)) (Constant y)) = Mod (Add p (Constant $ x `mod` y)) (Constant y)
simplify (Mod p q) = Mod (simplify p) (simplify q)
simplify (Eql (Constant x) (Input n))
    | x > 9 || x < 0 = Constant 0
    | otherwise = Eql (Constant x) (Input n)
simplify (Eql (Constant x) (Constant y)) = Constant $ if x == y then 1 else 0
simplify (Eql p q) = Eql (simplify p) (simplify q)
simplify (Input n) = Input n
simplify (Register r) = Register r
simplify (Constant x) = Constant x

-- -- Replace the register with the newQ sub-expression everywhere it appears
-- substitute :: Reg -> Expression -> Expression -> Expression
-- substitute r subExpr (Add p q) = Add (substitute r subExpr p) (substitute r subExpr q)
-- substitute r subExpr (Mul p q) = Mul (substitute r subExpr p) (substitute r subExpr q)
-- substitute r subExpr (Div p q) = Div (substitute r subExpr p) (substitute r subExpr q)
-- substitute r subExpr (Mod p q) = Mod (substitute r subExpr p) (substitute r subExpr q)
-- substitute r subExpr (Eql p q) = Eql (substitute r subExpr p) (substitute r subExpr q)
-- substitute _ _ Input = Input
-- substitute r subExpr (Register s)
--     | r == s = subExpr
--     | otherwise = Register s
-- substitute _ _ (Constant x) = Constant x

instance Show Expression where
    show (Add p q) = "(" ++ show p ++ "+" ++ show q ++ ")"
    show (Mul p q) = "(" ++ show p ++ "*" ++ show q ++ ")"
    show (Div p q) = "(" ++ show p ++ "/" ++ show q ++ ")"
    show (Mod p q) = "(" ++ show p ++ "%" ++ show q ++ ")"
    show (Eql (Eql p q) (Constant 0)) = "(" ++ show p ++ "/=" ++ show q ++ ")"
    show (Eql p q) = "(" ++ show p ++ "==" ++ show q ++ ")"
    show (Input n) = "i" ++ show n
    show (Register r) = show r
    show (Constant x) = show x

instance Show Reg where
    show W = "w"
    show X = "x"
    show Y = "y"
    show Z = "z"

instance Show State where
    show (State w x y z) = show (w, x, y, z)

get :: Reg -> State -> Expression
get W (State w _ _ _) = w
get X (State _ x _ _) = x
get Y (State _ _ y _) = y
get Z (State _ _ _ z) = z

put :: Reg -> Expression -> State -> State
put W e (State _ x y z) = State e x y z
put X e (State w _ y z) = State w e y z
put Y e (State w x _ z) = State w x e z
put Z e (State w x y _) = State w x y e

parseProgram :: [Expression] -> [Text] -> [Instruction]
parseProgram inputs = fst . L.foldl' addIns ([], inputs)
  where
    addIns :: ([Instruction], [Expression]) -> Text -> ([Instruction], [Expression])
    addIns (xs, es) t =
        let (x, es') = parse es t
         in (xs ++ [x], es')

-- Parse an instruction given the index for the next input
parse :: [Expression] -> Text -> (Instruction, [Expression])
parse inputs = parseOrStop (pInstruction inputs)

pInstruction :: [Expression] -> Parser (Instruction, [Expression])
pInstruction inputs = pInput <|> ((,inputs) <$> pArithmetic)
  where
    pReg :: Parser Reg
    pReg = MC.string "w" $> W <|> MC.string "x" $> X <|> MC.string "y" $> Y <|> MC.string "z" $> Z
    pValue :: Parser Expression
    pValue = Register <$> pReg <|> Constant <$> pSignedInt
    pInput :: Parser (Instruction, [Expression])
    pInput = (\r -> ((r, head inputs), tail inputs)) <$> (MC.string "inp " *> pReg)
    pArithmetic :: Parser Instruction
    pArithmetic =
        ((\r v -> (r, Add (Register r) v)) <$> (MC.string "add " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Div (Register r) v)) <$> (MC.string "div " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Mod (Register r) v)) <$> (MC.string "mod " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Eql (Register r) v)) <$> (MC.string "eql " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Mul (Register r) v)) <$> (MC.string "mul " *> pReg) <*> (MC.string " " *> pValue))

-- for testing
unparseProgram :: [Instruction] -> [Text]
unparseProgram = map (T.pack . unparse)

unparse :: Instruction -> String
unparse (r, Add (Register p) q) = if r == p then "add " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Mul (Register p) q) = if r == p then "mul " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Div (Register p) q) = if r == p then "div " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Mod (Register p) q) = if r == p then "mod " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Eql (Register p) q) = if r == p then "eql " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Input _) = "inp " ++ show r
unparse _ = "Unexpected instruction"

-- instance Show Expression where
--     show (EAdd p q) = "(" ++ "+ " ++ show p ++ " " ++ show q ++ ")"
--     show (EMul p q) = "(" ++ "* " ++ show p ++ " " ++ show q ++ ")"
--     show (EDiv p q) = "(" ++ "/ " ++ show p ++ " " ++ show q ++ ")"
--     show (EMod p q) = "(" ++ "% " ++ show p ++ " " ++ show q ++ ")"
--     show (EEql p q) = "(" ++ "== " ++ show p ++ " " ++ show q ++ ")"
--     show EInput = "input"
--     show (ERegister r) = show r
--     show (EConstant x) = show x

{-

compile :: [Instruction] -> Expression
compile = L.foldl' addInstruction (ERegister Z) . reverse

-- compileValue :: Register -> Value -> Expression
-- compileValue r (ValueRegister s)

addInstruction :: Expression -> Instruction -> Expression
addInstruction expr instruction = replaceRegister (valueToExpr (value instruction)) expr
  where
    replaceRegister :: Expression -> Expression -> Expression
    replaceRegister replacement expression@(ERegister s)
        | s == register instruction = replacement
        | otherwise = expression
    replaceRegister _ expression = expression

-- NEED TO RECURSE - Functor!

compileInstruction :: Instruction -> (Register, Expression)
compileInstruction (Input r) = (r, EConstant 98)
compileInstruction (Add r v) = (r, EAdd (ERegister r) (valueToExpr v))
compileInstruction (Mul r v) = (r, EMul (ERegister r) (valueToExpr v))
compileInstruction (Div r v) = (r, EDiv (ERegister r) (valueToExpr v))
compileInstruction (Mod r v) = (r, EMod (ERegister r) (valueToExpr v))
compileInstruction (Eql r v) = (r, EEql (ERegister r) (valueToExpr v))

valueToExpr :: Value -> Expression
valueToExpr (ValueRegister r) = ERegister r
valueToExpr (ValueConstant x) = EConstant x

-}