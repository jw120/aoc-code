{- |
 Module      : AOC_2021_24
 Description : Advent of code 2021 day 24
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_24 (solvers, parse, unparse, run) where

import Data.List qualified as L (foldl')
import Data.Text (Text)

--import Data.Text qualified as T (lines, pack, unpack)
import Text.Megaparsec.Char qualified as MC (string)

-- parseOrStop,
import AOC_2018_19 (initialState)
import Utilities (Parser, pSignedInt, parseOrStop, ($>), (<|>))

solvers :: Text -> (Text, Text)
solvers _t =
    ( "NYI" -- T.pack . show . magnitude $ addList numbers
    , "NYI" -- T.pack . show $ eval packet
    )

type Instruction = (Reg, Expression)

data Expression
    = Add Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Mod Expression Expression
    | Eql Expression Expression
    | Input
    | Register Reg
    | Constant Int

data Reg = W | X | Y | Z deriving (Eq)

data State = State Expression Expression Expression Expression

-- Run a series of states starting from all registers at zero
run :: [Instruction] -> State
run = L.foldl' (flip interpret) initialState
  where
    initialState = State (Constant 0) (Constant 0) (Constant 0) (Constant 0)

interpret :: Instruction -> State -> State
interpret (r, e) = fmap (substitute r e)

-- Replace the register with the new sub-expression everywhere it appears
substitute :: Reg -> Expression -> Expression -> Expression
substitute r subExpr (Add p q) = Add (substitute r subExpr p) (substitute r subExpr q)
substitute r subExpr (Mul p q) = Mul (substitute r subExpr p) (substitute r subExpr q)
substitute r subExpr (Div p q) = Div (substitute r subExpr p) (substitute r subExpr q)
substitute r subExpr (Mod p q) = Mod (substitute r subExpr p) (substitute r subExpr q)
substitute r subExpr (Eql p q) = Eql (substitute r subExpr p) (substitute r subExpr q)
substitute _ _ Input = Input
substitute r subExpr (Register s)
    | r == s = subExpr
    | otherwise = Register s
substitute _ _ (Constant x) = Constant x

instance Show Expression where
    show (Add p q) = show p ++ "+" ++ show q
    show (Mul p q) = show p ++ "*" ++ show q
    show (Div p q) = show p ++ "/" ++ show q
    show (Mod p q) = show p ++ "%" ++ show q
    show (Eql p q) = show p ++ "==" ++ show q
    show Input = "input"
    show (Register r) = show r
    show (Constant x) = show x

instance Show Reg where
    show W = "w"
    show X = "x"
    show Y = "y"
    show Z = "z"

get :: Reg -> State -> Expression
get W (State w _ _ _) = w
get X (State _ x _ _) = x
get Y (State _ _ y _) = y
get Z (State _ _ _ z) = z

put :: Reg -> Expression -> State -> State
put W e (State w x y z) = State e x y z
put X e (State w x y z) = State w e y z
put Y e (State w x y z) = State w x e z
put Z e (State w x y z) = State w x y e

parse :: Text -> Instruction
parse = parseOrStop pInstruction

pInstruction :: Parser Instruction
pInstruction = pInput <|> pArithmetic
  where
    pReg :: Parser Reg
    pReg = MC.string "w" $> W <|> MC.string "x" $> X <|> MC.string "y" $> Y <|> MC.string "z" $> Z
    pValue :: Parser Expression
    pValue = Register <$> pReg <|> Constant <$> pSignedInt
    pInput :: Parser Instruction
    pInput = (,Input) <$> (MC.string "inp " *> pReg)
    pArithmetic :: Parser Instruction
    pArithmetic =
        ((\r v -> (r, Add (Register r) v)) <$> (MC.string "add " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Div (Register r) v)) <$> (MC.string "div " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Mod (Register r) v)) <$> (MC.string "mod " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Eql (Register r) v)) <$> (MC.string "eql " *> pReg) <*> (MC.string " " *> pValue))
            <|> ((\r v -> (r, Mul (Register r) v)) <$> (MC.string "mul " *> pReg) <*> (MC.string " " *> pValue))

-- for testing
unparse :: Instruction -> String
unparse (r, Add (Register p) q) = if r == p then "add " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Mul (Register p) q) = if r == p then "mul " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Div (Register p) q) = if r == p then "div " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Mod (Register p) q) = if r == p then "mod " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Eql (Register p) q) = if r == p then "eql " ++ show p ++ " " ++ show q else "Failed"
unparse (r, Input) = "inp " ++ show r
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