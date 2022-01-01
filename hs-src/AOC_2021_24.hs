{- |
 Module      : AOC_2021_24
 Description : Advent of code 2021 day 24
 Copyright   : (c) Joe Watson 2021
 License     : GPL-3
 Maintainer  : jw1200@gmail.com
 Stability   : experimental
-}
module AOC_2021_24 (solvers, pInstruction, compile) where

import Data.List qualified as L (foldl')
import Data.Text (Text)

--import Data.Text qualified as T (lines, pack, unpack)
import Text.Megaparsec.Char qualified as MC (string)

import Utilities (Parser, pSignedInt, ($>), (<|>)) -- parseOrStop,

solvers :: Text -> (Text, Text)
solvers _t =
    ( "NYI" -- T.pack . show . magnitude $ addList numbers
    , "NYI" -- T.pack . show $ eval packet
    )

data Instruction
    = Input Register
    | Add Register Value
    | Mul Register Value
    | Div Register Value
    | Mod Register Value
    | Eql Register Value

instance Show Instruction where
    show (Input r) = "inp " ++ show r
    show (Add r v) = "add " ++ show r ++ " " ++ show v
    show (Mul r v) = "mul " ++ show r ++ " " ++ show v
    show (Div r v) = "div " ++ show r ++ " " ++ show v
    show (Mod r v) = "mod " ++ show r ++ " " ++ show v
    show (Eql r v) = "eql " ++ show r ++ " " ++ show v

register :: Instruction -> Register
register (Input r) = r
register (Add r _) = r
register (Mul r _) = r
register (Div r _) = r
register (Mod r _) = r
register (Eql r _) = r

value :: Instruction -> Value
value (Input _) = ValueConstant 99
value (Add _ v) = v
value (Mul _ v) = v
value (Div _ v) = v
value (Mod _ v) = v
value (Eql _ v) = v

data Register = W | X | Y | Z deriving (Eq)

instance Show Register where
    show W = "w"
    show X = "x"
    show Y = "y"
    show Z = "z"

data Value = ValueRegister Register | ValueConstant Int

instance Show Value where
    show (ValueRegister r) = show r
    show (ValueConstant x) = show x

pInstruction :: Parser Instruction
pInstruction = pInput <|> pArithmetic
  where
    pRegister :: Parser Register
    pRegister = MC.string "w" $> W <|> MC.string "x" $> X <|> MC.string "y" $> Y <|> MC.string "z" $> Z
    pValue :: Parser Value
    pValue = ValueRegister <$> pRegister <|> ValueConstant <$> pSignedInt
    pInput :: Parser Instruction
    pInput = Input <$> (MC.string "inp " *> pRegister)
    pArithmetic :: Parser Instruction
    pArithmetic =
        (Add <$> (MC.string "add " *> pRegister) <*> (MC.string " " *> pValue))
            <|> (Mul <$> (MC.string "mul " *> pRegister) <*> (MC.string " " *> pValue))
            <|> (Div <$> (MC.string "div " *> pRegister) <*> (MC.string " " *> pValue))
            <|> (Mod <$> (MC.string "mod " *> pRegister) <*> (MC.string " " *> pValue))
            <|> (Eql <$> (MC.string "eql " *> pRegister) <*> (MC.string " " *> pValue))

data Expression
    = EAdd Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression
    | EMod Expression Expression
    | EEql Expression Expression
    | EInput
    | ERegister Register
    | EConstant Int

instance Show Expression where
    show (EAdd p q) = "(" ++ "+ " ++ show p ++ " " ++ show q ++ ")"
    show (EMul p q) = "(" ++ "* " ++ show p ++ " " ++ show q ++ ")"
    show (EDiv p q) = "(" ++ "/ " ++ show p ++ " " ++ show q ++ ")"
    show (EMod p q) = "(" ++ "% " ++ show p ++ " " ++ show q ++ ")"
    show (EEql p q) = "(" ++ "== " ++ show p ++ " " ++ show q ++ ")"
    show EInput = "input"
    show (ERegister r) = show r
    show (EConstant x) = show x

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
