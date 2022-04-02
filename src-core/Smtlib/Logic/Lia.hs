{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language EmptyDataDeriving #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module Smtlib.Logic.Lia
  ( Constant(..)
  , Sort(..)
  , Index
  , Function(..)
    -- Decoding
  , decodeNumeral
  , decodeString
  , decodeIndex
  , decodeFunction
  , decodeSort
    -- Function
  , functionAnnotation
  , and
  , true
  , false
    -- Sorts
  , isBool
  ) where

import Prelude hiding (Bool(..),and)

import Data.Kind (Type)
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray)
import Data.Type.Equality ((:~:)(Refl),TestEquality,testEquality)
import Arithmetic.Types (WithNat(WithNat))

import qualified Data.Primitive as PM
import qualified GHC.TypeNats as GHC
import qualified Smtlib.Syntax as Syntax
import qualified Arithmetic.Nat as Nat
import qualified Smtlib.Function.Annotation as Function
import qualified Prelude

-- | Sort is restricted to bool and int in this logic.
data Sort = Bool | Int
  deriving (Eq,Show)

-- | Indices are not allowed in LIA.
data Index
  deriving (Eq,Show)

data Constant
  = Integer !Integer
  | True
  | False
  deriving (Eq,Show)

-- | Described by theory of linear arithmetic:
--
-- > :funs ((NUMERAL Int)
-- >        (- Int Int)                 ; negation
-- >        (- Int Int Int :left-assoc) ; subtraction
-- >        (+ Int Int Int :left-assoc)
-- >        (* Int Int Int :left-assoc)
-- >        (div Int Int Int :left-assoc)
-- >        (mod Int Int Int)
-- >        (abs Int Int)
-- >        (<= Int Int Bool :chainable)
-- >        (<  Int Int Bool :chainable)
-- >        (>= Int Int Bool :chainable)
-- >        (>  Int Int Bool :chainable)
-- >       )
--
-- And by core theory:
-- > :funs ((true Bool)
-- >        (false Bool)
-- >        (not Bool Bool)
-- >        (=> Bool Bool Bool :right-assoc)
-- >        (and Bool Bool Bool :left-assoc)
-- >        (or Bool Bool Bool :left-assoc)
-- >        (xor Bool Bool Bool :left-assoc)
-- >        (par (A) (= A A Bool :chainable))
-- >        (par (A) (distinct A A Bool :pairwise))
-- >        (par (A) (ite Bool A A A))
-- >       )
data Function :: GHC.Nat -> Type where
  Negate :: Function 1
  Abs :: Function 1
  Mod :: Function 2
  Add :: Function 2
  Multiply :: Function 2
  Subtract :: Function 2
  Div :: Function 2
  Le :: Function 2
  Lt :: Function 2
  Ge :: Function 2
  Gt :: Function 2
  Eq :: Function 2
  Not :: Function 1
  Implies :: Function 2
  And :: Function 2
  Or :: Function 2
  Xor :: Function 2
  Distinct :: Function 2
  IfThenElse :: Function 3

deriving instance Eq (Function n)
deriving instance Show (Function n)

instance TestEquality Function where
  testEquality Negate Negate = Just Refl
  testEquality Abs Abs = Just Refl
  testEquality Mod Mod = Just Refl
  testEquality Add Add = Just Refl
  testEquality Subtract Subtract = Just Refl
  testEquality Div Div = Just Refl
  testEquality Le Le = Just Refl
  testEquality Lt Lt = Just Refl
  testEquality Ge Ge = Just Refl
  testEquality Gt Gt = Just Refl
  testEquality Eq Eq = Just Refl
  testEquality Not Not = Just Refl
  testEquality Implies Implies = Just Refl
  testEquality And And = Just Refl
  testEquality Or Or = Just Refl
  testEquality Xor Xor = Just Refl
  testEquality Distinct Distinct = Just Refl
  testEquality IfThenElse IfThenElse = Just Refl
  testEquality _ _ = Nothing

decodeNumeral :: Integer -> Maybe Constant
decodeNumeral = Just . Integer

decodeString :: ShortText -> Maybe Constant
decodeString _ = Nothing

decodeIndex :: Syntax.Index -> Maybe Index
decodeIndex _ = Nothing

decodeFunction ::
     SmallArray Syntax.Index
  -> Maybe Syntax.Sort
  -> Int
  -> ShortText
  -> Maybe (WithNat Function)
decodeFunction !indices msort !arity !t
  | PM.sizeofSmallArray indices == 0
  , Nothing <- msort = case t of
      "+" -> Just (WithNat Nat.constant Add)
      "*" -> Just (WithNat Nat.constant Multiply)
      "-" -> case arity of
        0 -> Nothing
        1 -> Just (WithNat Nat.constant Negate)
        _ -> Just (WithNat Nat.constant Subtract)
      "abs" -> Just (WithNat Nat.constant Abs)
      "mod" -> Just (WithNat Nat.constant Mod)
      "div" -> Just (WithNat Nat.constant Div)
      "<=" -> Just (WithNat Nat.constant Le)
      "<" -> Just (WithNat Nat.constant Lt)
      ">=" -> Just (WithNat Nat.constant Ge)
      ">" -> Just (WithNat Nat.constant Gt)
      "=" -> Just (WithNat Nat.constant Eq)
      "not" -> Just (WithNat Nat.constant Not)
      "implies" -> Just (WithNat Nat.constant Implies)
      "and" -> Just (WithNat Nat.constant And)
      "or" -> Just (WithNat Nat.constant Or)
      "xor" -> Just (WithNat Nat.constant Xor)
      "distinct" -> Just (WithNat Nat.constant Distinct)
      "ite" -> Just (WithNat Nat.constant IfThenElse)
      _ -> Nothing
  | otherwise = Nothing

decodeSort :: Syntax.Sort -> Maybe Sort
decodeSort _ = Nothing

functionAnnotation :: Function n -> Function.Annotation n
functionAnnotation = \case
  Abs -> Function.None
  Mod -> Function.None
  Not -> Function.None
  Negate -> Function.None
  IfThenElse -> Function.None
  Add -> Function.LeftAssociative
  Multiply -> Function.LeftAssociative
  Subtract -> Function.LeftAssociative
  Div -> Function.LeftAssociative
  Le -> Function.Chainable
  Lt -> Function.Chainable
  Ge -> Function.Chainable
  Gt -> Function.Chainable
  Eq -> Function.Chainable
  Implies -> Function.RightAssociative
  And -> Function.LeftAssociative
  Or -> Function.LeftAssociative
  Xor -> Function.LeftAssociative
  Distinct -> Function.Pairwise

and :: Function 2
and = And

true :: Constant
true = True

false :: Constant
false = False

isBool :: Sort -> Prelude.Bool
isBool = \case
  Bool -> Prelude.True
  _ -> Prelude.False
