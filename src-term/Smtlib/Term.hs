{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module Smtlib.Term
  ( Term(..)
  , decode
  ) where

import Prelude hiding (and)

import Arithmetic.Nat ((=?))
import Arithmetic.Types (WithNat(WithNat))
import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT),runMaybeT)
import Data.Primitive (Array,SmallArray)
import Data.Text.Short (ShortText)
import Data.Type.Equality ((:~:)(Refl),testEquality)
import Data.String (IsString(fromString))
import Smtlib.Logic (Constant,Sort,Function)
import Smtlib.Logic (decodeNumeral,decodeFunction,decodeSort)
import Smtlib.Logic (decodeString,functionAnnotation)
import Smtlib.Logic (and,isBool,true,false)
import Vector.Boxed (Vector)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text.Short as TS
import qualified Smtlib.Function.Annotation as Function
import qualified Smtlib.Syntax as Syntax
import qualified Vector.Boxed as Vector

data QualIdentifier = QualIdentifier
  { identifier :: !ShortText
  , sort :: !(Maybe Sort)
  } deriving (Eq,Show)

instance IsString QualIdentifier where
  fromString s = QualIdentifier{identifier=TS.pack s,sort= Nothing}

-- | A term.
--
-- * Variables do not support indices. Z3 parses indexed variables, but
--   it ignores the indices. CVC5 (more correctly) reports an error:
--   Unknown indexed literal.
data Term
  = Literal Constant
  | Variable QualIdentifier
  | forall n. Apply (Function n) !(Vector n Term)

instance Eq Term where
  Literal x == Literal y = x == y
  Variable x == Variable y = x == y
  Apply funcA argsA == Apply funcB argsB = case testEquality funcA funcB of
    Nothing -> Prelude.False
    Just Refl -> argsA == argsB
  _ == _ = False

deriving instance Show Term

decode :: Syntax.Term -> Maybe Term
decode = \case
  Syntax.Literal c -> case c of
    Syntax.Numeral n -> fmap Literal (decodeNumeral n)
    Syntax.String s -> fmap Literal (decodeString s)
    Syntax.Decimal{} -> Nothing
  Syntax.Apply Syntax.QualIdentifier{identifier=Syntax.Identifier{symbol,indices},sort} args0 -> do
    let !sz = PM.sizeofSmallArray args0
    WithNat arity func <- decodeFunction indices sort sz symbol
    args1 <- traverseDecode args0
    case functionAnnotation func of
      Function.None -> Vector.with args1 $ \args2 -> do
        let argsLen = Vector.length args2
        eq <- argsLen =? arity
        let args = Vector.substitute eq args2
        Just (Apply func args)
      -- TODO: Pairwise is wrong. This implementation just ignores the annotation.
      Function.Pairwise -> Vector.with args1 $ \args2 -> do
        let argsLen = Vector.length args2
        eq <- argsLen =? arity
        let args = Vector.substitute eq args2
        Just (Apply func args)
      Function.LeftAssociative -> do
        when (sz < 2) Nothing
        pure $! C.foldl'
          (\ !acc a -> Apply func (Vector.doubleton acc a)
          ) (PM.indexArray args1 0) (C.slice args1 1 (sz - 1))
      Function.RightAssociative -> do
        when (sz < 2) Nothing
        pure $! C.foldr'
          (\ !acc a -> Apply func (Vector.doubleton acc a)
          ) (PM.indexArray args1 (sz - 1)) (C.slice args1 0 (sz - 1))
      Function.Chainable -> do
        when (sz < 2) Nothing
        let acc0 = Apply func (Vector.doubleton (PM.indexArray args1 0) (PM.indexArray args1 1))
        let xs = C.slice args1 1 (sz - 2)
        let ys = C.slice args1 2 (sz - 2)
        pure $! C.foldlZipWith'
          (\acc a b -> Apply and (Vector.doubleton acc (Apply func (Vector.doubleton a b))))
          acc0 xs ys
  Syntax.Variable Syntax.QualIdentifier{identifier=Syntax.Identifier{symbol,indices},sort} -> do
    sort' <- traverse decodeSort sort
    case symbol of
      "true" -> do
        case sort' of
          Nothing -> pure ()
          Just s -> when (not (isBool s)) Nothing
        when (PM.sizeofSmallArray indices /= 0) Nothing
        Just (Literal true)
      "false" -> do
        case sort' of
          Nothing -> pure ()
          Just s -> when (not (isBool s)) Nothing
        when (PM.sizeofSmallArray indices /= 0) Nothing
        Just (Literal false)
      _ -> case PM.sizeofSmallArray indices of
        0 -> Just (Variable QualIdentifier{identifier=symbol,sort=sort'})
        _ -> Nothing

traverseDecode :: SmallArray Syntax.Term -> Maybe (Array Term)
traverseDecode !src = runST $ do
  let len = PM.sizeofSmallArray src
  dst <- PM.newArray len errorThunk
  let go !ix = if ix < len
        then do
          x <- MaybeT (pure (decode (PM.indexSmallArray src ix)))
          lift (PM.writeArray dst ix x)
          go (ix + 1)
        else lift (PM.unsafeFreezeArray dst)
  runMaybeT (go 0)

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = errorWithoutStackTrace "Smtlib.Term: mistake"

