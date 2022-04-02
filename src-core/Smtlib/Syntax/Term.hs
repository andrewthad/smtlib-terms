{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Smtlib.Syntax.Term
  ( decode
  ) where

import Smtlib.Expr (Expr)
import Smtlib.Syntax (Term)
import Data.Text.Short (ShortText)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C
import qualified Smtlib.Expr as In
import qualified Smtlib.Syntax as Out

decode :: Expr -> Maybe Term
decode = \case
  In.Number n -> Just (Out.Literal (Out.Numeral n))
  In.String s -> Just (Out.Literal (Out.String s))
  In.Symbol s -> if reserved s
    then Nothing
    else Just (Out.Variable (symbolToQualId s))
  In.List es -> case PM.sizeofSmallArray es of
    0 -> Nothing
    sz -> case PM.indexSmallArray es 0 of
      In.Symbol "let" -> Nothing
      In.Symbol "forall" -> Nothing
      In.Symbol "exists" -> Nothing
      In.Symbol "match" -> Nothing -- TODO: handle this
      In.Symbol "!" -> Nothing -- TODO: handle this
      In.Symbol s -> do
        let args0 = C.clone (C.slice es 1 (sz - 1))
        args1 <- traverse decode args0
        pure (Out.Apply (symbolToQualId s) args1)
      In.List{} -> Nothing -- TODO: handle this
      In.Number{} -> Nothing
      In.String{} -> Nothing

-- No explicit sort, no indices.
symbolToQualId :: ShortText -> Out.QualIdentifier
symbolToQualId !s = Out.QualIdentifier
  { identifier=Out.Identifier{symbol=s,indices=mempty}
  , sort=Nothing
  }

reserved :: ShortText -> Bool
reserved s = case s of
  "let" -> True
  "forall" -> True
  "exists" -> True
  "define" -> True
  _ -> False

