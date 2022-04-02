{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Smtlib.Function.Annotation
  ( Annotation(..)
  ) where

import GHC.TypeNats (Nat)
import Data.Kind (Type)

-- | Annotations for binary functions
data Annotation :: Nat -> Type where
  None :: Annotation n
  LeftAssociative :: Annotation 2
  RightAssociative :: Annotation 2
  Chainable :: Annotation 2
  Pairwise :: Annotation 2
