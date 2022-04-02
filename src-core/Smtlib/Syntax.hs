{-# language DuplicateRecordFields #-}

module Smtlib.Syntax
  ( Constant(..)
  , Identifier(..)
  , Index(..)
  , QualIdentifier(..)
  , Sort(..)
  , SortedVar(..)
  , Term(..)
  , VarBinding(..)
  ) where

import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray)
import GHC.Exts (IsString)

import qualified GHC.Exts
import qualified Data.Text.Short as TS

-- | Described by:
--
-- > 〈qual_identifier 〉 ::= 〈identifier 〉 | ( as 〈identifier 〉 〈sort〉 )
data QualIdentifier = QualIdentifier
  { identifier :: !Identifier
  , sort :: !(Maybe Sort)
  } deriving (Eq,Show)

instance IsString QualIdentifier where
  fromString s = QualIdentifier
    {identifier=Identifier{symbol=TS.pack s,indices=mempty}
    ,sort=Nothing
    }

-- | Described by:
--
-- > 〈sort〉 ::= 〈identifier 〉 | ( 〈identifier 〉 〈sort〉+ )
data Sort = Sort
  { identifier :: !Identifier
  , sort :: !(SmallArray Sort)
  } deriving (Eq,Show)

-- | Described by:
--
-- > 〈sorted_var 〉 ::= ( 〈symbol 〉 〈sort〉 )
data SortedVar = SortedVar
  { symbol :: !ShortText
  , sort :: !Sort
  } deriving (Eq,Show)

-- | Described by:
--
-- > 〈var_binding 〉 ::= ( 〈symbol 〉 〈term〉 )
data VarBinding = VarBinding
  { symbol :: !ShortText
  , term :: !Term
  } deriving (Eq,Show)

-- | Described by:
--
-- > 〈index〉 ::= 〈numeral〉 | 〈symbol〉
data Index = IndexNumeral !Integer | IndexSymbol !ShortText
  deriving (Eq,Show)

instance IsString Index where
  fromString = IndexSymbol . TS.pack

-- | Described by:
--
-- > 〈identifier〉 ::= 〈symbol〉 | ( _ 〈symbol〉 〈index〉+ )
data Identifier = Identifier
  { symbol :: !ShortText
  , indices :: !(SmallArray Index)
  } deriving (Eq,Show)

-- | Described by:
--
-- > 〈term〉 ::=
-- >      〈spec_constant〉
-- >    | 〈qual_identifier 〉
-- >    | ( 〈qual_identifier 〉 〈term〉+ )
-- >    | ( let ( 〈var_binding 〉+ ) 〈term〉 )
-- >    | ( forall ( 〈sorted_var 〉+ ) 〈term〉 )
-- >    | ( exists ( 〈sorted_var 〉+ ) 〈term〉 )
-- >    | ( match 〈term〉 ( 〈match_case〉+ ) )
-- >    | ( ! 〈term〉 〈attribute〉+ )
--
-- Note: Many of the SmallArray fields must be non-empty, but it is difficult
-- to enforce this.
--
-- TODO: add Attributes back
data Term
  = Literal Constant
    -- ^ No reserved keyword for Literal.
  | Variable QualIdentifier
  | Apply QualIdentifier !(SmallArray Term)
    -- ^ No reserved keyword for Apply. This handles only the non-nullary rule.
    -- The SmallArray must have length >= 1, but this is not enforced. If you mess
    -- this up, it will result in nullary applications like (myFunc), with the parens,
    -- which any further parsing will reject.
    -- The decoder never produces applications with length < 1.
  | Let !(SmallArray VarBinding) Term
  | Forall !(SmallArray SortedVar) Term
  | Exists !(SmallArray SortedVar) Term
  deriving (Eq,Show)

-- TODO: support bits and hexadecimal
data Constant
  = Numeral !Integer
  | Decimal !Rational
  | String !ShortText
  deriving (Eq,Show)

