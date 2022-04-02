{-# language BangPatterns #-}
{-# language LambdaCase #-}

module Smtlib.Expr
  ( Expr(..)
  , decode
  ) where

import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray)
import Data.Bytes.Parser (Parser)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes (Bytes)

import qualified Data.Builder.ST as B
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Ascii as Ascii
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Chunks as Chunks


-- TODO: add data constructor for literal reals.
data Expr
  = Symbol {-# UNPACK #-} !ShortText
  | String {-# UNPACK #-} !ShortText
  | Number !Integer
  | List {-# UNPACK #-} !(SmallArray Expr)
  deriving (Eq,Show)

decode :: Bytes -> Maybe Expr
decode !b = P.parseBytesMaybe sexpr b

isSpace :: Word8 -> Prelude.Bool
{-# inline isSpace #-}
isSpace w =
     w == c2w ' '
  || w == c2w '\t'
  || w == c2w '\r'
  || w == c2w '\n'

-- parse an expression, skipping any leading whitespace
sexpr :: Parser () s Expr
sexpr = do
  P.skipWhile isSpace
  -- TODO: use utf8
  Latin.any () >>= branch

stepList :: B.Builder s Expr -> Parser () s Expr
stepList !b = do
  P.skipWhile isSpace
  -- TODO: use utf8
  c <- Latin.any ()
  case c of
    ')' -> do
      !r <- P.effect (B.freeze b)
      let !arr = Chunks.concat r
      pure (List arr)
    _ -> do
      val <- branch c
      P.effect (B.push val b) >>= stepList

branch :: Char -> Parser () s Expr
branch = \case
  '(' -> P.effect B.new >>= stepList
  ')' -> P.fail ()
  '"' -> string
  '-' -> Latin.peek >>= \case
    Just c | c >= '0' && c <= '9' -> do
      x <- Latin.decUnsignedInteger ()
      let !x' = negate x
      pure (Number x')
    _ -> do
      Unsafe.unconsume 1 *> symbol
  c | c >= '0' && c <= '9' ->
        fmap Number (Latin.decTrailingInteger (ord c - ord '0'))
    | c >= '!' && c <= '~' -> -- FIXME: only ascii allowed in symbol
        Unsafe.unconsume 1 *> symbol
    | otherwise -> P.fail ()

-- This is only called in contexts where it is guaranteed to consume
-- at least one character.
symbol :: Parser () s Expr
symbol = do
  t <- Ascii.takeShortWhile (\c -> (c >= '!' && c <= '~') && c /= ')')
  pure (Symbol t)

-- FIXME: only supports ascii characters.
-- FIXME: does not support escape sequences.
string :: Parser () s Expr
string = do
  t <- Ascii.shortTrailedBy () '"'
  pure (String t)

c2w :: Char -> Word8
{-# inline c2w #-}
c2w = fromIntegral . ord
