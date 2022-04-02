{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import Smtlib.Expr (Expr(List,Symbol,Number,String),decode)

import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Primitive.Contiguous as C
import qualified Test.Tasty.HUnit as THU
import qualified Smtlib.Syntax as S
import qualified Smtlib.Syntax.Term as Term
import qualified Smtlib.Term.Lia as Lia
import qualified Smtlib.Logic.Lia as Lia
import qualified Vector.Boxed as Vector

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test"
  [ testGroup "sexpr"
    [ testGroup "spec"
      [ THU.testCase "A" $
          Just (List mempty)
          @=?
          decode (Ascii.fromString "()")
      , THU.testCase "B" $
          Just (List (C.singleton (Symbol "hello")))
          @=?
          decode (Ascii.fromString "(hello)")
      , THU.testCase "C" $
          Just (List (C.doubleton (Symbol "hello") (Symbol "world")))
          @=?
          decode (Ascii.fromString "(hello world)")
      , THU.testCase "D" $
          Just (List (C.tripleton (Number 1) (Number 2) (Number 3)))
          @=?
          decode (Ascii.fromString "(1 2 3)")
      , THU.testCase "E" $
          Just (List (C.tripleton (Symbol "concat") (String "foo") (String "bar")))
          @=?
          decode (Ascii.fromString "(concat \"foo\" \"bar\")")
      , THU.testCase "F" $
          Just (List (C.doubleton (List mempty) (List (C.doubleton (Symbol "a") (Symbol "b")))))
          @=?
          decode (Ascii.fromString "(() (a b))")
      ]
    ]
  , testGroup "smtlib"
    [ testGroup "spec"
      [ THU.testCase "A" $
          Just
            ( S.Apply "+"
              ( C.doubleton
                ( S.Literal (S.Numeral 55))
                ( S.Variable "x")
              )
            )
          @=?
          (Term.decode =<< decode (Ascii.fromString "(+ 55 x)"))
      , THU.testCase "B" $
          Just
            ( S.Apply "<="
              ( C.doubleton
                ( S.Apply "+"
                  ( C.doubleton
                    (S.Variable "b")
                    (S.Apply "*"
                      ( C.doubleton
                        (S.Apply "-" (C.singleton (S.Literal (S.Numeral 1))))
                        (S.Variable "a")
                      )
                    )
                  )
                )
                ( S.Literal (S.Numeral 0))
              )
            )
          @=?
          (Term.decode =<< decode (Ascii.fromString "(<= (+ b (* (- 1) a)) 0)"))
      , THU.testCase "C" $
          Just
            ( S.Apply "+"
              ( C.tripleton
                (S.Variable "a")
                (S.Variable "b")
                (S.Variable "c")
              )
            )
          @=?
          (Term.decode =<< decode (Ascii.fromString "(+ a b c)"))
      ]
    ]
  , testGroup "lia"
    [ testGroup "spec"
      [ THU.testCase "A" $
          Just
            ( Lia.Apply Lia.Add
              ( Vector.doubleton
                ( Lia.Apply Lia.Add (Vector.doubleton (Lia.Literal (Lia.Integer 1)) (Lia.Literal (Lia.Integer 2))))
                ( Lia.Literal (Lia.Integer 3))
              )
            )
          @=?
          (Lia.decode =<< Term.decode =<< decode (Ascii.fromString "(+ 1 2 3)"))
      , THU.testCase "B" $
          Just
            ( Lia.Apply Lia.Div
              ( Vector.doubleton
                ( Lia.Literal (Lia.Integer 4))
                ( Lia.Literal (Lia.Integer 2))
              )
            )
          @=?
          (Lia.decode =<< Term.decode =<< decode (Ascii.fromString "(div 4 2)"))
      , THU.testCase "C" $
          Just
            ( Lia.Apply Lia.And
              ( Vector.doubleton
                ( Lia.Apply Lia.Eq (Vector.doubleton (Lia.Variable "x") (Lia.Variable "y")))
                ( Lia.Apply Lia.Eq (Vector.doubleton (Lia.Variable "y") (Lia.Variable "z")))
              )
            )
          @=?
          (Lia.decode =<< Term.decode =<< decode (Ascii.fromString "(= x y z)"))
      , THU.testCase "D" $
          Just
            (Lia.Apply Lia.Subtract (Vector.doubleton (Lia.Variable "x") (Lia.Variable "y")))
          @=?
          (Lia.decode =<< Term.decode =<< decode (Ascii.fromString "(- x y)"))
      , THU.testCase "E" $
          Just (Lia.Apply Lia.Negate (Vector.singleton (Lia.Variable "x")))
          @=?
          (Lia.decode =<< Term.decode =<< decode (Ascii.fromString "(- x)"))
      ]
    ]
  ]

