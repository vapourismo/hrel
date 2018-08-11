{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module HRel.Database.SQL.Expression
    ( Function (..)
    , Expression (..)

    , buildExpression

    , paramWith

    , true
    , false
    , (&&)
    , and
    , (||)
    , or
    , not
    , (==)
    , (/=)
    , (<)
    , (<=)
    , (>)
    , (>=)
    , (~~)
    )
where

import Prelude (Bool (..), Fractional (..), Integer, Maybe (Nothing), Num (..), Show (..), ($), (.))

import Control.Applicative

import           Data.Foldable    (Foldable (..))
import           Data.List        (intersperse)
import           Data.Monoid      (mconcat, (<>))
import           Data.Scientific  (FPFormat (Fixed), Scientific, formatScientific)
import           Data.String      (IsString (fromString))
import qualified Data.Text        as Text
import           Data.Traversable (sequenceA)
import           Data.Vinyl       (Rec (..))

import HRel.Database.SQL.Builder (Builder, Code (..), Name, evalBuilder, mkParam, quoteName,
                                  quoteString)
import HRel.Database.Value       (Value)

data Function params ret where
    BinaryOperator :: Text.Text -> Function '[a, b] r
    UnaryOperator  :: Text.Text -> Function '[a] r
    Function       :: Text.Text -> Function xs r

buildTuple :: Rec (Expression i) xs -> Builder i Code
buildTuple params =
    mconcat . intersperse ", " <$> sequenceA (unwind params)
    where
        unwind :: Rec (Expression i) xs -> [Builder i Code]
        unwind RNil          = []
        unwind (exp :& rest) = buildExpression exp : unwind rest

buildFunctionCall :: Function xs r -> Rec (Expression i) xs -> Builder i Code
buildFunctionCall function params =
    case function of
        BinaryOperator operator | lhs :& rhs :& RNil <- params -> do
            lhsCode <- buildExpression lhs
            rhsCode <- buildExpression rhs

            pure $ mconcat
                [ "("
                , lhsCode
                , " "
                , Code operator
                , " "
                , rhsCode
                , ")"
                ]

        UnaryOperator operator | exp :& RNil <- params -> do
            code <- buildExpression exp
            pure (mconcat ["(", Code operator, " ", code, ")"])

        Function funName -> do
            tupleCode <- buildTuple params
            pure (Code funName <> tupleCode)

data Expression i a where
    Variable  :: Name -> Expression i a
    IntLit    :: Num a => Integer -> Expression i a
    RealLit   :: Fractional a => Scientific -> Expression i a
    BoolLit   :: Bool -> Expression i Bool
    StringLit :: IsString a => Text.Text -> Expression i a
    Parameter :: (i -> Value) -> Expression i a
    Access    :: (Expression i a) -> Name -> Expression i b
    Apply     :: Function xs a -> Rec (Expression i) xs -> Expression i a

instance Show (Expression i a) where
    show = Text.unpack . unCode . evalBuilder . buildExpression

instance IsString a => IsString (Expression i a) where
    fromString = StringLit . fromString

instance Num a => Num (Expression i a) where
    lhs + rhs   = Apply (BinaryOperator "+") (lhs :& rhs :& RNil)
    lhs - rhs   = Apply (BinaryOperator "-") (lhs :& rhs :& RNil)
    lhs * rhs   = Apply (BinaryOperator "*") (lhs :& rhs :& RNil)
    negate exp  = Apply (UnaryOperator "-") (exp :& RNil)
    abs exp     = Apply (Function "ABS") (exp :& RNil)
    signum exp  = Apply (Function "SIGN") (exp :& RNil)
    fromInteger = IntLit

instance Fractional a => Fractional (Expression i a) where
    lhs / rhs   = Apply (BinaryOperator "/") (lhs :& rhs :& RNil)

    recip = (1 /)

    fromRational = RealLit . fromRational

buildExpression :: Expression i a -> Builder i Code
buildExpression = \case
    Variable name ->
        pure (quoteName name)

    IntLit integer ->
        pure (fromString (show integer))

    RealLit real ->
        pure (fromString (formatScientific Fixed Nothing real))

    BoolLit True ->
        pure "true"

    BoolLit False ->
        pure "false"

    StringLit string ->
        pure (quoteString '\'' string)

    Parameter accessor ->
        mkParam accessor

    Access exp name -> do
        code <- buildExpression exp
        pure $ mconcat
            [ "("
            , code
            , "."
            , quoteName name
            , ")"
            ]

    Apply fun params ->
        buildFunctionCall fun params

paramWith :: (i -> Value) -> Expression i a
paramWith = Parameter

true :: Expression i Bool
true = BoolLit True

false :: Expression i Bool
false = BoolLit False

(&&) :: Expression i Bool -> Expression i Bool -> Expression i Bool
lhs && rhs = Apply (BinaryOperator "AND") (lhs :& rhs :& RNil)

infixr 3 &&

and :: Foldable f => f (Expression i Bool) -> Expression i Bool
and = foldl' (&&) true

(||) :: Expression i Bool -> Expression i Bool -> Expression i Bool
lhs || rhs = Apply (BinaryOperator "OR") (lhs :& rhs :& RNil)

or :: Foldable f => f (Expression i Bool) -> Expression i Bool
or = foldl' (||) false

infixr 2 ||

not :: Expression i Bool -> Expression i Bool
not exp = Apply (UnaryOperator "NOT") (exp :& RNil)

(==) :: Expression i a -> Expression i a -> Expression i Bool
lhs == rhs = Apply (BinaryOperator "=") (lhs :& rhs :& RNil)

infixr 4 ==

(/=) :: Expression i a -> Expression i a -> Expression i Bool
lhs /= rhs = Apply (BinaryOperator "!=") (lhs :& rhs :& RNil)

infixr 4 /=

(<) :: Expression i a -> Expression i a -> Expression i Bool
lhs < rhs = Apply (BinaryOperator "<") (lhs :& rhs :& RNil)

infixr 4 <

(<=) :: Expression i a -> Expression i a -> Expression i Bool
lhs <= rhs = Apply (BinaryOperator "<=") (lhs :& rhs :& RNil)

infixr 4 <=

(>) :: Expression i a -> Expression i a -> Expression i Bool
lhs > rhs = Apply (BinaryOperator ">") (lhs :& rhs :& RNil)

infixr 4 >

(>=) :: Expression i a -> Expression i a -> Expression i Bool
lhs >= rhs = Apply (BinaryOperator ">=") (lhs :& rhs :& RNil)

infixr 4 >=

(~~) :: IsString a => Expression i a -> Expression i a -> Expression i Bool
lhs ~~ rhs = Apply (BinaryOperator "LIKE") (lhs :& rhs :& RNil)

infixr 4 ~~
