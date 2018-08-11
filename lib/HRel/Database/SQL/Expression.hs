{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Database.SQL.Expression
    ( Operator (..)
    , UnaryOperator (..)
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

import           Data.Foldable   (Foldable (..))
import           Data.Monoid     (mconcat)
import           Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import           Data.String     (IsString (fromString))
import qualified Data.Text       as Text

import HRel.Database.SQL.Builder
import HRel.Database.Value

data Operator operands result where
    Equals        :: Operator a Bool
    NotEquals     :: Operator a Bool
    Like          :: IsString a => Operator a Bool
    Lower         :: Operator a Bool
    LowerEquals   :: Operator a Bool
    Greater       :: Operator a Bool
    GreaterEquals :: Operator a Bool
    And           :: Operator Bool Bool
    Or            :: Operator Bool Bool
    Plus          :: Num a => Operator a a
    Minus         :: Num a => Operator a a
    Multiply      :: Num a => Operator a a
    Divide        :: Fractional a => Operator a a

buildOperator :: Operator a b -> Code
buildOperator = \case
    Equals        -> "="
    NotEquals     -> "!="
    Like          -> "LIKE"
    Lower         -> "<"
    LowerEquals   -> "<="
    Greater       -> ">"
    GreaterEquals -> ">="
    And           -> "AND"
    Or            -> "OR"
    Plus          -> "+"
    Minus         -> "-"
    Multiply      -> "*"
    Divide        -> "/"

data UnaryOperator operand result where
    Not      :: UnaryOperator Bool Bool
    Negate   :: Num a => UnaryOperator a a
    Absolute :: Num a => UnaryOperator a a
    SignOf   :: Num a => UnaryOperator a a

buildUnaryOperator :: UnaryOperator a b -> Expression i a -> Builder i Code
buildUnaryOperator Not      exp = do
    code <- buildExpression exp
    pure (mconcat ["(NOT ", code, ")"])

buildUnaryOperator Negate   exp = do
    code <- buildExpression exp
    pure (mconcat ["(-", code, ")"])

buildUnaryOperator Absolute exp = do
    code <- buildExpression exp
    pure (mconcat ["ABS(", code, ")"])

buildUnaryOperator SignOf   exp = do
    code <- buildExpression exp
    pure (mconcat ["SIGN(", code, ")"])

data Expression i a where
    Variable  :: !Name -> Expression i a
    IntLit    :: Num a => !Integer -> Expression i a
    RealLit   :: Fractional a => !Scientific -> Expression i a
    BoolLit   :: !Bool -> Expression i Bool
    StringLit :: IsString a => !Text.Text -> Expression i a
    Parameter :: !(i -> Value) -> Expression i a
    BinaryOp  :: !(Operator a b) -> Expression i a -> Expression i a -> Expression i b
    UnaryOp   :: !(UnaryOperator a b) -> !(Expression i a) -> Expression i b
    Access    :: !(Expression i a) -> !Name -> Expression i b

instance Show (Expression i a) where
    show = Text.unpack . unCode . evalBuilder . buildExpression

instance Num a => Num (Expression i a) where
    (+)         = BinaryOp Plus
    (-)         = BinaryOp Minus
    (*)         = BinaryOp Multiply
    negate      = UnaryOp Negate
    abs         = UnaryOp Absolute
    signum      = UnaryOp SignOf
    fromInteger = IntLit

instance Fractional a => Fractional (Expression i a) where
    (/) = BinaryOp Divide

    recip = BinaryOp Divide (IntLit 1)

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

    BinaryOp op lhs rhs -> do
        lhsCode <- buildExpression lhs
        rhsCode <- buildExpression rhs
        pure $ mconcat
            [ "("
            , lhsCode
            , " "
            , buildOperator op
            , " "
            , rhsCode
            , ")"
            ]

    UnaryOp op exp ->
        buildUnaryOperator op exp

    Access exp name -> do
        code <- buildExpression exp
        pure $ mconcat
            [ "("
            , code
            , "."
            , quoteName name
            , ")"
            ]

paramWith :: (i -> Value) -> Expression i a
paramWith = Parameter

true :: Expression i Bool
true = BoolLit True

false :: Expression i Bool
false = BoolLit False

(&&) :: Expression i Bool -> Expression i Bool -> Expression i Bool
(&&) = BinaryOp And

infixr 3 &&

and :: Foldable f => f (Expression i Bool) -> Expression i Bool
and = foldl' (&&) true

(||) :: Expression i Bool -> Expression i Bool -> Expression i Bool
(||) = BinaryOp Or

or :: Foldable f => f (Expression i Bool) -> Expression i Bool
or = foldl' (||) false

infixr 2 ||

not :: Expression i Bool -> Expression i Bool
not = UnaryOp Not

(==) :: Expression i a -> Expression i a -> Expression i Bool
(==) = BinaryOp Equals

infixr 4 ==

(/=) :: Expression i a -> Expression i a -> Expression i Bool
(/=) = BinaryOp NotEquals

infixr 4 /=

(<) :: Expression i a -> Expression i a -> Expression i Bool
(<) = BinaryOp Lower

infixr 4 <

(<=) :: Expression i a -> Expression i a -> Expression i Bool
(<=) = BinaryOp LowerEquals

infixr 4 <=

(>) :: Expression i a -> Expression i a -> Expression i Bool
(>) = BinaryOp Greater

infixr 4 >

(>=) :: Expression i a -> Expression i a -> Expression i Bool
(>=) = BinaryOp GreaterEquals

infixr 4 >=

(~~) :: IsString a => Expression i a -> Expression i a -> Expression i Bool
(~~) = BinaryOp Like

infixr 4 ~~
