{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module HRel.Database.SQL.Expression
    ( PgBool
    , PgString
    , PgNumber

    , Operator (..)
    , UnaryOperator (..)
    , Expression (..)

    , buildExpression

    , param

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

import qualified Data.ByteString.Char8 as CharString
import           Data.Foldable         (Foldable (..))
import           Data.Monoid
import           Data.Scientific
import           Data.String
import qualified Data.Text             as Text

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Types

data PgBool

data PgString

data PgNumber

data Operator operands result where
    Equals        :: Operator a        PgBool
    NotEquals     :: Operator a        PgBool
    Like          :: Operator PgString PgBool
    Lower         :: Operator a        PgBool
    LowerEquals   :: Operator a        PgBool
    Greater       :: Operator a        PgBool
    GreaterEquals :: Operator a        PgBool
    And           :: Operator PgBool   PgBool
    Or            :: Operator PgBool   PgBool
    Plus          :: Operator PgNumber PgNumber
    Minus         :: Operator PgNumber PgNumber
    Multiply      :: Operator PgNumber PgNumber
    Divide        :: Operator PgNumber PgNumber

buildOperator :: Operator a b -> Query
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
    Not      :: UnaryOperator PgBool PgBool
    Negate   :: UnaryOperator PgNumber PgNumber
    Absolute :: UnaryOperator PgNumber PgNumber
    SignOf   :: UnaryOperator PgNumber PgNumber

buildUnaryOperator :: UnaryOperator a b -> Expression i a -> Builder i Query
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
    Variable  :: Name -> Expression i a
    IntLit    :: Integer -> Expression i PgNumber
    RealLit   :: Scientific -> Expression i PgNumber
    BoolLit   :: Bool -> Expression i PgBool
    StringLit :: Text.Text -> Expression i PgString
    Parameter :: (i -> Param) -> Expression i a
    BinaryOp  :: Operator a b -> Expression i a -> Expression i a -> Expression i b
    UnaryOp   :: UnaryOperator a b -> Expression i a -> Expression i b
    Access    :: Expression i a -> Name -> Expression i b

instance Show (Expression i a) where
    show = CharString.unpack . fromQuery . evalBuilder . buildExpression

instance Num (Expression i PgNumber) where
    (+)         = BinaryOp Plus
    (-)         = BinaryOp Minus
    (*)         = BinaryOp Multiply
    negate      = UnaryOp Negate
    abs         = UnaryOp Absolute
    signum      = UnaryOp SignOf
    fromInteger = IntLit

instance Fractional (Expression i PgNumber) where
    (/) = BinaryOp Divide

    recip = BinaryOp Divide (IntLit 1)

    fromRational = RealLit . fromRational

buildExpression :: Expression i a -> Builder i Query
buildExpression = \case
    Variable name        -> pure (quoteName name)
    IntLit integer       -> pure (fromString (show integer))
    RealLit real         -> pure (fromString (formatScientific Fixed Nothing real))
    BoolLit True         -> pure "true"
    BoolLit False        -> pure "false"
    StringLit string     -> pure (quoteString '\'' string)
    Parameter accessor   -> fromString . ('$' :) . show <$> mkParam accessor
    BinaryOp op lhs rhs  -> do
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
    UnaryOp op exp       -> buildUnaryOperator op exp
    Access exp name      -> do
        code <- buildExpression exp
        pure $ mconcat
            [ "("
            , code
            , "."
            , quoteName name
            , ")"
            ]

param :: (i -> Param) -> Expression i a
param = Parameter

true :: Expression i PgBool
true = BoolLit True

false :: Expression i PgBool
false = BoolLit False

(&&) :: Expression i PgBool -> Expression i PgBool -> Expression i PgBool
(&&) = BinaryOp And

infixr 3 &&

and :: Foldable f => f (Expression i PgBool) -> Expression i PgBool
and = foldl' (&&) true

(||) :: Expression i PgBool -> Expression i PgBool -> Expression i PgBool
(||) = BinaryOp Or

or :: Foldable f => f (Expression i PgBool) -> Expression i PgBool
or = foldl' (||) false

infixr 2 ||

not :: Expression i PgBool -> Expression i PgBool
not = UnaryOp Not

(==) :: Expression i a -> Expression i a -> Expression i PgBool
(==) = BinaryOp Equals

infixr 4 ==

(/=) :: Expression i a -> Expression i a -> Expression i PgBool
(/=) = BinaryOp NotEquals

infixr 4 /=

(<) :: Expression i a -> Expression i a -> Expression i PgBool
(<) = BinaryOp Lower

infixr 4 <

(<=) :: Expression i a -> Expression i a -> Expression i PgBool
(<=) = BinaryOp LowerEquals

infixr 4 <=

(>) :: Expression i a -> Expression i a -> Expression i PgBool
(>) = BinaryOp Greater

infixr 4 >

(>=) :: Expression i a -> Expression i a -> Expression i PgBool
(>=) = BinaryOp GreaterEquals

infixr 4 >=

(~~) :: Expression i PgString -> Expression i PgString -> Expression i PgBool
(~~) = BinaryOp Like

infixr 4 ~~
