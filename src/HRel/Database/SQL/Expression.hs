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

import Prelude
    ( Bool (..)
    , Fractional (..)
    , Integer
    , Maybe (Nothing)
    , Num (..)
    , Show (..)
    , Word
    , (.)
    )

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

buildUnaryOperator :: UnaryOperator a b -> Expression a -> Query
buildUnaryOperator Not      exp = mconcat ["(NOT ", buildExpression exp, ")"]
buildUnaryOperator Negate   exp = mconcat ["(-", buildExpression exp, ")"]
buildUnaryOperator Absolute exp = mconcat ["ABS(", buildExpression exp, ")"]
buildUnaryOperator SignOf   exp = mconcat ["SIGN(", buildExpression exp, ")"]

data Expression a where
    Variable  :: Name -> Expression a
    IntLit    :: Integer -> Expression PgNumber
    RealLit   :: Scientific -> Expression PgNumber
    BoolLit   :: Bool -> Expression PgBool
    StringLit :: Text.Text -> Expression PgString
    Parameter :: Word -> Expression a
    BinaryOp  :: Operator a b -> Expression a -> Expression a -> Expression b
    UnaryOp   :: UnaryOperator a b -> Expression a -> Expression b
    Access    :: Expression a -> Name -> Expression b

instance Show (Expression a) where
    show = CharString.unpack . fromQuery . buildExpression

instance Num (Expression PgNumber) where
    (+)         = BinaryOp Plus
    (-)         = BinaryOp Minus
    (*)         = BinaryOp Multiply
    negate      = UnaryOp Negate
    abs         = UnaryOp Absolute
    signum      = UnaryOp SignOf
    fromInteger = IntLit

instance Fractional (Expression PgNumber) where
    (/) = BinaryOp Divide

    recip = BinaryOp Divide (IntLit 1)

    fromRational = RealLit . fromRational

buildExpression :: Expression a -> Query
buildExpression = \case
    Variable name        -> quoteName name
    IntLit integer       -> fromString (show integer)
    RealLit real         -> fromString (formatScientific Fixed Nothing real)
    BoolLit True         -> "true"
    BoolLit False        -> "false"
    StringLit string     -> quoteString '\'' string
    Parameter index      -> fromString ('$' : show index)
    BinaryOp op lhs rhs  -> mconcat
        [ "("
        , buildExpression lhs
        , " "
        , buildOperator op
        , " "
        , buildExpression rhs
        , ")"
        ]
    UnaryOp op exp       -> buildUnaryOperator op exp
    Access exp name      -> mconcat
        [ "("
        , buildExpression exp
        , "."
        , quoteName name
        , ")"
        ]

true :: Expression PgBool
true = BoolLit True

false :: Expression PgBool
false = BoolLit False

(&&) :: Expression PgBool -> Expression PgBool -> Expression PgBool
(&&) = BinaryOp And

infixr 3 &&

and :: Foldable f => f (Expression PgBool) -> Expression PgBool
and = foldl' (&&) true

(||) :: Expression PgBool -> Expression PgBool -> Expression PgBool
(||) = BinaryOp Or

or :: Foldable f => f (Expression PgBool) -> Expression PgBool
or = foldl' (||) false

infixr 2 ||

not :: Expression PgBool -> Expression PgBool
not = UnaryOp Not

(==) :: Expression a -> Expression a -> Expression PgBool
(==) = BinaryOp Equals

infixr 4 ==

(/=) :: Expression a -> Expression a -> Expression PgBool
(/=) = BinaryOp NotEquals

infixr 4 /=

(<) :: Expression a -> Expression a -> Expression PgBool
(<) = BinaryOp Lower

infixr 4 <

(<=) :: Expression a -> Expression a -> Expression PgBool
(<=) = BinaryOp LowerEquals

infixr 4 <=

(>) :: Expression a -> Expression a -> Expression PgBool
(>) = BinaryOp Greater

infixr 4 >

(>=) :: Expression a -> Expression a -> Expression PgBool
(>=) = BinaryOp GreaterEquals

infixr 4 >=

(~~) :: Expression PgString -> Expression PgString -> Expression PgBool
(~~) = BinaryOp Like

infixr 4 ~~
