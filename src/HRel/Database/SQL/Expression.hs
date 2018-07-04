{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Database.SQL.Expression
    ( Operator (..)
    , UnaryOperator (..)
    , Expression (..)
    , expressionToSql

    , PgBool
    , PgString
    , PgNumber

    , true
    , false
    , (&&)
    , and
    , (||)
    , or
    , not
    , (==)
    , (~~)

      -- * Re-exports
    , Fractional (..)
    , Num (..) )
where

import Prelude (Fractional (..), Num (..), Show (..), Word)

import Control.Category ((.))

import           Data.Foldable (Foldable (..))
import           Data.Ratio    (denominator, numerator)
import qualified Data.Text     as Text

newtype Field = Field Text.Text

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

data UnaryOperator operand result where
    Not      :: UnaryOperator PgBool PgBool
    Negate   :: UnaryOperator PgNumber PgNumber
    Absolute :: UnaryOperator PgNumber PgNumber
    SignOf   :: UnaryOperator PgNumber PgNumber

data Expression a where
    Name    :: Field -> Expression a
    Literal :: Text.Text -> Expression a
    Param   :: Word -> Expression a
    Infix   :: Operator a b -> Expression a -> Expression a -> Expression b
    Unary   :: UnaryOperator a b -> Expression a -> Expression b

instance Show (Expression a) where
    show = Text.unpack . expressionToSql

instance Num (Expression PgNumber) where
    (+)         = Infix Plus
    (-)         = Infix Minus
    (*)         = Infix Multiply
    negate      = Unary Negate
    abs         = Unary Absolute
    signum      = Unary SignOf
    fromInteger = Literal . Text.pack . show

instance Fractional (Expression PgNumber) where
    (/) = Infix Divide

    recip = Infix Divide (Literal "1")

    fromRational ratio =
        Infix
            Divide
            (Literal (Text.pack (show (numerator ratio))))
            (Literal (Text.pack (show (denominator ratio))))

operatorToSql :: Operator a b -> Expression a -> Expression a -> Text.Text
operatorToSql operator lhs rhs =
    Text.concat
        [ "("
        , expressionToSql lhs
        , " "
        , opText
        , " "
        , expressionToSql rhs
        , ")" ]
    where
        opText =
            case operator of
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

unaryOperatorToSql :: UnaryOperator a b -> Expression a -> Text.Text
unaryOperatorToSql Not      expr = Text.concat ["(NOT ", expressionToSql expr, ")"]
unaryOperatorToSql Negate   expr = Text.concat ["(- ", expressionToSql expr, ")"]
unaryOperatorToSql Absolute expr = Text.concat ["ABS(", expressionToSql expr, ")"]
unaryOperatorToSql SignOf   expr = Text.concat ["SIGN(", expressionToSql expr, ")"]

expressionToSql :: Expression a -> Text.Text
expressionToSql = \case
    Name (Field name) -> Text.concat ["\"", Text.replace "\"" "\"\"" name, "\""]
    Literal lit       -> lit
    Param num         -> Text.pack ('$' : show num)
    Infix op lhs rhs  -> operatorToSql op lhs rhs
    Unary op body     -> unaryOperatorToSql op body

true :: Expression PgBool
true = Literal "true"

false :: Expression PgBool
false = Literal "false"

(&&) :: Expression PgBool -> Expression PgBool -> Expression PgBool
(&&) = Infix And

infixr 3 &&

and :: Foldable f => f (Expression PgBool) -> Expression PgBool
and = foldl' (&&) true

(||) :: Expression PgBool -> Expression PgBool -> Expression PgBool
(||) = Infix Or

or :: Foldable f => f (Expression PgBool) -> Expression PgBool
or = foldl' (||) false

infixr 2 ||

not :: Expression PgBool -> Expression PgBool
not = Unary Not

(==) :: Expression a -> Expression a -> Expression PgBool
(==) = Infix Equals

infixr 4 ==

(~~) :: Expression PgString -> Expression PgString -> Expression PgBool
(~~) = Infix Like

infixr 4 ~~
