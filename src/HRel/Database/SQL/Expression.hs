{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module HRel.Database.SQL.Expression
    ( PgBool
    , PgString
    , PgNumber

    , Operator (..)
    , UnaryOperator (..)
    , Expression (..)

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

import Prelude (Bool (..), Fractional (..), Integer, Num (..), Show (..), Word)

import           Data.Foldable (Foldable (..))
import           Data.Ratio    (denominator, numerator)
import qualified Data.Text     as Text

import HRel.Database.SQL.Types (Name)

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

deriving instance Show (Operator a b)

data UnaryOperator operand result where
    Not      :: UnaryOperator PgBool PgBool
    Negate   :: UnaryOperator PgNumber PgNumber
    Absolute :: UnaryOperator PgNumber PgNumber
    SignOf   :: UnaryOperator PgNumber PgNumber

deriving instance Show (UnaryOperator a b)

data Expression a where
    Variable  :: Name -> Expression a
    IntLit    :: Integer -> Expression a
    BoolLit   :: Bool -> Expression a
    StringLit :: Text.Text -> Expression a
    Parameter :: Word -> Expression a
    BinaryOp  :: Operator a b -> Expression a -> Expression a -> Expression b
    UnaryOp   :: UnaryOperator a b -> Expression a -> Expression b

deriving instance Show (Expression a)

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

    fromRational ratio =
        BinaryOp
            Divide
            (IntLit (numerator ratio))
            (IntLit (denominator ratio))

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
