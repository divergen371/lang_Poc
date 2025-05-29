module Kind
  ( Kind(..)
  , KindError(..)
  , kindCheck
  ) where

import Syntax

data Kind = Star | Row Kind
  deriving (Show, Eq)

-- タイポを修正: kindChack -> kindCheck
data KindError = KindMismatch | UnknownType deriving (Show, Eq)

kindCheck :: Ty -> Either KindError Kind
kindCheck (TV _) = Right Star
kindCheck (TFun _ _ _) = Right Star 