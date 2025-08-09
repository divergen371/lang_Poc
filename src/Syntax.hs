module Syntax where

import Data.Map (Map)

type EffName = String

data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  | Let String Expr Expr
  | Perform EffName Expr
  | Handle Expr Expr
  deriving (Show, Eq)

data Ty
  = TV String
  | TFun Ty EffRow Ty
  deriving (Eq, Ord, Show)

data EffRow = Row (Map EffName Ty) Tail
  deriving (Eq, Ord, Show)

data Tail = TVTail String | Empty
  deriving (Eq, Ord, Show)