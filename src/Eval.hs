module Eval where

import Syntax

-- 簡単な実装
data Value = VInt Int | VBool Bool deriving (Show, Eq)

data Env = EmptyEnv deriving (Show, Eq)

-- 基本的な評価関数
eval :: Env -> Expr -> Value
eval env (Var x) = VInt 0 -- 簡単な実装
eval env (Lam x e) = VInt 0 -- 簡単な実装
eval env (App e1 e2) = VInt 0 -- 簡単な実装
eval env (Let x e1 e2) = VInt 0 -- 簡単な実装
eval env (Perform eff e) = VInt 0 -- 簡単な実装
eval env (Handle e1 e2) = VInt 0 -- 簡単な実装