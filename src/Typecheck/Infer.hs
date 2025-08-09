module Typecheck.Infer where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Kind as K
import Syntax
import Typecheck.Subst
import Typecheck.Unify

-- 型環境
type TypeEnv = M.Map String Ty

-- 型推論の状態（新しい型変数を生成するため）
type InferState = Int

-- 型推論のモナド
type InferM = StateT InferState (Either TypeErr)

-- 新しい型変数を生成
freshTyVar :: InferM Ty
freshTyVar = do
  n <- get
  put (n + 1)
  return $ TV ("t" ++ show n)

-- 基本的な型推論関数
infer :: TypeEnv -> Expr -> InferM (Subst, Ty)
infer env (Var x) =
  case M.lookup x env of
    Just t -> return (emptySubst, t)
    Nothing -> lift $ Left TypeMismatch
infer env (Lam x e) = do
  tv <- freshTyVar
  let env' = M.insert x tv env
  (s, t) <- infer env' e
  return (s, TFun tv (Row M.empty Empty) t)
infer env (App e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  tv <- freshTyVar
  s3 <- lift $ unify (apply s1 t1) (TFun t2 (Row M.empty Empty) tv)
  return (compose s3 (compose s2 s1), apply s3 tv)
infer env (Let x e1 e2) = do
  (s1, t1) <- infer env e1
  let env' = M.insert x t1 (applyEnv s1 env)
  (s2, t2) <- infer env' e2
  return (compose s2 s1, t2)
infer _ (Perform _ _) = do
  -- 簡単な実装
  tv <- freshTyVar
  return (emptySubst, tv)
infer _ (Handle _ _) = do
  -- 簡単な実装
  tv <- freshTyVar
  return (emptySubst, tv)

-- 型環境に置換を適用
applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s env = M.map (apply s) env
