module Typecheck.Unify where

import Syntax
import Typecheck.Subst
import qualified Kind as K

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

-- 基本的なunify関数
unify :: Ty -> Ty -> Either TypeErr Subst
unify (TV a) (TV b) 
    | a == b = Right emptySubst  -- 同じ型変数なら空の置換
    | otherwise = Right $ M.singleton a (TV b)
unify (TV a) t = Right $ M.singleton a t
unify t (TV a) = Right $ M.singleton a t
unify (TFun t1 eff1 t2) (TFun t3 eff3 t4) = do
    s1 <- unify t1 t3
    s2 <- unifyRow eff1 eff3
    s3 <- unify t2 t4
    Right $ compose s3 (compose s2 s1)
unify _ _ = Left TypeMismatch

-- エフェクト行の単純化されたunify
unifyRow :: EffRow -> EffRow -> Either TypeErr Subst
unifyRow (Row l1 t1) (Row l2 t2) = do
    -- 簡単な実装：共通のエフェクトをチェック
    let common = M.intersectionWith (,) l1 l2
    sFields <- mapM (uncurry unify) (M.elems common)
    let s1 = foldr compose emptySubst sFields
    -- 末尾の単純化されたunify
    sTail <- unifyTail t1 t2
    Right $ compose sTail s1

-- 末尾のunify
unifyTail :: Tail -> Tail -> Either TypeErr Subst
unifyTail Empty Empty = Right emptySubst
unifyTail (TVTail a) tail = Right $ M.empty  -- 簡単な実装
unifyTail tail (TVTail a) = Right $ M.empty  -- 簡単な実装
unifyTail _ _ = Left TypeMismatch