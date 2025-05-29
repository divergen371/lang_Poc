module Typecheck.Subst where

import Syntax
import Data.Map as M

-- 型の置換を表すデータ型
type Subst = M.Map String Ty

-- 型エラーを表すデータ型
data TypeErr 
    = UnificationError
    | OccursCheckFailed
    | TypeMismatch
    deriving (Show, Eq)

-- 空の置換
emptySubst :: Subst
emptySubst = M.empty

-- 置換の合成
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

-- 型に置換を適用
class Substitutable a where
    apply :: Subst -> a -> a

instance Substitutable Ty where
    apply s (TV name) = M.findWithDefault (TV name) name s
    apply s (TFun t1 eff t2) = TFun (apply s t1) (apply s eff) (apply s t2)

instance Substitutable EffRow where
    apply s (Row effects tailRow) = Row (M.map (apply s) effects) (applyTail s tailRow)
      where
        applyTail :: Subst -> Tail -> Tail
        applyTail subst (TVTail name) = 
            case M.lookup name subst of
                Just _ -> TVTail name  -- 簡単な実装
                Nothing -> TVTail name
        applyTail _ Empty = Empty
