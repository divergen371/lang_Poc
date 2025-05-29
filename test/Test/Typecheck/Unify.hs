module Test.Typecheck.Unify where

import Test.Hspec
import qualified Data.Map as M

import Syntax
import Typecheck.Subst
import Typecheck.Unify
import Test.Syntax () -- Arbitrary instances

-- Tests
spec :: Spec
spec = do
  describe "Basic type unification" $ do
    it "unifies identical type variables" $
      unify (TV "a") (TV "a") `shouldBe` Right emptySubst
    
    it "unifies type variable with type" $ do
      let result = unify (TV "a") (TV "b")
      case result of
        Right s -> M.size s `shouldBe` 1
        Left _ -> expectationFailure "Expected successful unification"

  describe "Function type unification" $ do
    it "unifies simple function types" $ do
      let t1 = TFun (TV "a") (Row M.empty Empty) (TV "b")
      let t2 = TFun (TV "c") (Row M.empty Empty) (TV "d")
      case unify t1 t2 of
        Right _ -> return ()
        Left _ -> expectationFailure "Expected successful unification"

  describe "Effect row unification" $ do
    it "unifies empty effect rows" $ do
      let eff1 = Row M.empty Empty
      let eff2 = Row M.empty Empty
      unifyRow eff1 eff2 `shouldBe` Right emptySubst

  describe "Tail unification" $ do
    it "unifies empty tails" $
      unifyTail Empty Empty `shouldBe` Right emptySubst 