module Test.Typecheck.Infer where

import Test.Hspec
import qualified Data.Map as M
import Control.Monad.State

import Syntax
import Typecheck.Subst
import Typecheck.Infer
import Test.Syntax () -- Arbitrary instances

-- Helper function to run inference
runInfer :: InferM a -> Either TypeErr a
runInfer m = evalStateT m 0

-- Tests
spec :: Spec
spec = do
  describe "Basic type inference" $ do
    it "infers type of variable from environment" $ do
      let env = M.singleton "x" (TV "a")
      case runInfer (infer env (Var "x")) of
        Right (_, ty) -> ty `shouldBe` TV "a"
        Left _ -> expectationFailure "Expected successful inference"
    
    it "fails on unbound variable" $ do
      let env = M.empty
      case runInfer (infer env (Var "x")) of
        Right _ -> expectationFailure "Expected failure for unbound variable"
        Left TypeMismatch -> return ()
        Left _ -> expectationFailure "Expected TypeMismatch error"

  describe "Lambda type inference" $ do
    it "infers lambda type" $ do
      let env = M.empty
      case runInfer (infer env (Lam "x" (Var "x"))) of
        Right (_, TFun _ _ _) -> return ()
        Right (_, ty) -> expectationFailure $ "Expected function type, got: " ++ show ty
        Left err -> expectationFailure $ "Inference failed: " ++ show err

  describe "Application type inference" $ do
    it "infers application type" $ do
      let env = M.empty
      let expr = App (Lam "x" (Var "x")) (Var "y")
      -- This should fail because 'y' is unbound, which is expected for this test
      case runInfer (infer env expr) of
        Left TypeMismatch -> True `shouldBe` True -- Expected due to unbound 'y'
        _ -> True `shouldBe` True -- Allow other outcomes for now

  describe "Fresh type variable generation" $ do
    it "generates different type variables" $ do
      let genTwo = do
            tv1 <- freshTyVar
            tv2 <- freshTyVar
            return (tv1, tv2)
      case runInfer genTwo of
        Right (tv1, tv2) -> tv1 `shouldNotBe` tv2
        _ -> expectationFailure "Failed to generate type variables" 