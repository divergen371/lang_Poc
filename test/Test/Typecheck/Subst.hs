module Test.Typecheck.Subst where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M

import Syntax
import Typecheck.Subst
import Test.Syntax () -- Arbitrary instances

-- Tests
spec :: Spec
spec = do
  describe "Substitution operations" $ do
    it "empty substitution does nothing" $ property $ 
      forAll (resize 5 arbitrary) $ \ty ->
        apply emptySubst ty `shouldBe` (ty :: Ty)
    
    it "single variable substitution" $ do
      let subst = M.singleton "a" (TV "b")
      apply subst (TV "a") `shouldBe` TV "b"
    
    it "substitution leaves unrelated variables unchanged" $ do
      let subst = M.singleton "a" (TV "b")
      apply subst (TV "c") `shouldBe` TV "c"

  describe "Substitution composition" $ do
    it "compose with empty substitution" $ property $ 
      forAll (resize 3 arbitrary) $ \s ->
        compose s emptySubst `shouldBe` (s :: Subst)
    
    it "empty substitution is left identity" $ property $ 
      forAll (resize 3 arbitrary) $ \s ->
        compose emptySubst s `shouldBe` (s :: Subst)

  describe "Type errors" $ do
    it "TypeMismatch equality" $
      TypeMismatch `shouldBe` TypeMismatch
    
    it "UnificationError equality" $
      UnificationError `shouldBe` UnificationError
    
    it "OccursCheckFailed equality" $
      OccursCheckFailed `shouldBe` OccursCheckFailed 