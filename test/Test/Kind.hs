module Test.Kind where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M

import qualified Kind as K
import qualified Syntax as S

-- QuickCheck Arbitrary instances for Kind
instance Arbitrary K.Kind where
  arbitrary = oneof
    [ return K.Star
    , K.Row <$> arbitrary
    ]

instance Arbitrary K.KindError where
  arbitrary = oneof
    [ return K.KindMismatch
    , return K.UnknownType
    ]

-- Tests
spec :: Spec
spec = do
  describe "Kind checking" $ do
    it "type variables have kind Star" $
      K.kindCheck (S.TV "a") `shouldBe` Right K.Star
    
    it "function types have kind Star" $ do
      let t1 = S.TV "a"
      let t2 = S.TV "b"
      let eff = S.Row M.empty S.Empty
      K.kindCheck (S.TFun t1 eff t2) `shouldBe` Right K.Star

  describe "Kind structure" $ do
    it "Star kind equality" $
      K.Star `shouldBe` K.Star
    
    it "Row kind construction" $
      K.Row K.Star `shouldSatisfy` (\(K.Row _) -> True)

  describe "Kind error handling" $ do
    it "KindMismatch equality" $
      K.KindMismatch `shouldBe` K.KindMismatch
    
    it "UnknownType equality" $
      K.UnknownType `shouldBe` K.UnknownType 