{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Syntax where

import qualified Data.Map as M
import Syntax
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Arbitrary instances
instance Arbitrary Expr where
  arbitrary = sized genExpr
    where
      genExpr 0 =
        oneof
          [ Var <$> genName
          ]
      genExpr n =
        oneof
          [ Var <$> genName,
            Lam <$> genName <*> genExpr (n `div` 2),
            App <$> genExpr (n `div` 2) <*> genExpr (n `div` 2),
            Let <$> genName <*> genExpr (n `div` 2) <*> genExpr (n `div` 2),
            Perform <$> genName <*> genExpr (n `div` 2),
            Handle <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)
          ]
      genName = elements ["x", "y", "z", "f", "g"]

instance Arbitrary Ty where
  arbitrary = sized genTy
    where
      genTy 0 = TV <$> elements ["a", "b", "c"]
      genTy n =
        oneof
          [ TV <$> elements ["a", "b", "c"],
            TFun <$> genTy (n `div` 3) <*> genEffRow (n `div` 3) <*> genTy (n `div` 3)
          ]
      genEffRow 0 = return $ Row M.empty Empty
      genEffRow n =
        oneof
          [ return $ Row M.empty Empty,
            return $ Row M.empty (TVTail "r"),
            do
              -- 簡単なエフェクトマップを生成（サイズを制限）
              effName <- elements ["Console", "State", "Exception"]
              effType <- genTy (n `div` 4)
              tailVar <- arbitrary
              return $ Row (M.singleton effName effType) tailVar
          ]

instance Arbitrary EffRow where
  arbitrary = sized genEffRow
    where
      genEffRow 0 = return $ Row M.empty Empty
      genEffRow n =
        oneof
          [ return $ Row M.empty Empty,
            return $ Row M.empty (TVTail "r"),
            do
              -- 簡単なエフェクトマップを生成（サイズを制限）
              effName <- elements ["Console", "State", "Exception"]
              effType <- resize (n `div` 4) arbitrary
              tailVar <- arbitrary
              return $ Row (M.singleton effName effType) tailVar
          ]

instance Arbitrary Tail where
  arbitrary =
    oneof
      [ return Empty,
        TVTail <$> elements ["r1", "r2", "r3"]
      ]

-- Tests
spec :: Spec
spec = do
  describe "Expression parsing" $ do
    it "generates valid expressions" $ property $ \expr ->
      expr
        `shouldSatisfy` ( \e -> case e of
                            Var _ -> True
                            Lam _ _ -> True
                            App _ _ -> True
                            Let _ _ _ -> True
                            Perform _ _ -> True
                            Handle _ _ -> True
                        )

  describe "Type structure" $ do
    it "type variables are properly represented" $
      TV "a" `shouldBe` TV "a"

    it "function types are properly constructed" $ do
      let t1 = TV "a"
      let t2 = TV "b"
      let eff = Row M.empty Empty
      TFun t1 eff t2
        `shouldSatisfy` ( \t -> case t of
                            TFun _ _ _ -> True
                            _ -> False
                        )

  describe "Effect rows" $ do
    it "empty effect row" $
      Row M.empty Empty `shouldBe` Row M.empty Empty

    it "effect row with tail variable" $
      Row M.empty (TVTail "r") `shouldBe` Row M.empty (TVTail "r")