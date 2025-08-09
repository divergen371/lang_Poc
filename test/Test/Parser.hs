module Test.Parser where

import Parser
import Syntax
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Expression parsing" $ do
    it "parses variables correctly" $ do
      parseExpr "x" `shouldBe` Right (Var "x")
      parseExpr "foo" `shouldBe` Right (Var "foo")

    it "parses lambda expressions correctly" $ do
      parseExpr "\\x. x" `shouldBe` Right (Lam "x" (Var "x"))
      parseExpr "\\f. \\x. f x" `shouldBe` Right (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))

    it "parses application correctly" $ do
      parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))
      parseExpr "f x y" `shouldBe` Right (App (App (Var "f") (Var "x")) (Var "y"))

    it "parses simple let expressions correctly" $ do
      parseExpr "let x = y in x" `shouldBe` Right (Let "x" (Var "y") (Var "x"))
      parseExpr "let f = g in f h" `shouldBe` Right (Let "f" (Var "g") (App (Var "f") (Var "h")))

    it "parses let expressions with lambda correctly" $ do
      parseExpr "let id = \\x. x in id" `shouldBe` Right (Let "id" (Lam "x" (Var "x")) (Var "id"))
      parseExpr "let f = \\x. \\y. x in f" `shouldBe` Right (Let "f" (Lam "x" (Lam "y" (Var "x"))) (Var "f"))

    it "parses parenthesized expressions correctly" $ do
      parseExpr "(\\x. x)" `shouldBe` Right (Lam "x" (Var "x"))
      parseExpr "let x = (\\y. y) in x" `shouldBe` Right (Let "x" (Lam "y" (Var "y")) (Var "x"))

  describe "Parsing errors" $ do
    it "rejects reserved words as identifiers" $ do
      parseExpr "let" `shouldSatisfy` isLeft
      parseExpr "in" `shouldSatisfy` isLeft
      parseExpr "perform" `shouldSatisfy` isLeft

    it "rejects incomplete let expressions" $ do
      parseExpr "let x =" `shouldSatisfy` isLeft
      parseExpr "let x = y" `shouldSatisfy` isLeft
      parseExpr "let x = y in" `shouldSatisfy` isLeft

  describe "Complex expressions" $ do
    it "parses nested let expressions" $ do
      parseExpr "let x = let y = z in y in x"
        `shouldBe` Right (Let "x" (Let "y" (Var "z") (Var "y")) (Var "x"))

    it "parses let with application" $ do
      parseExpr "let f = \\x. x in f y"
        `shouldBe` Right (Let "f" (Lam "x" (Var "x")) (App (Var "f") (Var "y")))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False