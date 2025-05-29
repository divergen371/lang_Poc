module ParserTest where

import Test.QuickCheck
import Parser
import Syntax

-- 基本的なパーサーテスト
testBasicExpressions :: IO ()
testBasicExpressions = do
  putStrLn "=== Basic Expression Tests ==="
  
  -- 変数
  testParse "x" "Variable"
  
  -- Lambda式
  testParse "\\x. x" "Lambda"
  
  -- アプリケーション
  testParse "f x" "Application"
  
  -- 簡単なlet式
  testParse "let x = y in x" "Simple Let"
  
  -- 問題のlet式
  testParse "let id = \\x. x in id" "Lambda in Let"
  
  -- より複雑な例
  testParse "let f = \\x. \\y. x in f" "Nested Lambda in Let"

-- 個別のテストヘルパー
testParse :: String -> String -> IO ()
testParse input description = do
  putStrLn $ "\nTesting: " ++ description
  putStrLn $ "Input: " ++ input
  case parseExpr input of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right ast -> putStrLn $ "SUCCESS: " ++ show ast

-- let式の詳細テスト
testLetExpressions :: IO ()
testLetExpressions = do
  putStrLn "\n=== Let Expression Tests ==="
  
  -- 段階的にテスト
  testParse "let" "Keyword only"
  testParse "let x" "With identifier"
  testParse "let x =" "With equals"
  testParse "let x = y" "With simple value"
  testParse "let x = y in" "With 'in' keyword"
  testParse "let x = y in z" "Complete simple let"
  
  -- Lambda関連
  testParse "\\x. x" "Just lambda"
  testParse "let x = \\y. y" "Lambda without 'in'"
  testParse "let x = (\\y. y)" "Lambda with parens"
  testParse "let x = (\\y. y) in x" "Lambda with parens in let"

-- メインテスト関数
runAllTests :: IO ()
runAllTests = do
  testBasicExpressions
  testLetExpressions
  putStrLn "\n=== Tests Complete ===" 