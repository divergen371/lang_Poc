module Main where

import System.IO
import Control.Monad.State
import qualified Data.Map as M

import Syntax
import Parser
import Typecheck.Infer
import Typecheck.Subst

-- REPL のコンテキスト
data ReplContext = ReplContext 
  { replTypeEnv :: TypeEnv  -- 型環境
  , replValueEnv :: M.Map String Expr  -- 値環境（簡易的な実装）
  } deriving (Show)

initialContext :: ReplContext
initialContext = ReplContext M.empty M.empty

-- REPL のメインループ
repl :: ReplContext -> IO ()
repl ctx = do
  putStr "effect-poc> "
  hFlush stdout
  input <- getLine
  case input of
    ":quit" -> putStrLn "Goodbye!"
    ":q"    -> putStrLn "Goodbye!"
    ":type" -> do
      putStrLn "Usage: :type <expression>"
      repl ctx
    ':':'t':'y':'p':'e':' ':expr -> do
      -- 式の型を表示
      case parseExpr expr of
        Left err -> do
          putStrLn $ "Parse error: " ++ err
          repl ctx
        Right ast -> do
          case runInfer (infer (replTypeEnv ctx) ast) of
            Left err -> do
              putStrLn $ "Type error: " ++ show err
              repl ctx
            Right (_, ty) -> do
              putStrLn $ "Type: " ++ prettyType ty
              repl ctx
    ":help" -> do
      printHelp
      repl ctx
    ":h" -> do
      printHelp
      repl ctx
    ":?" -> do
      printHelp
      repl ctx
    _ -> do
      -- 式を評価
      case parseExpr input of
        Left err -> do
          putStrLn $ "Parse error: " ++ err
          repl ctx
        Right ast -> do
          putStrLn $ "Parsed: " ++ show ast
          case runInfer (infer (replTypeEnv ctx) ast) of
            Left err -> do
              putStrLn $ "Type error: " ++ show err
              repl ctx
            Right (_, ty) -> do
              putStrLn $ "Type: " ++ prettyType ty
              -- TODO: 評価を実装
              putStrLn $ "Value: <evaluation not implemented yet>"
              repl ctx

-- 型推論の実行
runInfer :: InferM a -> Either TypeErr a
runInfer m = evalStateT m 0

-- 型のプリティプリント
prettyType :: Ty -> String
prettyType (TV name) = name
prettyType (TFun t1 eff t2) = 
  prettyType t1 ++ " -[" ++ prettyEffRow eff ++ "]> " ++ prettyType t2

prettyEffRow :: EffRow -> String
prettyEffRow (Row effs Empty) | M.null effs = "∅"
prettyEffRow (Row effs tailRow) = 
  let effStrs = map (\(name, ty) -> name ++ "(" ++ prettyType ty ++ ")") (M.toList effs)
      tailStr = case tailRow of
        Empty -> ""
        TVTail name -> " | " ++ name
  in if null effStrs
     then case tailRow of
       Empty -> "∅"
       TVTail name -> name
     else unwords effStrs ++ tailStr

-- ヘルプメッセージ
printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "Mini-Setia Effect System REPL"
  , ""
  , "Commands:"
  , "  :help, :?, :h       Show this help message"
  , "  :type <expr>       Show the type of an expression"
  , "  :quit, :q          Exit the REPL"
  , ""
  , "Expression syntax:"
  , "  x                  Variable"
  , "  \\x. e             Lambda abstraction"
  , "  e1 e2              Application"
  , "  let x = e1 in e2   Let binding"
  , "  perform eff arg    Effect operation"
  , "  handle e with h    Effect handler"
  ]

main :: IO ()
main = do
  putStrLn "Welcome to Mini-Setia Effect System REPL!"
  putStrLn "Type :help for help, :quit to exit"
  putStrLn ""
  repl initialContext
