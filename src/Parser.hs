{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- レキサー
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- 予約語
reserved :: [String]
reserved = ["let", "in", "perform", "handle", "with"]

-- キーワードパーサー
keyword :: String -> Parser String
keyword w = lexeme $ do
  s <- string w
  notFollowedBy alphaNumChar
  return s

-- 識別子（予約語をチェック）
identifier :: Parser String
identifier = lexeme $ do
  name <- do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    return (first : rest)
  if name `elem` reserved
    then fail $ "Reserved word: " ++ name
    else return name

-- 予約語でないことをチェックする識別子パーサー
safeIdentifier :: Parser String
safeIdentifier = try $ do
  -- 先読みで予約語をチェック
  lookAhead $ do
    name <- do
      first <- letterChar
      rest <- many (alphaNumChar <|> char '_')
      return (first : rest)
    if name `elem` reserved
      then fail "reserved word"
      else return name
  -- 実際に識別子をパース
  identifier

-- 括弧
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- 式のパーサー（最上位）
pExpr :: Parser Expr
pExpr = try pLet <|> pNonLet

-- let式（右結合）
pLet :: Parser Expr
pLet = do
  _ <- keyword "let"
  x <- identifier
  _ <- symbol "="
  e1 <- try pLet <|> pNonLet -- letの右辺でもlet式を許可（tryでバックトラッキング）
  _ <- keyword "in"
  e2 <- pExpr -- inの後は完全な式を許可
  return $ Let x e1 e2

-- lambda式レベル
pLambdaLevel :: Parser Expr
pLambdaLevel = pLambda <|> pAppLevel

-- アプリケーションレベル
pAppLevel :: Parser Expr
pAppLevel = pApp

-- non-let式（letを除く全ての式）
pNonLet :: Parser Expr
pNonLet = pLambda <|> pApp

-- lambda式（let次に低優先度）
pLambda :: Parser Expr
pLambda = do
  _ <- symbol "\\" <|> symbol "λ"
  x <- identifier
  _ <- symbol "."
  body <- pNonLet -- lambda本体はnon-let式
  return $ Lam x body

-- perform式
pPerform :: Parser Expr
pPerform = do
  _ <- keyword "perform"
  eff <- identifier
  arg <- pAtom
  return $ Perform eff arg

-- handle式
pHandle :: Parser Expr
pHandle = do
  _ <- keyword "handle"
  expr <- pApp -- handleの第一引数はアプリケーションレベル
  _ <- keyword "with"
  handler <- pExpr -- withの後は再帰的にpExpr
  return $ Handle expr handler

-- アプリケーション（高優先度）
pApp :: Parser Expr
pApp =
  choice
    [ try pPerform,
      try pHandle,
      pAppChain
    ]

pAppChain :: Parser Expr
pAppChain = do
  terms <- some pAtom
  return $ foldl1 App terms

pAtom :: Parser Expr
pAtom =
  choice
    [ Var <$> safeIdentifier, -- 予約語をチェックする識別子を使用
      parens pExpr
    ]

-- パース関数
parseExpr :: String -> Either String Expr
parseExpr input = case parse (spaceConsumer *> pExpr <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right result -> Right result