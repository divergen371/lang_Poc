# Mini-Setia Effect System PoC

行多相代数エフェクト&ハンドラを実装する Haskell プロトタイプです。STLC（Simply Typed Lambda Calculus）ベースのコアを持つエフェクトシステムの研究・実装プロジェクトです。

## 🎯 プロジェクトスコープ

### ✅ 対象

- **行多相代数エフェクト** (State, Exception, Console)
- **エフェクトハンドラ**
- **STLC core** with CPS transformation
- **型推論** (Algorithm M + エフェクト推論)
- **Small-step interpreter**

### ❌ 対象外

- 最適化・パフォーマンス改善
- ガベージコレクション
- マルチバックエンドサポート

## 🏗️ プロジェクト構造

```
app/              Main.hs (REPL)
src/
 ├─ Syntax.hs      -- Expr, Ty, EffRow
 ├─ Kind.hs        -- Row kind system
 ├─ Typecheck/     -- Algorithm M implementation
 │   ├─ Subst.hs   -- Type substitution
 │   ├─ Unify.hs   -- Type unification
 │   └─ Infer.hs   -- Type inference
 ├─ Elaborate.hs   -- Surface → CPS Core (TODO)
 ├─ Core.hs        -- CPS IR with Op / Handle nodes (TODO)
 └─ Eval.hs        -- Small-step handler interpreter (TODO)
test/             QuickCheck & doctest specs (TODO)
```

## 🚀 セットアップ

### 必要環境

- **GHC**: 9.10.x (LTS 推奨) または 9.12.2
- **Cabal**: ≥ 3.10
- **HLS**: GHC バージョンに対応

### インストール手順

```bash
# リポジトリクローン
git clone <repository-url>
cd lang_Poc

# パッケージインデックス更新
cabal update

# ビルド
cabal build

# テスト実行
cabal test

# REPL起動
cabal run effect-poc
```

### 開発環境設定

```bash
# Ormolu (コードフォーマッタ) インストール
cabal install ormolu

# HLS設定は hie.yaml で自動設定済み
```

## 💡 構文例

### 基本構文

```haskell
-- 変数
x
foo
variable_name

-- 括弧による優先順位制御
(x)
(\x. x)
```

### Lambda 式

```haskell
-- 恒等関数
\x. x

-- 関数合成
\f. \g. \x. f (g x)

-- カリー化された加算（概念的）
\x. \y. add x y
```

### 関数アプリケーション

```haskell
-- 単純なアプリケーション
f x

-- 複数引数（左結合）
f x y    -- (f x) y と同じ

-- 括弧による明示的な結合
f (g x)
```

### Let 式

```haskell
-- 基本的なlet束縛
let x = y in x

-- Lambda式の束縛
let id = \x. x in id

-- より複雑な例
let f = \x. \y. x in f a b

-- ネストしたlet式（右結合）
let x = let y = z in y in x

-- 括弧を使った場合
let f = (\x. x) in f y
```

### エフェクト構文（実装予定）

```haskell
-- エフェクト実行
perform State get
perform Console (print "Hello")

-- エフェクトハンドリング
handle (perform State get) with stateHandler
```

### REPL 使用例

```bash
$ cabal run effect-poc
Welcome to Mini-Setia Effect System REPL!
Type :help for help, :quit to exit

effect-poc> let id = \x. x in id
Parsed: Let "id" (Lam "x" (Var "x")) (Var "id")
Type: t0 -[∅]> t0
Value: <evaluation not implemented yet>

effect-poc> \f. \x. f x
Parsed: Lam "f" (Lam "x" (App (Var "f") (Var "x")))
Type: t0 -[∅]> t1 -[∅]> t2
Value: <evaluation not implemented yet>

effect-poc> :type \x. x
Type: t0 -[∅]> t0
```

注記:

- 現在、評価器は未実装です。そのため、REPL での評価結果は常に `Value: <evaluation not implemented yet>` のダミー出力になります（`:type` は型のみを表示）。
- 関数型はエフェクト行付きで表示されます。エフェクトが空の場合は `t1 -[∅]> t2` の形式になります。

### REPL コマンド

| コマンド       | 説明                 |
| -------------- | -------------------- |
| `:help`, `:?`  | ヘルプメッセージ表示 |
| `:type <expr>` | 式の型を表示         |
| `:quit`, `:q`  | REPL 終了            |

## 🛠️ 開発ガイドライン

### コーディング規約

- **モジュール**: PascalCase、ファイルパスと一致必須
- **エフェクト追加**: `Syntax.hs`の`EffLabel`に追加 + `rowUnify`と`Eval`を拡張
- **インポート**: 明示的インポート推奨 (`import Data.Map (Map)`)
- **言語拡張**: 最小限に抑制

### ビルドコマンド

```bash
cabal update          # パッケージインデックス更新
cabal build           # コンパイル
cabal test            # テスト実行
cabal clean           # ビルド成果物削除
```

## 📋 実装状況

### ✅ 完了

- [x] 基本構文定義 (`Syntax.hs`)
- [x] パーサー実装 (`Parser.hs`)
- [x] 型置換システム (`Typecheck/Subst.hs`)
- [x] 基本的な型推論 (`Typecheck/Infer.hs`)
- [x] 型の単一化 (`Typecheck/Unify.hs`)
- [x] カインドシステム基盤 (`Kind.hs`)
- [x] REPL (`app/Main.hs`)

### 🚧 進行中

- [ ] エフェクト行の完全な単一化
- [ ] `EffLabel` データ型の定義
- [ ] 型推論におけるエフェクト処理

### 📝 未着手

- [ ] CPS IR 実装 (`Core.hs`)
- [ ] Surface → Core 変換 (`Elaborate.hs`)
- [ ] エフェクトハンドラ意味論 (`Eval.hs`)
- [ ] エフェクト実行機能

## 🧪 一般的な問題と解決策

| エラーメッセージ             | 解決方法                                          |
| ---------------------------- | ------------------------------------------------- |
| `cannot find source for X`   | `other-modules:` に追加し `hs-source-dirs` を確認 |
| `Unknown/unsupported GHC`    | GHC 9.10 にダウングレードか `allow-newer` 追加    |
| `rowUnify occurs-check loop` | `Row.Unify` の tail-variable ハンドリングを再確認 |
| `Reserved word: in`          | パーサーの予約語処理問題（修正済み）              |

## 📚 今後の TODO

- [ ] エフェクトサブタイピング `{Console|ρ} ⊑ {IO}`
- [ ] ハンドラオーバーヘッドのベンチマーク
- [ ] Core ⟷ 将来のバックエンド用 FlatBuffers スキーマ

## 📄 ライセンス

BSD-3-Clause - 詳細は [LICENSE](LICENSE) を参照

---

_Last updated: 2025-05-29_
_Last updated: 2025-08-09_
