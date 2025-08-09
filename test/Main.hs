module Main where

import Test.Hspec
import qualified Test.Kind as Kind
import qualified Test.Parser as Parser
import qualified Test.Syntax as Syntax
import qualified Test.Typecheck.Infer as Infer
import qualified Test.Typecheck.Subst as Subst
import qualified Test.Typecheck.Unify as Unify

main :: IO ()
main = hspec $ do
  describe "Syntax" Syntax.spec
  describe "Parser" Parser.spec
  describe "Kind System" Kind.spec
  describe "Type Substitution" Subst.spec
  describe "Type Unification" Unify.spec
  describe "Type Inference" Infer.spec