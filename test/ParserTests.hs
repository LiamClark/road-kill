{-# Language OverloadedStrings #-}

module ParserTests where

import Test.Hspec
import Parser
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as C

parserTests = describe "first test" $ do
                it "parses parens" $
                  let
                     expected = Just $ SExpr [SNum 5]
                  in
                     maybeResult (parse parseSexpr "(5)") `shouldBe` expected
                it "allows nested lists" $
                  let
                     expected = Just $ SExpr [SExpr [SNum 5, SNum 6], SNum 6]
                  in maybeResult (parse parseSexpr "((5 6) 6)") `shouldBe` expected
                it "allows atoms" $
                  let
                     expected = Just $ SExpr [SExpr [SNum 5], SAtom "hoi"]
                  in maybeResult (parse parseSexpr "((5) 'hoi)") `shouldBe` expected
                it "allows ids" $
                  let
                     expected = Just $ SExpr [SExpr [SNum 5], SId "hoi"]
                  in maybeResult (parse parseSexpr "((5) hoi)") `shouldBe` expected

lexerTests = describe "lexer tests" $ do
                it "allows trailing white space" $
                  maybeResult (parse parseSexpr "(5 )") `shouldBe` Just (SExpr [SNum 5])
                it "allows heading white space" $
                  maybeResult (parse parseSexpr "( 5)") `shouldBe` Just (SExpr [SNum 5])
