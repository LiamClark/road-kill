{-# Language OverloadedStrings #-}

module ParserTests where

import Test.Hspec
import Parser
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as C

parserTests = describe "first test" $ do
                it "parses parens" $
                  let
                     expected = Just $ sexpr [snum 5]
                  in
                     maybeResult (parse parseSexpr "(5)") `shouldBe` expected
                it "allows nested lists" $
                  let
                     expected = Just $ sexpr [sexpr [snum 5, snum 6], snum 6]
                  in maybeResult (parse parseSexpr "((5 6) 6)") `shouldBe` expected
                it "allows atoms" $
                  let
                     expected = Just $ sexpr [sexpr [snum 5], sexpr [snum 6], satom "hoi"]
                  in maybeResult (parse parseSexpr "((5) (6) 'hoi)") `shouldBe` expected
                it "allows ids" $
                  let
                     expected = Just $ sexpr [sexpr [snum 5], sid "hoi"]
                  in maybeResult (parse parseSexpr "((5) (6) hoi)") `shouldBe` expected

lexerTests = describe "lexer tests" $ do
                it "allows trailing white space" $
                  maybeResult (parse parseSexpr "(5 )") `shouldBe` Just (sexpr [snum 5])
                it "allows heading white space" $
                  maybeResult (parse parseSexpr "( 5)") `shouldBe` Just (sexpr [snum 5])
