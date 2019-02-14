{-# Language OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as C

data Ast = SExpr [Ast] | SNum Int | SAtom String | SId String
  deriving (Show, Eq)

parseSexpr :: Parser Ast
parseSexpr = (string  "(" >> innerList  <*  string ")") <|> num <|> atom <|> id
  where innerList = SExpr <$> sepBy1 parseSexpr spaces
        atom = lexeme $ SAtom <$> (char '\'' >> many1 letter_iso8859_15)
        id   = lexeme $ SId <$> many1 letter_iso8859_15
        num  = lexeme $ SNum <$> decimal

spaces = skipMany $ char ' '

lexeme p = spaces >> p <* spaces

junk p = spaces >> p

token p = p <* spaces
