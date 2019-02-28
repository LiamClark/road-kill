{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveTraversable #-}
{-# Language TypeSynonymInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleInstances #-}
{-# Language InstanceSigs #-}
module Parser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as C

import Data.Functor.Foldable hiding (fold)
import Data.Functor.Classes
import Data.Functor.Compose

import Data.Foldable

import Data.Coerce (coerce)

data AstF a = SExpr [a] | SNum Int | SAtom String | SId String
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Ast = Fix AstF


instance Eq1 AstF where 
  liftEq :: forall a b. (a -> b -> Bool) -> AstF a -> AstF b -> Bool
  liftEq f (SNum i) (SNum i') = i == i'
  liftEq f (SAtom i) (SAtom i') = i == i'
  liftEq f (SId i) (SId i') = i == i'
  liftEq f (SExpr as) (SExpr bs) = and (zipWith f as bs)
  liftEq f _ _ = False

instance Show1 AstF where
  liftShowsPrec f shwlst i s@(SNum n) = (\x -> ("(SNum " ++ show n ++ ")") ++ x) 
  liftShowsPrec f shwlst i s@(SAtom n) = \x -> ("(SAtom " ++ n ++ ")") ++ x
  liftShowsPrec f shwlst i s@(SId n) = \x -> ("(SId " ++ n ++ ")") ++ x
  liftShowsPrec f shwlst i astF@(SExpr n) =   (\x -> "SExpr " ++ x) . shwlst n 

    -- (\x -> "(SExpr " ++ x ++ ")") . fold q 
    where q :: AstF ShowS = f i <$> astF

sexpr = Fix . SExpr
snum = Fix . SNum
satom = Fix . SAtom
sid = Fix . SId

parseSexpr :: Parser Ast
parseSexpr = (string  "(" >> innerList  <*  string ")") <|> num <|> atom <|> id
  where innerList = (sexpr) <$> sepBy1 parseSexpr spaces
        atom = lexeme $ (satom) <$> (char '\'' >> many1 letter_iso8859_15)
        id   = lexeme $ (sid) <$> many1 letter_iso8859_15
        num  = lexeme $ (snum) <$> decimal

spaces = skipMany $ char ' '

lexeme p = spaces >> p <* spaces

junk p = spaces >> p

token p = p <* spaces
