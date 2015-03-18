{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Not my own version :(
-- copied from: http://stackoverflow.com/questions/16006279/how-do-you-chain-an-arbitrarily-long-series-of-atomic-parsers-using-applicatives
zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p  = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore(satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- parseSExpr :: Parser SExpr
parseAtom = spaces *> (N <$> posInt) <* spaces <|> 
            spaces *> (I <$> ident)  <* spaces

openPar = char '('
closePar = char ')'

f = (\x -> Comb [A x]) <$> parseAtom
f' = openPar *> oneOrMore f <* closePar
-- parseSExpr' = (\x -> Comb [A x]) <$> (openPar *> oneOrMore parseAtom  <* closePar)

parseSExpr = A <$> parseAtom <|>
           (\x -> Comb [x]) <$> (openPar *> parseSExpr  <* closePar)


