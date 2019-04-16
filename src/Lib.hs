module Lib where

import Control.Applicative hiding (empty)
import Control.Monad
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Expr as E
import Data.Char (chr)

data Regex = Empty
           | Null
           | Ch Char
           | Seq Regex Regex
           | Alt Regex Regex
           | Star Regex
  deriving (Show, Eq)

---------------------------------------------------------
-- YOUR CHANGES START HERE
---------------------------------------------------------

deriv :: Regex -> Char -> Regex
deriv = undefined

nullable :: Regex -> Regex
nullable = undefined

matchString :: Regex -> String -> Bool
matchString = undefined

---------------------------------------------------------
-- YOUR CHANGES END HERE
---------------------------------------------------------

matchLine :: Regex -> String -> Bool
matchLine r = matchString (pad r)
  where
    pad r = Seq (Star anyChar) (Seq r (Star anyChar))

anyChar :: Regex
anyChar = foldr Alt Null (map (Ch . chr) [0..255])

plus r = Seq r (Star r)
option r = Alt Empty r

oneOf "" = Null
oneOf [c] = Ch c
oneOf (c:cs) = Alt (Ch c) (oneOf cs)

term :: P.Parser Regex
term = E.buildExpressionParser ops atom where

  ops = [ [ E.Postfix (Star   <$ P.char '*')
          , E.Postfix (plus   <$ P.char '+')
          , E.Postfix (option <$ P.char '?')
          ]
        , [ E.Infix (return Seq) E.AssocRight
          ]
        , [ E.Infix (Alt <$ (P.char '|')) E.AssocRight
          ]
        ]

  atom = msum [ Ch <$> lit, dot, parens term ]

  dot = (return anyChar) <* P.char '.'

  lit = P.noneOf ".*+?|()"
  parens = P.between (P.char '(') (P.char ')')

parseRegex :: String -> Maybe Regex
parseRegex s = case P.parse term s s of
                Right r -> Just r
                _ -> Nothing
