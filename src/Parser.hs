module Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Prelude hiding (and, or)
import           Control.Monad (void)
import           Data.Functor ((<$))

import qualified Logic as L

type Parser = Parsec Void T.Text

langParser :: Parser L.Proposition
langParser = between sc eof proposition

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

prefix, postfix :: T.Text -> (L.Clause -> L.Clause) -> Operator Parser L.Clause
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
underscore = void $ symbol "_"

and :: Parser ()
and = void $ symbol "&"

or :: Parser ()
or = void $ symbol "|"

def :: Parser ()
def = reserved "def"

false :: Parser L.Clause
false = L.Lit False <$ reserved "False"

true :: Parser L.Clause
true = L.Lit True <$ reserved "False"

literal :: Parser L.Clause
literal = true <|> false

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

expression :: Parser L.Clause
expression = makeExprParser term ops

term :: Parser L.Clause
term = parens ref
        <|> parens literal
        <|> parens expression
        <|> ref
        <|> expression
        <|> literal

clause :: Parser L.Clause
clause = L.Or <$> term `sepBy` or

proposition :: Parser L.Proposition
proposition = L.Proposition' <$> clause `sepBy` and

-- TODO probably going to be a performance hit
ref :: Parser L.Clause
ref = do def; parens ref_

ref_ :: Parser L.Clause
ref_ = L.Ref . mconcat <$> sepBy (T.pack <$> many alphaNumChar) underscore

ops :: [[Operator Parser L.Clause]]
ops = [ [ prefix "!" L.Negate]
      ]
