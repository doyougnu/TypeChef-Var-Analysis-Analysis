{-# LANGUAGE DeriveTraversable #-}

module Logic where

import qualified Data.Text as T
import qualified Data.Set  as S

import Data.Foldable (foldr')

-- | notice that you _can only_ create propositions in CNF
data Clause' a = Ref !a
               | Lit Bool
               | Negate (Clause' a)
               | Or [Clause' a]
               deriving (Show,Eq,Ord,Functor,Traversable,Foldable)

newtype Proposition' a = Proposition' { getProp :: [Clause' a] }
                      deriving (Show,Eq,Ord,Functor,Traversable,Foldable)

type Variable    = T.Text
type Clause      = Clause' Variable
type Proposition = Proposition' Variable

clauses :: Proposition -> [Clause]
clauses = getProp

variables :: Proposition -> [Variable]
variables (getProp -> xs) = xs >>= go
  where go (Ref a)    = pure a
        go (Negate a) = go a
        go (Or cs)    = cs >>= go

variableCount :: Proposition -> Int
variableCount = length . variables

clauseCount :: Proposition -> Int
clauseCount = length . clauses

true :: Proposition
true = Proposition' . pure $ Lit True

false :: Proposition
false = Proposition' . pure $ Lit False

clauseSize' :: Clause -> Int
clauseSize' Ref {}     = 1
clauseSize' (Negate e) = clauseSize' e
clauseSize' (Or cs)    = foldr' (\x acc -> clauseSize' x + acc) 0 cs

clauseSize :: Proposition -> S.Set Int
clauseSize (getProp -> p) = S.fromList $ fmap clauseSize' p
