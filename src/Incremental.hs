module Incremental where

import           Data.Foldable           (foldr')
import qualified Control.Monad.State.Strict as St
import qualified Data.List               as L
import qualified Data.Map.Strict         as M
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import Numeric
import Data.Char (intToDigit)

import Logic
import BusyBox


type Cache = St.State (M.Map T.Text S.SBool)

new :: T.Text -> Cache S.SBool
new v = do s <- St.get
           case M.lookup v s of
            Nothing -> do sym <- S.sBool $ T.unpack v
                          St.modify (M.insert v sym)
                          return sym
            Just s  -> return s

evalC :: Clause -> Cache S.SBool
evalC (Lit True)  = return S.sTrue
evalC (Lit False) = return S.sFalse
evalC (Ref b)     = new b
evalC (Negate e)  = S.sNot <$> evalC e
evalC (Or xs)     = S.sOr  <$> mapM evalC xs

eval :: Proposition -> Cache S.SBool
eval = fmap S.sAnd . mapM evalC . getProp

constructIncremental :: [Analysis] -> IO [S.SatResult]
constructIncremental xs = S.runSMT $ do
  let analysisToIncremental (getAnalysis -> a) = Analysis <$> mapM eval a

      symbolicAnalyses :: S.Symbolic [Analysis]
      symbolicAnalyses = St.evalStateT (mapM analysisToIncremental xs) mempty

      doAnalysis analysis = do
        let !fm            = featureModel analysis
            !nM            = noMode analysis
            !lexProblems   = lexing analysis
            !parseProblems = parsing analysis
            !tcProblems    = typeChecking analysis

            runQuery !qry  = SC.inNewAssertionStack $! do
              S.constrain $! eval qry
              S.SatResult <$> SC.getSMTResult

        S.constrain (eval fm)
        mapM_ (S.constrain . eval) nM
        !lexResults   <- mapM runQuery lexProblems
        !parseResults <- mapM runQuery parseProblems
        !tcResults    <- mapM runQuery tcProblems
        return $ lexResults <> parseResults <> tcResults

  -- make all variables known to sbv
  s' <- symbolicAnalyses

  -- find the plain stuff and pack the solver
  let plainAnalysis = findPlain s'

  -- constrain the plain stuff
  S.constrain $ eval $ featureModel plainAnalysis
  mapM_ (S.constrain . eval) $ noMode plainAnalysis
  mapM_ (S.constrain . eval) $ lexing plainAnalysis
  mapM_ (S.constrain . eval) $ parsing plainAnalysis
  mapM_ (S.constrain . eval) $ typeChecking plainAnalysis

  -- off we go
  SC.query $ mapM doAnalysis $ filter (/= plainAnalysis) s'
