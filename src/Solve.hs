module Solve where

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
import System.CPUTime (getCPUTime)

import Logic
import BusyBox


type Cache = St.StateT (M.Map T.Text S.SBool) S.Symbolic

new :: T.Text -> Cache S.SBool
new v = do s <- St.get
           case M.lookup v s of
            Nothing -> do sym <- St.lift $ S.sBool $ T.unpack v
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

type IsSatisfiable = Bool
type ElapsedTime = Double

time :: IO t -> IO (t, ElapsedTime)
time a = do
    !start <- getCPUTime
    !v <- a
    !end   <- getCPUTime
    let diff :: Double
        diff = fromIntegral (end - start) / (10^12)
    return (v, diff)


solve :: Proposition -> IO (IsSatisfiable, ElapsedTime)
solve p = time $ S.isSatisfiable $ St.evalStateT (eval p) mempty >>= S.constrain
