module BusyBox where

import qualified Data.Map.Strict   as M
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Control.Exception as E
import qualified System.Directory  as D

import Text.Megaparsec (parse)
import Data.Either     (lefts, rights)

import Logic
import qualified Parser as P


newtype Directory = Directory { unDir :: FilePath }
  deriving (Eq, Show)

-- some constants
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b

isDirPlain :: Directory -> Bool
isDirPlain = (==) plainDir

plainDir = Directory "plain"
prefix = "SAT_problems_"
suffix = ".txt"
mkFileConst a = prefix <> a <> suffix

featureModelLbl = "FEATURE_MODEL.txt"
parseProblems   = mkFileConst "PARSING"
lexingProblems  = mkFileConst "LEXING"
tcProblems      = mkFileConst "TYPE_CHECKING"
noModeProblems  = mkFileConst "NO_MODE"

newtype Analysis = Analysis { getAnalysis :: M.Map QueryMode [Proposition] }
  deriving (Eq,Ord,Show)

instance Semigroup Analysis where
  (getAnalysis -> a) <> (getAnalysis -> b) = Analysis $! M.unionWith (<>) a b
instance Monoid Analysis where mempty = Analysis M.empty

data QueryMode = NoMode
               | FeatureModel
               | Lexing
               | Parsing
               | TypeChecking
               deriving (Eq,Ord,Show)

-- | don't feel like making the correct semigorup and monoid instances for maybe
-- here
get :: QueryMode -> Analysis -> [Proposition]
get m ( getAnalysis -> a) = case m `M.lookup` a of
                              Nothing -> mempty
                              Just xs -> xs


featureModel :: Analysis -> Proposition
featureModel a = case get FeatureModel a of
                   [] -> true
                   xs -> head xs

lexing :: Analysis -> [Proposition]
lexing = get Lexing

parsing :: Analysis -> [Proposition]
parsing = get Parsing

typeChecking :: Analysis -> [Proposition]
typeChecking = get TypeChecking

noMode :: Analysis -> [Proposition]
noMode = get NoMode

dataFiles :: IO [Directory]
dataFiles = fmap (Directory . (home </>)) <$> D.listDirectory home
  where home = "data/busybox/sat_queries"
  -- where home = "/home/doyougnu/research/TypeChef-BusyboxAnalysis/sat_queries"

readPropFile :: FilePath -> IO [Proposition]
readPropFile f = do txtProblems <- T.lines <$> TIO.readFile f
                    let problems' = parse P.langParser "" <$> txtProblems
                    return $! rights problems'

readFM :: Directory -> IO Proposition
readFM (unDir -> d) = go `E.catch`
                      \e -> print (e :: E.IOException) >> return true
  where go = readPropFile (d </> featureModelLbl) >>= \c ->
          return $ case c of
            [] -> true
            xs -> head xs  -- then we found the feature model

readParseProblems :: Directory -> IO [Proposition]
readParseProblems (unDir -> d) = readPropFile (d </> parseProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readLexingProblems :: Directory -> IO [Proposition]
readLexingProblems (unDir -> d) = readPropFile (d </> lexingProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readTcProblems :: Directory -> IO [Proposition]
readTcProblems (unDir -> d) = readPropFile (d </> tcProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readNoModeProblems :: Directory -> IO [Proposition]
readNoModeProblems (unDir -> d) = readPropFile (d </> noModeProblems)
                     `E.catch` \e -> print (e :: E.IOException) >> return []

-- | because we set the feature model mode initially all queries will be
-- captured as a feature model if no feature model exists. We combine this with
-- the no mode queries which is the only other mode that plain queries will be
-- found in.
handlePlain :: Directory -> IO [Proposition]
handlePlain dir = do fs <- fm
                     ns <- nm
                     return (fs ++ ns) `E.catch`
                       \e -> print (e :: E.IOException) >> return []
  where
    d  = unDir dir
    fm = readPropFile (d </> featureModelLbl)
    nm = readNoModeProblems dir

findPlain :: [Analysis] -> Analysis
findPlain xs = go
  where isAnaPlain (getAnalysis -> a) = M.member FeatureModel a
                                        && M.member    NoMode a
                                        && M.notMember TypeChecking a
                                        && M.notMember Lexing a
                                        && M.notMember Parsing a
        go = case filter isAnaPlain xs of
               []    -> mempty
               (x:_) -> x

mkAnalysis :: Directory -> IO Analysis
mkAnalysis d = do putStrLn $ "Reading: " ++ unDir d
                  if isDirPlain d

                    then do qs <- handlePlain d
                            return $! Analysis $! M.singleton NoMode qs

                    else do fm <- M.singleton FeatureModel <$> readFM             d
                            putStrLn "Got fm"
                            pp <- M.singleton Parsing      <$> readParseProblems  d
                            putStrLn "Got parsing"
                            lp <- M.singleton Lexing       <$> readLexingProblems d
                            putStrLn "Got lexing"
                            tp <- M.singleton TypeChecking <$> readTcProblems     d
                            putStrLn "Got type checking"
                            np <- M.singleton NoMode       <$> readNoModeProblems d
                            putStrLn "Got no mode"
                            return $ Analysis $! mconcat [pp,lp,tp,np]

getProblems :: IO [Analysis]
getProblems = dataFiles >>= mapM mkAnalysis . take 2
