module Main where

import Data.Csv
import GHC.Generics (Generic)
import Prelude hiding (writeFile)
import Data.ByteString.Lazy (writeFile)

import Logic
import BusyBox
import Solve

data Row = Row { numClauses     :: !Int
               , numVariables   :: !Int
               , numUnitClauses :: !Int
               , wasSat         :: !Bool
               , satTime        :: !Double
               } deriving (Show,Generic)

instance ToField Bool where
  toField True  = "TRUE"
  toField False = "FALSE"

instance ToNamedRecord  Row
instance DefaultOrdered Row

doAnalysis :: Analysis -> IO [Row]
doAnalysis = mapM makeRow . allProblems

makeRow :: Proposition -> IO Row
makeRow p = do let numClauses   = clauseCount   p
                   numVariables = variableCount p
                   numUnitClauses = 0
               (wasSat, satTime) <- solve p
               return $ Row {..}

outputFile :: FilePath
outputFile = "data/busybox_results.csv"

main :: IO ()
main = do as <- getProblems
          results <- mapM doAnalysis as
          let b = encodeDefaultOrderedByName (mconcat results)
          writeFile outputFile b
