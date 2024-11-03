
{-|
  Module : ProblemBase
  Library for testing wani

  = When adding new Q ...
  * Create a new file from minimum.hs
  * Create functions for the assumed Qs
  * Add the created function to the `yes` or `notYes` array
  * Import the new file to Main.hs
  * Add `yes` and `notYes` to Main.test
-}
{-# LANGUAGE OverloadedStrings #-}
module ProblemBase(
  -- * Types
  TestType,
  -- * Sigs
  p,
  q,
  r,
  sigEs,
  -- * Functions
  execute,
  executeWithDNE,
  executeWithDepth,
  executeWithDNEDepth,
  executeTimeWithDepth,
  -- * Test
  checkTests
)
 where
import DTS.Prover.Wani.Prove (prove')
import Control.Monad (forM)
import qualified Data.Time as TIME
import ListT (ListT,null)               --list-t
import qualified Interface.Tree as I
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U

type ProofSearchResult = ListT IO (I.Tree QT.DTTrule (U.Judgment))
type TestType = (Bool,ProofSearchResult) -- ^ (predicted,result)

executeWithDepthMode :: Int
  -> Maybe QT.LogicSystem
  -> U.ProofSearchQuery
  -> ProofSearchResult
executeWithDepthMode depth mode = prove' QT.ProofSearchSetting{QT.maxDepth=Just depth,QT.maxTime=Just 10000000,QT.logicSystem=mode}

executeWithMode :: Maybe QT.LogicSystem
  -> U.ProofSearchQuery
  -> ProofSearchResult
executeWithMode = executeWithDepthMode 5

-- | To test with DNE
executeWithDNE ::  U.ProofSearchQuery -> ProofSearchResult
executeWithDNE = executeWithDNEDepth 5

-- -- | To test with default setting
execute :: U.ProofSearchQuery -> ProofSearchResult
execute = executeWithDNE

-- | To test with depth tuning
executeWithDepth :: Int
  -> U.ProofSearchQuery
  -> ProofSearchResult
executeWithDepth depth = executeWithDepthMode depth Nothing

-- -- | To test with DNE and depth tuning
executeWithDNEDepth :: Int
  -> U.ProofSearchQuery
  -> ProofSearchResult
executeWithDNEDepth depth = executeWithDepthMode depth (Just QT.Classical)

-- | To test time with depth tuning 
executeTimeWithDepth :: TestType
  -> IO()
executeTimeWithDepth test = do
  start <- TIME.getCurrentTime
  let (_,r) = test
  putStrLn "a" -- (show r) -- No instance for (Show (U.Judgment DTS.Labels.DTT))
  end <- TIME.getCurrentTime
  putStrLn (show$TIME.diffUTCTime end start)

-- -- | constant term, p
p :: U.Preterm
p = U.Con "p"

-- | constant term, q
q :: U.Preterm
q = U.Con "q"

-- | constant term, r
r :: U.Preterm
r = U.Con "r"

-- | constant List
sigEs :: U.Signature
sigEs = [("r",U.Type),("q",U.Type),("p",U.Type)]

-- | Used to perform a batch of tests
checkTests :: [TestType] -> IO Bool 
checkTests ts = do
  results <- forM ts $ \(predicted,result) -> do
    isNull <- ListT.null result
    return $ predicted == not isNull
  return $ and results
