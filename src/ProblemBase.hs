
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
import qualified Data.Time as TIME
import qualified DTS.QueryTypes as QT
import qualified DTS.UDTTdeBruijn as U
import DTS.Labels (DTT)

type TestType = (Bool,QT.ProofSearchResult) -- ^ (predicted,result)

executeWithDepthMode :: Int
  -> Maybe QT.LogicSystem
  -> QT.ProofSearchQuery
  -> QT.ProofSearchResult
executeWithDepthMode depth mode = prove' QT.ProofSearchSetting{QT.maxDepth=Just depth,QT.maxTime=Just 10000000,QT.system=mode}

executeWithMode :: Maybe QT.LogicSystem
  -> QT.ProofSearchQuery
  -> QT.ProofSearchResult
executeWithMode = executeWithDepthMode 5

-- | To test with DNE
executeWithDNE ::  QT.ProofSearchQuery -> QT.ProofSearchResult
executeWithDNE = executeWithDNEDepth 5

-- -- | To test with default setting
execute :: QT.ProofSearchQuery -> QT.ProofSearchResult
execute = executeWithDNE

-- | To test with depth tuning
executeWithDepth :: Int
  -> QT.ProofSearchQuery
  -> QT.ProofSearchResult
executeWithDepth depth = executeWithDepthMode depth Nothing

-- -- | To test with DNE and depth tuning
executeWithDNEDepth :: Int
  -> QT.ProofSearchQuery
  -> QT.ProofSearchResult
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
p :: U.Preterm DTT
p = U.Con "p"

-- | constant term, q
q :: U.Preterm DTT
q = U.Con "q"

-- | constant term, r
r :: U.Preterm DTT
r = U.Con "r"

-- | constant List
sigEs :: U.Signature
sigEs = [("r",U.Type),("q",U.Type),("p",U.Type)]

-- | Used to perform a batch of tests
checkTests :: [TestType] -> Bool 
checkTests ts = and $ map (\f -> let (predicted,result) = f in predicted == (not $ null $ result) ) ts
