
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
import DTS.Wani.Prove (prove)
import qualified DTS.Wani.WaniBase as B
import qualified DTS.DTT as DT
import qualified DTS.Prover_daido.Judgement as J
import qualified DTS.Wani.Arrowterm as A
import qualified Data.Time as TIME

-- | When Q has proofs, you should set `True` as a first output element. if does not, `False`.
type TestType = 
  Bool -- ^ to debug or not
  -> (Bool,B.Result) -- ^ (predicted,result)

-- | To test with default setting
execute ::  J.TEnv -- ^ con list
  -> A.SUEnv  -- ^ var list
  -> DT.Preterm -- ^ prop
  -> Bool -- ^ to debug or not
  -> B.Result
execute varEnv sigEnv preType b = prove varEnv sigEnv preType B.settingDef{B.debug=b}

executeWithMode :: B.ProofMode
  -> J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> Bool
  -> B.Result
executeWithMode mode varEnv sigEnv preType b = prove varEnv sigEnv preType B.settingDef{B.debug=b}{B.mode = B.WithDNE}

-- | To test with DNE
executeWithDNE ::  J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> Bool
  -> B.Result
executeWithDNE = executeWithMode B.WithDNE

-- | To test with depth tuning
executeWithDepth :: J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> Bool
  -> Int
  -> B.Result
executeWithDepth  = executeWithDepthMode B.Plain

-- | To test with DNE and depth tuning
executeWithDNEDepth :: J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> Bool
  -> Int
  -> B.Result
executeWithDNEDepth  = executeWithDepthMode B.WithDNE

executeWithDepthMode :: B.ProofMode
  -> J.TEnv
  -> A.SUEnv 
  -> DT.Preterm 
  -> Bool
  -> Int
  -> B.Result
executeWithDepthMode mode varEnv sigEnv preType b depth = prove varEnv sigEnv preType B.settingDef{B.debug=b,B.maxdepth=depth,B.mode=mode}

-- | To test time with depth tuning 
executeTimeWithDepth :: TestType
  -> Bool
  -> IO()
executeTimeWithDepth test b = do
  start <- TIME.getCurrentTime
  let (_,r) = test b
  putStrLn (show $ B.trees r)
  end <- TIME.getCurrentTime
  putStrLn (show$TIME.diffUTCTime end start)

-- | constant term, p
p :: DT.Preterm
p = DT.Con "p"

-- | constant term, q
q :: DT.Preterm
q = DT.Con "q"

-- | constant term, r
r :: DT.Preterm
r = DT.Con "r"

-- | constant List
sigEs :: A.SUEnv
sigEs = [("r",DT.Type),("q",DT.Type),("p",DT.Type)]

-- | Used to perform a batch of tests
checkTests :: [TestType] -> Bool 
checkTests ts = and $ map (\f -> let (predicted,result) = f False in predicted == (not $ null $ B.trees result) ) ts
