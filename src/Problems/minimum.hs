{-# LANGUAGE OverloadedStrings #-}
module Problems.Minimum (
  yes,
  notYes
)
where
import qualified DTS.DTTdeBruijn as U
import qualified ProblemBase as PB
import qualified DTS.Prover.Wani.WaniBase as B

yes :: [PB.TestType]
yes = [testYes]

notYes :: [PB.TestType]
notYes = [testNo]

testYes :: PB.TestType
testYes =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") U.Type),("girl",U.Pi (U.Con "entity") U.Type),("x",U.Con "entity"),("event",U.Pi (U.Con "entity") U.Type),("entity",U.Type)]
    varEnv = [U.Pi (U.Con "entity") (U.Pi (U.App (U.Con "girl") (U.Var 0)) (U.App (U.Con "man") (U.Var 1))),U.App (U.Con "girl") (U.Con "x")]
    pre_type = U.App (U.Con "man") (U.Con "x")
  in (True,PB.executeWithDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

testNo :: PB.TestType
testNo = 
  let
    sigEnv = [("man",U.Pi (U.Con "entity") U.Type),("girl",U.Pi (U.Con "entity") U.Type),("x",U.Con "entity"),("event",U.Pi (U.Con "entity") U.Type),("entity",U.Type)]
    varEnv = [U.Pi (U.Con "entity") (U.Pi (U.App (U.Con "girl") (U.Var 0)) (U.App (U.Con "man") (U.Var 1))),U.App (U.Con "girl") (U.Con "x")]
    pre_type = U.Not $ U.App (U.Con "man") (U.Con "x")
  in (False,PB.executeWithDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))