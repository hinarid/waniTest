{-# LANGUAGE OverloadedStrings #-}
module Problems.Minimum (
  yes,
  notYes
)
where
import qualified DTS.DTT as DT
import qualified ProblemBase as PB
import qualified DTS.Alligator.AlligatorBase as AB

yes :: [PB.TestType]
yes = [
  testYes]

notYes :: [PB.TestType]
notYes = [
  testNo]

testYes :: PB.TestType
testYes b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") DT.Type),("girl",DT.Pi (DT.Con "entity") DT.Type),("x",DT.Con "entity"),("event",DT.Pi (DT.Con "entity") DT.Type),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.Con "entity") (DT.Pi (DT.App (DT.Con "girl") (DT.Var 0)) (DT.App (DT.Con "man") (DT.Var 1))),DT.App (DT.Con "girl") (DT.Con "x")]
    pre_type = DT.App (DT.Con "man") (DT.Con "x")
  in (True,PB.executeWithDepth  varEnv sigEnv pre_type b 3)

testNo :: PB.TestType
testNo b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") DT.Type),("girl",DT.Pi (DT.Con "entity") DT.Type),("x",DT.Con "entity"),("event",DT.Pi (DT.Con "entity") DT.Type),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.Con "entity") (DT.Pi (DT.App (DT.Con "girl") (DT.Var 0)) (DT.App (DT.Con "man") (DT.Var 1))),DT.App (DT.Con "girl") (DT.Con "x")]
    pre_type = DT.Not $ DT.App (DT.Con "man") (DT.Con "x")
  in (False,PB.executeWithDepth  varEnv sigEnv pre_type b 3)