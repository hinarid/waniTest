{-# LANGUAGE OverloadedStrings #-}
module Problems.PlayGround (
  yes
)
where
import qualified DTS.DTTdeBruijn as U
import qualified ProblemBase as PB

yes :: [PB.TestType]
yes = [testYes]

testYes :: PB.TestType
testYes =
  let
    sigEnv = [
                ("taro",U.Entity),
                ("love",U.Pi (U.Entity) (U.Pi U.Entity U.Type))
              ]
    varEnv = [U.Pi (U.Entity) (U.Sigma (U.Entity) (U.App (U.App (U.Con "love") (U.Var 1)) (U.Var 0)))]
    pre_type = U.Sigma (U.Entity) (U.App (U.App (U.Con "love") (U.Con "taro")) (U.Var 0))
  in (True,PB.executeWithDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))