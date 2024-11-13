{-# LANGUAGE OverloadedStrings #-}
module Problems.DifficultProblems(
  yes,
  notYes
)
 where
import qualified DTS.DTTdeBruijn as U
import ProblemBase as PB

yes :: [TestType]
yes = [
  choiceTest,
  dniTest,
  dniTest2,
  transitive,
  efq,
  lem,
  tnd,
  pars,
  parsEfq,
  gem,
  gemEfq,
  cm,
  con1,
  con2,
  con3,
  con4,
  mp,
  s,
  k
  ]

notYes :: [TestType]
notYes = []

choiceTest ::TestType
choiceTest =
  let
    sigEnv = [("C",U.Pi (U.Con "A") (U.Pi (U.App (U.Con "B") (U.Var 0)) U.Type)),("B",U.Pi (U.Con "A") U.Type),("A",U.Type)]
    varEnv = []
    pre_type = 
      U.Pi 
        (U.Pi (U.Con "A") (U.Sigma (U.App (U.Con "B") (U.Var 0)) (U.App (U.App (U.Con "C") (U.Var 1)) (U.Var 0)))) 
        (U.Sigma (U.Pi (U.Con "A") (U.App (U.Con "B") (U.Var 0))) (U.Pi (U.Con "A") (U.App (U.App (U.Con "C") (U.Var 0)) (U.App (U.Var 1) (U.Var 0)))))
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

dniTest:: TestType
dniTest =
  let
    sigEnv = [("a",U.Type)]
    varEnv = [U.Con "a"]
    pre_type = U.Not $U.Not (U.Con "a")
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

dniTest2 :: TestType
dniTest2 =
  let sigEnv = []
      varEnv = [U.Type]
      pre_type =  U.Pi (U.Var 0) (U.Not (U.Not (U.Var 1)))
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

transitive :: TestType
transitive =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi p q) (U.Pi (U.Pi q r) (U.Pi p r))
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest6 :: TestType
piElimTest6 =
  let
    sigEnv = sigEs
    varEnv = [U.Pi p q,U.Pi q r]
    pre_type =  U.Pi p r
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

efq :: TestType
efq =
  let sigEnv = []
      varEnv = [U.Type, U.Bot]
      pre_type = U.Var 0
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

lem :: TestType
lem =
  let
    sigEnv = sigEs
    varEnv = [U.Type]
    pre_type = U.Not (U.Sigma (U.Not $ U.Var 0) (U.Not (U.Not (U.Var 1))))
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

tnd :: TestType
tnd =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi p q) (U.Pi (U.Pi (U.Not p) q) q)
  in (True,executeWithDNEDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

pars :: TestType -- depth 9以上
pars =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi (U.Pi p q) p) p
  in (True,executeWithDNEDepth 12 (U.ProofSearchQuery sigEnv varEnv pre_type))

parsEfq :: TestType -- depth 9以上
parsEfq =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi (U.Pi p q) p) p
  in (False,executeWithEFQDepth 20 (U.ProofSearchQuery sigEnv varEnv pre_type))


gem :: TestType
gem =
  let
    sigEnv = sigEs
    varEnv = [U.Type,U.Type]
    pre_type = U.Not (U.Sigma (U.Not (U.Pi p q)) (U.Not p))
  in (True,executeWithDNEDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

gemEfq :: TestType
gemEfq =
  let
    sigEnv = sigEs
    varEnv = [U.Type,U.Type]
    pre_type = U.Not (U.Sigma (U.Not (U.Pi p q)) (U.Not p))
  in (True,executeWithEFQDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))


cm :: TestType
cm =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type =  U.Pi (U.Pi p (U.Pi p U.Bot)) (U.Pi p U.Bot)
  in  (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))


con1 :: TestType
con1 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi p q) (U.Pi (U.Not q) (U.Not p))
  in    (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

con2 :: TestType
con2 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi p (U.Not q)) (U.Pi q (U.Not p))
  in    (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

con3 :: TestType
con3 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi (U.Not q)  p) (U.Pi (U.Not p) q)
  in    (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

con4 :: TestType
con4 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi (U.Not p)  (U.Not q)) (U.Pi q p)
  in   (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

mp :: TestType
mp =
  let
    sigEnv = sigEs
    varEnv = [U.Pi p q, p]
    pre_type = q
  in    (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

s :: TestType
s =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = U.Pi (U.Pi p (U.Pi q r)) (U.Pi (U.Pi p q) (U.Pi p r))
  in    (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

k :: TestType
k =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type =U.Pi p (U.Pi q p)
  in    (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))
