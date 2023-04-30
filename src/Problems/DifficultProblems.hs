{-# LANGUAGE OverloadedStrings #-}
module Problems.DifficultProblems(
  yes,
  notYes
)
 where
import qualified DTS.DTT as DT
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
  gem,
  cm,
  con1,
  con2,
  con3,
  con4,
  mp,
  s,
  k]

notYes :: [TestType]
notYes = []

choiceTest ::TestType
choiceTest b=
  let
    sigEnv = [("C",DT.Pi (DT.Con "A") (DT.Pi (DT.App (DT.Con "B") (DT.Var 0)) DT.Type)),("B",DT.Pi (DT.Con "A") DT.Type),("A",DT.Type)]
    varEnv = []
    pre_type = 
      DT.Pi 
        (DT.Pi (DT.Con "A") (DT.Sigma (DT.App (DT.Con "B") (DT.Var 0)) (DT.App (DT.App (DT.Con "C") (DT.Var 1)) (DT.Var 0)))) 
        (DT.Sigma (DT.Pi (DT.Con "A") (DT.App (DT.Con "B") (DT.Var 0))) (DT.Pi (DT.Con "A") (DT.App (DT.App (DT.Con "C") (DT.Var 0)) (DT.App (DT.Var 1) (DT.Var 0)))))
  in (True,execute varEnv sigEnv pre_type b)

dniTest:: TestType
dniTest b=
  let
    sigEnv = [("a",DT.Type)]
    varEnv = [DT.Con "a"]
    pre_type = DT.Not $DT.Not (DT.Con "a")
  in (True,executeWithDNE varEnv sigEnv pre_type b)

dniTest2 :: TestType
dniTest2 b =
  let sigEnv = []
      varEnv = [DT.Type]
      pre_type =  DT.Pi (DT.Var 0) (DT.Not (DT.Not (DT.Var 1)))
  in (True,executeWithDNE varEnv sigEnv pre_type b)

transitive :: TestType
transitive b=
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi p q) (DT.Pi (DT.Pi q r) (DT.Pi p r))
  in (True,executeWithDNE varEnv sigEnv pre_type b)

piElimTest6 :: TestType
piElimTest6 b =
  let
    sigEnv = sigEs
    varEnv = [DT.Pi p q,DT.Pi q r]
    pre_type =  DT.Pi p r
  in (True,executeWithDNE varEnv sigEnv pre_type b)

efq :: TestType
efq =
  let sigEnv = []
      varEnv = [DT.Type, DT.Bot]
      pre_type = DT.Var 0
  in \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

lem :: TestType
lem =
  let
    sigEnv = sigEs
    varEnv = [DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not $ DT.Var 0) (DT.Not (DT.Not (DT.Var 1))))
  in \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

tnd :: TestType
tnd =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi p q) (DT.Pi (DT.Pi (DT.Not p) q) q)
  in \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

pars :: TestType -- depth 9以上
pars =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi (DT.Pi p q) p) p
  in \b -> (True,executeWithDNEDepth varEnv sigEnv pre_type b 10)

gem :: TestType
gem =
  let
    sigEnv = sigEs
    varEnv = [DT.Type,DT.Type]
    pre_type = DT.Not (DT.Sigma (DT.Not (DT.Pi p q)) (DT.Not p))
  in \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

cm :: TestType
cm =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type =  DT.Pi (DT.Pi p (DT.Pi p DT.Bot)) (DT.Pi p DT.Bot)
  in \b ->  (True,execute varEnv sigEnv pre_type b)


con1 :: TestType
con1 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi p q) (DT.Pi (DT.Not q) (DT.Not p))
  in   \b ->  (True,execute varEnv sigEnv pre_type b)

con2 :: TestType
con2 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi p (DT.Not q)) (DT.Pi q (DT.Not p))
  in   \b ->  (True,execute varEnv sigEnv pre_type b)

con3 :: TestType
con3 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi (DT.Not q)  p) (DT.Pi (DT.Not p) q)
  in    \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

con4 :: TestType
con4 =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi (DT.Not p)  (DT.Not q)) (DT.Pi q p)
  in   \b -> (True,executeWithDNE varEnv sigEnv pre_type b)

mp :: TestType
mp =
  let
    sigEnv = sigEs
    varEnv = [DT.Pi p q, p]
    pre_type = q
  in   \b ->  (True,execute varEnv sigEnv pre_type b)

s :: TestType
s =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type = DT.Pi (DT.Pi p (DT.Pi q r)) (DT.Pi (DT.Pi p q) (DT.Pi p r))
  in   \b ->  (True,execute varEnv sigEnv pre_type b)

k :: TestType
k =
  let
    sigEnv = sigEs
    varEnv = []
    pre_type =DT.Pi p (DT.Pi q p)
  in   \b ->  (True,execute varEnv sigEnv pre_type b)
