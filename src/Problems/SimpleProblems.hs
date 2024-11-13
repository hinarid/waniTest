{-# LANGUAGE OverloadedStrings #-}
module Problems.SimpleProblems(
  yes,
  notYes
)
where
import qualified DTS.DTTdeBruijn as U
import ProblemBase as PB

yes :: [TestType]
yes = [
  membershipTest1,
  membershipTest2,
  membershipTestForward1,
  membershipTestForward2,
  eqIntroTest1,
  piIntroTest1,
  piIntroTest2,
  piIntroTest3,
  piElimTest1,
  piElimTest2,
  piElimTest3,
  piElimTest4,
  eqElimTest1,
  eqElimTest2,
  eqElimTest3,
  eqElimTest4,
  eqElimTest5,
  transTest1,
  transTest2,
  transTest3,
  transTest4,
  transTest5,
  transTest6,
  transTest7,
  transTest8,
  eqElimTestApp,
  eqElimTestApp2,
  eqElimTestSigma1,
  eqElimTestPi1,
  eqElimTestPi2,
  eqElimTestPi3,
  eqElimTestSigma2,
  dneTest1,
  dneTest2,
  dneTest3,
  dneTest4,
  sigmaIntroTest1,
  sigmaIntroTest2,
  piElimTest5,
  piElimTest6,
  dneTest5]

notYes :: [TestType]
notYes = [
  membershipTestNotFound,
  membershipTestNotFound2,
  membershipTestNotFound3,
  piElimTestNotFound1,
  piElimTestNotFound2,
  eqIntroTestNotFound,
  eqElimTestSigmaNotFound,
  eqElimTestPiNotFound,
  eqElimTestNotFound1,
  false]


membershipTest1 :: TestType
membershipTest1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Con "entity"]
    pre_type = U.Con "entity"
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTest2 :: TestType
membershipTest2 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Con "entity"]
    pre_type = U.Con "entity"
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTestNotFound :: TestType
membershipTestNotFound =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Type]
    pre_type = U.Con "entity"
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTestNotFound2 :: TestType
membershipTestNotFound2 =
  let
    sigEnv = [("q",U.Type),("p",U.Type),("entity",U.Type)]
    varEnv = [U.Con "p"]
    pre_type = U.App (U.Con "q") (U.Con "p")
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTestNotFound3 :: TestType
membershipTestNotFound3 =
  let
    sigEnv = [("q",U.Type),("p",U.Type),("entity",U.Type)]
    varEnv = [U.Var 0,U.Type,U.Type]
    pre_type = U.Var 2
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTestForward1 :: TestType
membershipTestForward1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Sigma (U.Type) $U.Sigma U.Type (U.Con "entity")]
    pre_type = U.Con "entity"
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

membershipTestForward2 :: TestType
membershipTestForward2 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Con "entity") $U.Sigma (U.Con "entity") (U.Type)]
    pre_type = U.Pi (U.Con "entity") U.Type
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

eqIntroTest1 :: TestType
eqIntroTest1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Con "entity",U.Pi (U.Con "entity") $U.Sigma (U.Con "entity") (U.Con "entity")]
    pre_type = U.Eq (U.Con "entity") (U.Var 0) (U.Var 0)
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piIntroTest1 :: TestType
piIntroTest1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Con "entity"]
    pre_type = U.Pi (U.Var 0) (U.Var 1)
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piIntroTest2 :: TestType
piIntroTest2 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Con "entity",U.Type,U.Con "entity"]
    pre_type = U.Pi (U.Var 1) (U.Con "entity")
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piIntroTest3 :: TestType
piIntroTest3 =
  let
    sigEnv = [("event",U.Pi (U.Con "entity") U.Type),("entity",U.Type)]
    varEnv = [U.Con "entity",U.Type,U.Pi (U.Con "entity") U.Type]
    pre_type = U.Pi (U.Var 1) (U.Con "entity")
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest1 :: TestType
piElimTest1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Var 2) (U.Var 1) ,U.Type,U.Var 0,U.Type]
    pre_type = U.Var 2
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest2 :: TestType
piElimTest2 =
  let
    sigEnv = [("r",U.Type),("q",U.Type),("p",U.Type),("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Con "p") (U.Pi (U.Con "q") (U.Con "r")),U.Con "q",U.Con "p"]
    pre_type = U.Con "r"
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest3 :: TestType
piElimTest3 =
  let
    sigEnv = [("r",U.Type),("q",U.Type),("p",U.Type),("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Con "p") (U.Pi (U.Con "p")(U.Pi (U.Con "q") (U.Con "r"))),U.Con "p"]
    pre_type = U.Pi (U.Con "q") (U.Con "r")
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest4 :: TestType
piElimTest4 =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") U.Type),("girl",U.Pi (U.Con "entity") U.Type),("x",U.Con "entity"),("event",U.Pi (U.Con "entity") U.Type),("entity",U.Type)]
    varEnv = [U.Pi (U.Con "entity") (U.Pi (U.App (U.Con "girl") (U.Var 0)) (U.App (U.Con "man") (U.Var 1))),U.App (U.Con "girl") (U.Con "x")]
    pre_type = U.App (U.Con "man") (U.Con "x")
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTestNotFound1 :: TestType
piElimTestNotFound1 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Var 2) (U.Var 1) ,U.Type,U.Var 0,U.Type]
    pre_type = U.Var 0
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTestNotFound2 :: TestType
piElimTestNotFound2 =
  let
    sigEnv = [("entity",U.Type)]
    varEnv = [U.Type,U.Pi (U.Var 0) (U.Var 1) ,U.Type,U.Var 0,U.Type]
    pre_type = U.Var 2
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqIntroTestNotFound :: TestType
eqIntroTestNotFound =
  let
    sigEnv = [("love",U.Pi (U.Con "entity") (U.Pi (U.Con "entity") U.Type)),("man",U.Pi (U.Con "entity") U.Type),("girl",U.Pi (U.Con "entity") U.Type),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = []
    pre_type = U.Eq (U.Type) (U.App (U.App (U.Con "love") (U.Con "y")) (U.Con "x")) (U.App (U.App (U.Con "love") (U.Con "y")) (U.Con "x"))
  in (False,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTest1 :: TestType
eqElimTest1 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Eq (U.Con "entity") (U.Con "x") (U.Con "y"),U.App (U.Con "f") (U.Con "x")]
    pre_type = U.App (U.Con "f") (U.Con "y")
  in (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTest2 :: TestType
eqElimTest2 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.App (U.Con "f") (U.Con "x"),U.Eq (U.Con "entity") (U.Con "x") (U.Var 0),U.Con "entity"]
    pre_type = U.App (U.Con "f") (U.Var 2)
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTest3 :: TestType
eqElimTest3 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.App (U.Con "f") (U.Var 2),U.Eq (U.Con "entity") (U.Var 0) (U.Var 1),U.Con "entity",U.Con "entity"]
    pre_type = U.App (U.Con "f") (U.Var 2)
  in (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTest4 :: TestType
eqElimTest4 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Eq (U.Con "entity") (U.Con "x") (U.Var 0),U.Con "entity",U.App (U.Con "f") (U.Con "x")]
    pre_type = U.App (U.Con "f") (U.Var 1)
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTest5 :: TestType
eqElimTest5 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Eq (U.Con "entity") (U.Var 0) (U.Con "x") ,U.Con "entity",U.App (U.Con "f") (U.Con "x")]
    pre_type = U.App (U.Con "f") (U.Var 1)
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

xy = U.Eq (U.Con "entity") (U.Con"x") (U.Con "y")
yx = U.Eq (U.Con "entity") (U.Con"y") (U.Con "x")
yz = U.Eq (U.Con "entity") (U.Con "y") (U.Con "z")
zy = U.Eq (U.Con "entity") (U.Con "z") (U.Con "y")
xz = U.Eq (U.Con "entity") (U.Con "x") (U.Con "z") 
zx = U.Eq (U.Con "entity") (U.Con "z") (U.Con "x") 

transTest1 :: TestType
transTest1 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [xy,yz]
    pre_type = xz
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest2 :: TestType
transTest2 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [xy,yz]
    pre_type = zx
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest3 :: TestType
transTest3 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [xy,zy]
    pre_type = xz
  in (True,executeWithDNEDepth 2 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest4 :: TestType
transTest4 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [xy,zy]
    pre_type = zx
  in (True,executeWithDNEDepth 2 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest5 :: TestType
transTest5 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [yx,yz]
    pre_type = xz
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest6 :: TestType
transTest6 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [yx,yz]
    pre_type = zx
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest7 :: TestType
transTest7 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [yx,zy]
    pre_type = xz
  in (True,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

transTest8 :: TestType
transTest8 =
  let
    sigEnv = [("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [yx,zy]
    pre_type = zx
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestApp :: TestType
eqElimTestApp =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.App (U.Con "f") (U.Con "y"),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.App (U.Con "f") (U.Con "x")
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestApp2 :: TestType
eqElimTestApp2 =
  let
    sigEnv = [("sister",U.Pi (U.Con "entity") (U.Con "entity")),("family",U.Pi (U.Con "entity") (U.Con "entity")),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.App (U.Con "sister") (U.Con "x"),U.Eq (U.Pi (U.Con "entity") (U.Con "entity")) (U.Con "family") (U.Con "sister")]
    pre_type = U.App (U.Con "family") (U.Con "x")
  in (True,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestSigma1 :: TestType
eqElimTestSigma1 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Sigma (U.App (U.Con "f") (U.Con "x")) (U.App (U.Con "f") (U.Con "x")),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.Sigma (U.App (U.Con "f") (U.Con "x")) (U.App (U.Con "f") (U.Con "y"))
  in (True,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestPi1 :: TestType
eqElimTestPi1 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Pi (U.App (U.Con "f") (U.Con "x")) (U.App (U.Con "f") (U.Con "x")),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.Pi (U.App (U.Con "f") (U.Con "x")) (U.App (U.Con "f") (U.Con "y"))
  in (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestPiNotFound :: TestType
eqElimTestPiNotFound =
  let
    sigEnv = [("z",U.Type),("y",U.Type),("x",U.Type),("entity",U.Type)]
    varEnv = [U.Pi (U.Con "z") (U.Con "x"),U.Eq (U.Type) (U.Con "x") (U.Con "y")]
    pre_type = U.Pi (U.Con "z") (U.Con "y")
  in (False,executeWithDNEDepth 7 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestPi2 :: TestType
eqElimTestPi2 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Pi (U.App (U.Con "f") (U.Con "z")) (U.App (U.Con "f") (U.Con "x")),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.Pi (U.App (U.Con "f") (U.Con "z")) (U.App (U.Con "f") (U.Con "y"))
  in (True,executeWithDNEDepth 5 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestPi3 :: TestType
eqElimTestPi3 =
  let
    sigEnv = [("x",U.Type),("entity",U.Type)]
    varEnv = []
    pre_type = U.Pi (U.Con "x") (U.Con "x")
  in (True,executeWithDNEDepth 7 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestSigmaNotFound :: TestType
eqElimTestSigmaNotFound =
  let
    sigEnv = [("z",U.Type),("y",U.Type),("x",U.Type),("entity",U.Type)]
    varEnv = [U.Sigma (U.Con "x") (U.Pi (U.Con "z") (U.Con "x")),U.Eq (U.Type) (U.Con "x") (U.Con "y")]
    pre_type = U.Sigma (U.Con "x") (U.Pi (U.Con "z") (U.Con "y"))
  in (False,executeWithDNEDepth 7 (U.ProofSearchQuery sigEnv varEnv pre_type))
 
eqElimTestSigma2 :: TestType
eqElimTestSigma2 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("z",U.Con "entity"),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Sigma (U.App (U.Con "f") (U.Con "x")) (U.Pi (U.App (U.Con "f") (U.Con "z")) (U.App (U.Con "f") (U.Con "x"))),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.Sigma (U.App (U.Con "f") (U.Con "x")) (U.Pi (U.App (U.Con "f") (U.Con "z")) (U.App (U.Con "f") (U.Con "y")))
  in (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

eqElimTestNotFound1 :: TestType
eqElimTestNotFound1 =
  let
    sigEnv = [("f",U.Pi (U.Con "entity") U.Type),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Eq (U.Con "entity") (U.Con "x") (U.Var 0),U.Con "entity", U.App (U.Con "f") (U.Con "x")]
    pre_type = U.App (U.Con "f") (U.Con "y")
  in (False,executeWithDNEDepth 4 (U.ProofSearchQuery sigEnv varEnv pre_type))

dneTest1 :: TestType
dneTest1 =
  let
    sigEnv = []
    varEnv = [U.Not $ U.Not (U.Var 0),U.Type]
    pre_type = U.Var 1
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

dneTest2 :: TestType
dneTest2 =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") (U.Type)),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Not $ U.Not (U.App (U.Con "man") (U.Con "y"))]
    pre_type = U.App (U.Con "man") (U.Con "y")
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

dneTest3 :: TestType
dneTest3 =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") (U.Type)),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Not $ U.Not (U.App (U.Con "man") (U.Con "x")),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.App (U.Con "man") (U.Con "x")
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

dneTest4 :: TestType
dneTest4 =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") (U.Type)),("y",U.Con "entity"),("x",U.Con "entity"),("entity",U.Type)]
    varEnv = [U.Not $ U.Not (U.App (U.Con "man") (U.Con "x")),U.Eq (U.Con "entity") (U.Con "x") (U.Con "y")]
    pre_type = U.App (U.Con "man") (U.Con "y")
  in (True,executeWithDNEDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))

sigmaIntroTest1 :: TestType
sigmaIntroTest1 =
  let
    sigEnv = [("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [U.App (U.Con "man") (U.Var 0),U.Con "entity"]
    pre_type = U.Sigma (U.Con "entity") (U.Con "entity")
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

sigmaIntroTest2 :: TestType
sigmaIntroTest2 =
  let
    sigEnv = [("dog",U.Pi (U.Con "entity") (U.Type)),("love",U.Pi (U.Con "entity") (U.Pi (U.Con "entity") U.Type)),("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [U.App (U.App (U.Con "love") (U.Var 3)) (U.Var 1),U.App (U.Con "dog") (U.Var 0),U.Con "entity",U.App (U.Con "man") (U.Var 0),U.Con "entity"]
    pre_type = U.Sigma (U.Con "entity") (U.Sigma (U.Con "entity") (U.Sigma (U.App (U.Con "man") (U.Var 1)) (U.Sigma (U.App (U.App (U.Con "love") (U.Var 2)) (U.Var 1)) (U.App (U.Con "dog") (U.Var 6)))))
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

false :: TestType
false =
  let
    sigEnv = []
    varEnv = [U.Type]
    pre_type =  U.Bot
  in  (False,execute (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest5 :: TestType
piElimTest5 =
  let
    sigEnv = sigEs
    varEnv = [p,U.Pi p q,U.Pi q r]
    pre_type =  r
  in(True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

piElimTest6 :: TestType
piElimTest6 =
  let
    sigEnv = sigEs
    varEnv = [U.Pi (U.Var 2) (U.Var 2),U.Pi (U.Var 2) (U.Var 2),U.Type,U.Type,U.Type]
    pre_type =  U.Pi (U.Var 4) (U.Var 3)
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))

dneTest5 :: TestType
dneTest5 =
  let
    sigEnv = sigEs
    varEnv = [p,U.Pi q r,U.Pi p (U.Pi (U.Pi q U.Bot) U.Bot)]
    pre_type = r
  in (True,executeWithDNE (U.ProofSearchQuery sigEnv varEnv pre_type))
