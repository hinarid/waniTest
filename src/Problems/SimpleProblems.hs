{-# LANGUAGE OverloadedStrings #-}
module Problems.SimpleProblems(
  yes,
  notYes
)
where
import qualified DTS.DTT as DT
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
membershipTest1 b=
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Con "entity"]
    pre_type = DT.Con "entity"
  in (True,execute varEnv sigEnv pre_type b)

membershipTest2 :: TestType
membershipTest2 b=
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Con "entity"]
    pre_type = DT.Con "entity"
  in (True,execute varEnv sigEnv pre_type b)

membershipTestNotFound :: TestType
membershipTestNotFound b=
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Type]
    pre_type = DT.Con "entity"
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

membershipTestNotFound2 :: TestType
membershipTestNotFound2 b=
  let
    sigEnv = [("q",DT.Type),("p",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Con "p"]
    pre_type = DT.App (DT.Con "q") (DT.Con "p")
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

membershipTestNotFound3 :: TestType
membershipTestNotFound3 b=
  let
    sigEnv = [("q",DT.Type),("p",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Var 0,DT.Type,DT.Type]
    pre_type = DT.Var 2
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

membershipTestForward1 :: TestType
membershipTestForward1 b=
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Sigma (DT.Type) $DT.Sigma DT.Type (DT.Con "entity")]
    pre_type = DT.Con "entity"
  in (True,execute varEnv sigEnv pre_type b)

membershipTestForward2 :: TestType
membershipTestForward2 b=
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Con "entity") $DT.Sigma (DT.Con "entity") (DT.Type)]
    pre_type = DT.Pi (DT.Con "entity") DT.Type
  in (True,execute varEnv sigEnv pre_type b)

eqIntroTest1 :: TestType
eqIntroTest1 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Con "entity",DT.Pi (DT.Con "entity") $DT.Sigma (DT.Con "entity") (DT.Con "entity")]
    pre_type = DT.Eq (DT.Con "entity") (DT.Var 0) (DT.Var 0)
  in (True,execute varEnv sigEnv pre_type b)

piIntroTest1 :: TestType
piIntroTest1 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Con "entity"]
    pre_type = DT.Pi (DT.Var 0) (DT.Var 1)
  in (True,execute varEnv sigEnv pre_type b)

piIntroTest2 :: TestType
piIntroTest2 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Con "entity",DT.Type,DT.Con "entity"]
    pre_type = DT.Pi (DT.Var 1) (DT.Con "entity")
  in (True,execute varEnv sigEnv pre_type b)

piIntroTest3 :: TestType
piIntroTest3 b =
  let
    sigEnv = [("event",DT.Pi (DT.Con "entity") DT.Type),("entity",DT.Type)]
    varEnv = [DT.Con "entity",DT.Type,DT.Pi (DT.Con "entity") DT.Type]
    pre_type = DT.Pi (DT.Var 1) (DT.Con "entity")
  in (True,execute varEnv sigEnv pre_type b)

piElimTest1 :: TestType
piElimTest1 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Var 2) (DT.Var 1) ,DT.Type,DT.Var 0,DT.Type]
    pre_type = DT.Var 2
  in (True,execute varEnv sigEnv pre_type b)

piElimTest2 :: TestType
piElimTest2 b =
  let
    sigEnv = [("r",DT.Type),("q",DT.Type),("p",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Con "p") (DT.Pi (DT.Con "q") (DT.Con "r")),DT.Con "q",DT.Con "p"]
    pre_type = DT.Con "r"
  in (True,execute varEnv sigEnv pre_type b)

piElimTest3 :: TestType
piElimTest3 b =
  let
    sigEnv = [("r",DT.Type),("q",DT.Type),("p",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Con "p") (DT.Pi (DT.Con "p")(DT.Pi (DT.Con "q") (DT.Con "r"))),DT.Con "p"]
    pre_type = DT.Pi (DT.Con "q") (DT.Con "r")
  in (True,execute varEnv sigEnv pre_type b)

piElimTest4 :: TestType
piElimTest4 b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") DT.Type),("girl",DT.Pi (DT.Con "entity") DT.Type),("x",DT.Con "entity"),("event",DT.Pi (DT.Con "entity") DT.Type),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.Con "entity") (DT.Pi (DT.App (DT.Con "girl") (DT.Var 0)) (DT.App (DT.Con "man") (DT.Var 1))),DT.App (DT.Con "girl") (DT.Con "x")]
    pre_type = DT.App (DT.Con "man") (DT.Con "x")
  in (True,executeWithDepth  varEnv sigEnv pre_type b 3)

piElimTestNotFound1 :: TestType
piElimTestNotFound1 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Var 2) (DT.Var 1) ,DT.Type,DT.Var 0,DT.Type]
    pre_type = DT.Var 0
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

piElimTestNotFound2 :: TestType
piElimTestNotFound2 b =
  let
    sigEnv = [("entity",DT.Type)]
    varEnv = [DT.Type,DT.Pi (DT.Var 0) (DT.Var 1) ,DT.Type,DT.Var 0,DT.Type]
    pre_type = DT.Var 2
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

eqIntroTestNotFound :: TestType
eqIntroTestNotFound b =
  let
    sigEnv = [("love",DT.Pi (DT.Con "entity") (DT.Pi (DT.Con "entity") DT.Type)),("man",DT.Pi (DT.Con "entity") DT.Type),("girl",DT.Pi (DT.Con "entity") DT.Type),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = []
    pre_type = DT.Eq (DT.Type) (DT.App (DT.App (DT.Con "love") (DT.Con "y")) (DT.Con "x")) (DT.App (DT.App (DT.Con "love") (DT.Con "y")) (DT.Con "x"))
  in (False,executeWithDepth varEnv sigEnv pre_type b 5)

eqElimTest1 :: TestType
eqElimTest1 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y"),DT.App (DT.Con "f") (DT.Con "x")]
    pre_type = DT.App (DT.Con "f") (DT.Con "y")
  in (True,executeWithDepth varEnv sigEnv pre_type b 7)

eqElimTest2 :: TestType
eqElimTest2 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.App (DT.Con "f") (DT.Con "x"),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Var 0),DT.Con "entity"]
    pre_type = DT.App (DT.Con "f") (DT.Var 2)
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

eqElimTest3 :: TestType
eqElimTest3 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.App (DT.Con "f") (DT.Var 2),DT.Eq (DT.Con "entity") (DT.Var 0) (DT.Var 1),DT.Con "entity",DT.Con "entity"]
    pre_type = DT.App (DT.Con "f") (DT.Var 2)
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

eqElimTest4 :: TestType
eqElimTest4 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Var 0),DT.Con "entity",DT.App (DT.Con "f") (DT.Con "x")]
    pre_type = DT.App (DT.Con "f") (DT.Var 1)
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

eqElimTest5 :: TestType
eqElimTest5 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Eq (DT.Con "entity") (DT.Var 0) (DT.Con "x") ,DT.Con "entity",DT.App (DT.Con "f") (DT.Con "x")]
    pre_type = DT.App (DT.Con "f") (DT.Var 1)
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

xy = DT.Eq (DT.Con "entity") (DT.Con"x") (DT.Con "y")
yx = DT.Eq (DT.Con "entity") (DT.Con"y") (DT.Con "x")
yz = DT.Eq (DT.Con "entity") (DT.Con "y") (DT.Con "z")
zy = DT.Eq (DT.Con "entity") (DT.Con "z") (DT.Con "y")
xz = DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "z") 
zx = DT.Eq (DT.Con "entity") (DT.Con "z") (DT.Con "x") 

transTest1 :: TestType
transTest1 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [xy,yz]
    pre_type = xz
  in (True,executeWithDepth varEnv sigEnv pre_type b 3)

transTest2 :: TestType
transTest2 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [xy,yz]
    pre_type = zx
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

transTest3 :: TestType
transTest3 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [xy,zy]
    pre_type = xz
  in (True,executeWithDepth varEnv sigEnv pre_type b 2)

transTest4 :: TestType
transTest4 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [xy,zy]
    pre_type = zx
  in (True,executeWithDepth varEnv sigEnv pre_type b 2)

transTest5 :: TestType
transTest5 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [yx,yz]
    pre_type = xz
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

transTest6 :: TestType
transTest6 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [yx,yz]
    pre_type = zx
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

transTest7 :: TestType
transTest7 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [yx,zy]
    pre_type = xz
  in (True,executeWithDepth varEnv sigEnv pre_type b 4)

transTest8 :: TestType
transTest8 b =
  let
    sigEnv = [("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [yx,zy]
    pre_type = zx
  in (True,executeWithDepth varEnv sigEnv pre_type b 3)

eqElimTestApp :: TestType
eqElimTestApp b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.App (DT.Con "f") (DT.Con "y"),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.App (DT.Con "f") (DT.Con "x")
  in (True,execute varEnv sigEnv pre_type b)

eqElimTestApp2 :: TestType
eqElimTestApp2 b =
  let
    sigEnv = [("sister",DT.Pi (DT.Con "entity") (DT.Con "entity")),("family",DT.Pi (DT.Con "entity") (DT.Con "entity")),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.App (DT.Con "sister") (DT.Con "x"),DT.Eq (DT.Pi (DT.Con "entity") (DT.Con "entity")) (DT.Con "family") (DT.Con "sister")]
    pre_type = DT.App (DT.Con "family") (DT.Con "x")
  in (True,executeWithDepth varEnv sigEnv pre_type b 5)

eqElimTestSigma1 :: TestType
eqElimTestSigma1 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Sigma (DT.App (DT.Con "f") (DT.Con "x")) (DT.App (DT.Con "f") (DT.Con "x")),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Sigma (DT.App (DT.Con "f") (DT.Con "x")) (DT.App (DT.Con "f") (DT.Con "y"))
  in (True,execute varEnv sigEnv pre_type b)

eqElimTestPi1 :: TestType
eqElimTestPi1 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.App (DT.Con "f") (DT.Con "x")) (DT.App (DT.Con "f") (DT.Con "x")),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Pi (DT.App (DT.Con "f") (DT.Con "x")) (DT.App (DT.Con "f") (DT.Con "y"))
  in (True,executeWithDepth varEnv sigEnv pre_type b 9)

eqElimTestPiNotFound :: TestType
eqElimTestPiNotFound b=
  let
    sigEnv = [("z",DT.Type),("y",DT.Type),("x",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.Con "z") (DT.Con "x"),DT.Eq (DT.Type) (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Pi (DT.Con "z") (DT.Con "y")
  in (False,executeWithDepth varEnv sigEnv pre_type b 7)

eqElimTestPi2 :: TestType
eqElimTestPi2 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Pi (DT.App (DT.Con "f") (DT.Con "z")) (DT.App (DT.Con "f") (DT.Con "x")),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Pi (DT.App (DT.Con "f") (DT.Con "z")) (DT.App (DT.Con "f") (DT.Con "y"))
  in (True,executeWithDepth varEnv sigEnv pre_type b 5)

eqElimTestPi3 :: TestType
eqElimTestPi3 b =
  let
    sigEnv = [("x",DT.Type),("entity",DT.Type)]
    varEnv = []
    pre_type = DT.Pi (DT.Con "x") (DT.Con "x")
  in (True,executeWithDepth varEnv sigEnv pre_type b 7)

eqElimTestSigmaNotFound :: TestType
eqElimTestSigmaNotFound b =
  let
    sigEnv = [("z",DT.Type),("y",DT.Type),("x",DT.Type),("entity",DT.Type)]
    varEnv = [DT.Sigma (DT.Con "x") (DT.Pi (DT.Con "z") (DT.Con "x")),DT.Eq (DT.Type) (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Sigma (DT.Con "x") (DT.Pi (DT.Con "z") (DT.Con "y"))
  in (False,executeWithDepth varEnv sigEnv pre_type b 7)
 
eqElimTestSigma2 :: TestType
eqElimTestSigma2 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("z",DT.Con "entity"),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Sigma (DT.App (DT.Con "f") (DT.Con "x")) (DT.Pi (DT.App (DT.Con "f") (DT.Con "z")) (DT.App (DT.Con "f") (DT.Con "x"))),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.Sigma (DT.App (DT.Con "f") (DT.Con "x")) (DT.Pi (DT.App (DT.Con "f") (DT.Con "z")) (DT.App (DT.Con "f") (DT.Con "y")))
  in (True,executeWithDepth varEnv sigEnv pre_type b 6)

eqElimTestNotFound1 :: TestType
eqElimTestNotFound1 b =
  let
    sigEnv = [("f",DT.Pi (DT.Con "entity") DT.Type),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Var 0),DT.Con "entity", DT.App (DT.Con "f") (DT.Con "x")]
    pre_type = DT.App (DT.Con "f") (DT.Con "y")
  in (False,executeWithDepth varEnv sigEnv pre_type b 4)

dneTest1 :: TestType
dneTest1 b =
  let
    sigEnv = []
    varEnv = [DT.Not $ DT.Not (DT.Var 0),DT.Type]
    pre_type = DT.Var 1
  in (True,executeWithDNE varEnv sigEnv pre_type b)

dneTest2 :: TestType
dneTest2 b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") (DT.Type)),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Not $ DT.Not (DT.App (DT.Con "man") (DT.Con "y"))]
    pre_type = DT.App (DT.Con "man") (DT.Con "y")
  in (True,executeWithDNE varEnv sigEnv pre_type b)

dneTest3 :: TestType
dneTest3 b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") (DT.Type)),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Not $ DT.Not (DT.App (DT.Con "man") (DT.Con "x")),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.App (DT.Con "man") (DT.Con "x")
  in (True,executeWithDNE varEnv sigEnv pre_type b)

dneTest4 :: TestType
dneTest4 b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") (DT.Type)),("y",DT.Con "entity"),("x",DT.Con "entity"),("entity",DT.Type)]
    varEnv = [DT.Not $ DT.Not (DT.App (DT.Con "man") (DT.Con "x")),DT.Eq (DT.Con "entity") (DT.Con "x") (DT.Con "y")]
    pre_type = DT.App (DT.Con "man") (DT.Con "y")
  in (True,executeWithDNEDepth varEnv sigEnv pre_type b 6)

sigmaIntroTest1 :: TestType
sigmaIntroTest1 b =
  let
    sigEnv = [("man",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [DT.App (DT.Con "man") (DT.Var 0),DT.Con "entity"]
    pre_type = DT.Sigma (DT.Con "entity") (DT.Con "entity")
  in (True,executeWithDNE varEnv sigEnv pre_type b)

sigmaIntroTest2 :: TestType
sigmaIntroTest2 b =
  let
    sigEnv = [("dog",DT.Pi (DT.Con "entity") (DT.Type)),("love",DT.Pi (DT.Con "entity") (DT.Pi (DT.Con "entity") DT.Type)),("man",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [DT.App (DT.App (DT.Con "love") (DT.Var 3)) (DT.Var 1),DT.App (DT.Con "dog") (DT.Var 0),DT.Con "entity",DT.App (DT.Con "man") (DT.Var 0),DT.Con "entity"]
    pre_type = DT.Sigma (DT.Con "entity") (DT.Sigma (DT.Con "entity") (DT.Sigma (DT.App (DT.Con "man") (DT.Var 1)) (DT.Sigma (DT.App (DT.App (DT.Con "love") (DT.Var 2)) (DT.Var 1)) (DT.App (DT.Con "dog") (DT.Var 6)))))
  in (True,executeWithDepth varEnv sigEnv pre_type b 3)

false :: TestType
false b =
  let
    sigEnv = []
    varEnv = [DT.Type]
    pre_type =  DT.Bot
  in  (False,execute varEnv sigEnv pre_type b)

piElimTest5 :: TestType
piElimTest5 b  =
  let
    sigEnv = sigEs
    varEnv = [p,DT.Pi p q,DT.Pi q r]
    pre_type =  r
  in(True,executeWithDNE varEnv sigEnv pre_type b)

piElimTest6 :: TestType
piElimTest6 b =
  let
    sigEnv = sigEs
    varEnv = [DT.Pi (DT.Var 2) (DT.Var 2),DT.Pi (DT.Var 2) (DT.Var 2),DT.Type,DT.Type,DT.Type]
    pre_type =  DT.Pi (DT.Var 4) (DT.Var 3)
  in (True,executeWithDNE varEnv sigEnv pre_type b)

dneTest5 :: TestType
dneTest5 =
  let
    sigEnv = sigEs
    varEnv = [p,DT.Pi q r,DT.Pi p (DT.Pi (DT.Pi q DT.Bot) DT.Bot)]
    pre_type = r
  in \b -> (True,executeWithDNE varEnv sigEnv pre_type b)
