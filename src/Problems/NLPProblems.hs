{-# LANGUAGE OverloadedStrings #-}
module Problems.NLPProblems (
  yes,
  notYes
)
 where
import qualified DTS.DTT as DT
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import ProblemBase as PB

yes :: [PB.TestType]
yes = [
  aGirlWritesAThesis_IsThereAGirl,
  aGirlWritesAThesis_IsThereAGirl',
  aManEntersHeWhistle_IsThereWhistler,
  aManEntersHeWhistle_IsThereMan]

notYes :: [PB.TestType]
notYes = []

thereIsA :: T.Text -> DT.Preterm
thereIsA txt = DT.Sigma (DT.Con "entity") (DT.App (DT.Con txt) (DT.Var 0))

thereIsAGirl :: DT.Preterm
thereIsAGirl = thereIsA "girl"

aGirlWritesAThesis :: DT.Preterm
aGirlWritesAThesis = 
    DT.Sigma 
        (thereIsAGirl)
        (DT.Sigma 
            (DT.Sigma (DT.Con "entity") (DT.App (DT.Con "thesis") (DT.Var 0))) 
            (DT.App (DT.App (DT.Con "write") (DT.Proj DT.Fst $DT.Var 1)) (DT.Proj DT.Fst $ DT.Var 0))
        )

aGirlWritesAThesis_IsThereAGirl :: TestType
aGirlWritesAThesis_IsThereAGirl  b =
  let
    sigEnv = [("write",DT.Pi (DT.Con "entity") (DT.Pi (DT.Con "entity") (DT.Type))),("thesis",DT.Pi (DT.Con "entity") (DT.Type)),("girl",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [aGirlWritesAThesis]
    pre_type = thereIsAGirl
  in (True,executeWithDNEDepth varEnv sigEnv pre_type b 3)

aGirlWritesAThesis' :: DT.Preterm
aGirlWritesAThesis' = 
    DT.Sigma 
        (DT.Sigma 
            thereIsAGirl
            (DT.Sigma (DT.Con "entity") (DT.App (DT.Con "thesis") (DT.Var 0)))
        )
        (DT.App (DT.App (DT.Con "write") (DT.Proj DT.Fst $DT.Proj DT.Fst $DT.Var 0)) (DT.Proj DT.Fst $DT.Proj DT.Snd $DT.Var 0))

aGirlWritesAThesis_IsThereAGirl' :: TestType
aGirlWritesAThesis_IsThereAGirl'  b =
  let
    sigEnv = [("write",DT.Pi (DT.Con "entity") (DT.Pi (DT.Con "entity") (DT.Type))),("thesis",DT.Pi (DT.Con "entity") (DT.Type)),("girl",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [aGirlWritesAThesis']
    pre_type = thereIsAGirl
  in (True,executeWithDNEDepth varEnv sigEnv pre_type b 3)

aManEntersHeWhistle :: DT.Preterm
aManEntersHeWhistle = 
    DT.Sigma 
        (DT.Sigma (DT.Con "entity") (DT.App (DT.Con "man") (DT.Var 0)))
        (DT.Sigma
            (DT.App (DT.Con "enter") (DT.Proj DT.Fst $DT.Var 0))
            (DT.App (DT.Con "whistle") (DT.Proj DT.Fst $DT.Var 1))
        )

aManWhistle :: DT.Preterm
aManWhistle = 
    DT.Sigma (DT.Con "entity") (DT.App (DT.Con "whistle") (DT.Var 0))

thereIsAMan :: DT.Preterm
thereIsAMan = thereIsA "man"

aManEntersHeWhistle_IsThereWhistler :: PB.TestType
aManEntersHeWhistle_IsThereWhistler  b =
  let
    sigEnv = [("whistle",DT.Pi (DT.Con "entity") (DT.Type)),("enter",DT.Pi (DT.Con "entity") (DT.Type)),("man",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = aManWhistle
  in (True,executeWithDNEDepth varEnv sigEnv pre_type b 2)

aManEntersHeWhistle_IsThereMan :: PB.TestType
aManEntersHeWhistle_IsThereMan  b =
  let
    sigEnv = [("whistle",DT.Pi (DT.Con "entity") (DT.Type)),("enter",DT.Pi (DT.Con "entity") (DT.Type)),("man",DT.Pi (DT.Con "entity") (DT.Type)),("entity",DT.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = thereIsAMan
  in (True,executeWithDNEDepth varEnv sigEnv pre_type b 3)
