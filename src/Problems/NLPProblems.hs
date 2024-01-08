{-# LANGUAGE OverloadedStrings #-}
module Problems.NLPProblems (
  yes,
  notYes
)
 where
import qualified DTS.UDTTdeBruijn as U
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import ProblemBase as PB
import qualified DTS.QueryTypes as QT (ProofSearchQuery(..))
import DTS.Labels (DTT)

yes :: [PB.TestType]
yes = [
  aGirlWritesAThesis_IsThereAGirl,
  aGirlWritesAThesis_IsThereAGirl',
  aManEntersHeWhistle_IsThereWhistler,
  aManEntersHeWhistle_IsThereMan]

notYes :: [PB.TestType]
notYes = []

thereIsA :: T.Text -> U.Preterm DTT
thereIsA txt = U.Sigma (U.Con "entity") (U.App (U.Con txt) (U.Var 0))

thereIsAGirl :: U.Preterm DTT
thereIsAGirl = thereIsA "girl"

aGirlWritesAThesis :: U.Preterm DTT
aGirlWritesAThesis = 
    U.Sigma 
        (thereIsAGirl)
        (U.Sigma 
            (U.Sigma (U.Con "entity") (U.App (U.Con "thesis") (U.Var 0))) 
            (U.App (U.App (U.Con "write") (U.Proj U.Fst $U.Var 1)) (U.Proj U.Fst $ U.Var 0))
        )

aGirlWritesAThesis_IsThereAGirl :: TestType
aGirlWritesAThesis_IsThereAGirl =
  let
    sigEnv = [("write",U.Pi (U.Con "entity") (U.Pi (U.Con "entity") (U.Type))),("thesis",U.Pi (U.Con "entity") (U.Type)),("girl",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aGirlWritesAThesis]
    pre_type = thereIsAGirl
  in (True,executeWithDNEDepth 3 QT.ProofSearchQuery{QT.sig=sigEnv,QT.ctx=varEnv,QT.typ=pre_type})

aGirlWritesAThesis' :: U.Preterm DTT
aGirlWritesAThesis' = 
    U.Sigma 
        (U.Sigma 
            thereIsAGirl
            (U.Sigma (U.Con "entity") (U.App (U.Con "thesis") (U.Var 0)))
        )
        (U.App (U.App (U.Con "write") (U.Proj U.Fst $U.Proj U.Fst $U.Var 0)) (U.Proj U.Fst $U.Proj U.Snd $U.Var 0))

aGirlWritesAThesis_IsThereAGirl' :: TestType
aGirlWritesAThesis_IsThereAGirl'  =
  let
    sigEnv = [("write",U.Pi (U.Con "entity") (U.Pi (U.Con "entity") (U.Type))),("thesis",U.Pi (U.Con "entity") (U.Type)),("girl",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aGirlWritesAThesis']
    pre_type = thereIsAGirl
  in (True,executeWithDNEDepth 3 QT.ProofSearchQuery{QT.sig=sigEnv,QT.ctx=varEnv,QT.typ=pre_type})

aManEntersHeWhistle :: U.Preterm DTT
aManEntersHeWhistle = 
    U.Sigma 
        (U.Sigma (U.Con "entity") (U.App (U.Con "man") (U.Var 0)))
        (U.Sigma
            (U.App (U.Con "enter") (U.Proj U.Fst $U.Var 0))
            (U.App (U.Con "whistle") (U.Proj U.Fst $U.Var 1))
        )

aManWhistle :: U.Preterm DTT
aManWhistle = 
    U.Sigma (U.Con "entity") (U.App (U.Con "whistle") (U.Var 0))

thereIsAMan :: U.Preterm DTT
thereIsAMan = thereIsA "man"

aManEntersHeWhistle_IsThereWhistler :: PB.TestType
aManEntersHeWhistle_IsThereWhistler =
  let
    sigEnv = [("whistle",U.Pi (U.Con "entity") (U.Type)),("enter",U.Pi (U.Con "entity") (U.Type)),("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = aManWhistle
  in (True,executeWithDNEDepth 2 QT.ProofSearchQuery{QT.sig=sigEnv,QT.ctx=varEnv,QT.typ=pre_type})

aManEntersHeWhistle_IsThereMan :: PB.TestType
aManEntersHeWhistle_IsThereMan =
  let
    sigEnv = [("whistle",U.Pi (U.Con "entity") (U.Type)),("enter",U.Pi (U.Con "entity") (U.Type)),("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = thereIsAMan
  in (True,executeWithDNEDepth 3 QT.ProofSearchQuery{QT.sig=sigEnv,QT.ctx=varEnv,QT.typ=pre_type})
