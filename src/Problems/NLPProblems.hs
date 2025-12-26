{-# LANGUAGE OverloadedStrings #-}
module Problems.NLPProblems (
  yes,
  notYes
)
 where
import qualified DTS.DTTdeBruijn as U
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import ProblemBase as PB

yes :: [PB.TestType]
yes = [
  aGirlWritesAThesis_IsThereAGirl,
  aGirlWritesAThesis_IsThereAGirl',
  aManEntersHeWhistle_IsThereWhistler,
  aManEntersHeWhistle_IsThereMan,
  entityCheck,
  q619,
  q623',
  q624
  ]

notYes :: [PB.TestType]
notYes = [
  q623,
  q623'WithoutEFQ
  ]

thereIsA :: T.Text -> U.Preterm
thereIsA txt = U.Sigma (U.Con "entity") (U.App (U.Con txt) (U.Var 0))

thereIsAGirl :: U.Preterm
thereIsAGirl = thereIsA "girl"

aGirlWritesAThesis :: U.Preterm
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
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

aGirlWritesAThesis' :: U.Preterm
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
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

aManEntersHeWhistle :: U.Preterm
aManEntersHeWhistle = 
    U.Sigma 
        (U.Sigma (U.Con "entity") (U.App (U.Con "man") (U.Var 0)))
        (U.Sigma
            (U.App (U.Con "enter") (U.Proj U.Fst $U.Var 0))
            (U.App (U.Con "whistle") (U.Proj U.Fst $U.Var 1))
        )

aManWhistle :: U.Preterm
aManWhistle = 
    U.Sigma (U.Con "entity") (U.App (U.Con "whistle") (U.Var 0))

thereIsAMan :: U.Preterm
thereIsAMan = thereIsA "man"

aManEntersHeWhistle_IsThereWhistler :: PB.TestType
aManEntersHeWhistle_IsThereWhistler =
  let
    sigEnv = [("whistle",U.Pi (U.Con "entity") (U.Type)),("enter",U.Pi (U.Con "entity") (U.Type)),("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = aManWhistle
  in (True,executeWithDNEDepth 2 (U.ProofSearchQuery sigEnv varEnv pre_type))

aManEntersHeWhistle_IsThereMan :: PB.TestType
aManEntersHeWhistle_IsThereMan =
  let
    sigEnv = [("whistle",U.Pi (U.Con "entity") (U.Type)),("enter",U.Pi (U.Con "entity") (U.Type)),("man",U.Pi (U.Con "entity") (U.Type)),("entity",U.Type)]
    varEnv = [aManEntersHeWhistle]
    pre_type = thereIsAMan
  in (True,executeWithDNEDepth 3 (U.ProofSearchQuery sigEnv varEnv pre_type))

entityCheck :: PB.TestType
entityCheck =
  let
    sigEnv = [
                ("girl",U.Pi U.Entity U.Type),
                ("leave",U.Pi U.Entity U.Type),
                ("boy",U.Pi U.Entity U.Type),
                ("cameIn",U.Pi U.Entity U.Type),
                ("male",U.Pi U.Entity U.Type),
                ("smile",U.Pi U.Entity U.Type),
                ("boyIsMale",U.Pi (U.Sigma (U.Entity) (U.App (U.Con "boy") (U.Var 0))) (U.App (U.Con "male") (U.Proj U.Fst (U.Var 0))))
              ]
    varEnv = [
                U.Sigma
                  (U.Sigma U.Entity (U.App (U.Con "girl") (U.Var 0)))
                  (U.Sigma
                    (U.App (U.Con "leave") (U.Proj U.Fst (U.Var 0)))
                    (U.Sigma
                      (U.Sigma U.Entity (U.App (U.Con "boy") (U.Var 0)))
                      (U.App (U.Con "cameIn") (U.Proj U.Fst (U.Var 0)))
                    )
                  )
              ]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "male") (U.Var 0))
  in (True,PB.executeWithDepth 6 (U.ProofSearchQuery sigEnv varEnv pre_type))


{--
a. [[A professor] or [an assistant professor]]i will attend the meeting of the university board.
b. She will report to the faculy.

[[x:entity, prof(x)],wAtM(pi1u)] + [[x:entity, aprof(x)],wAtM(pi1u)] \vdash ? : [x:entity,female(x)]

a. pf : u : x : [entity,prof(x)] →female(π1u)
b. af : u : x : [entity,aProf(x)] →female(π1u)

623 
[A professor]1 attended the meeting or the meeting was cancelled.
# She1 will report to the faculty.
--}


profsigEs = [
  ("pf",U.Pi (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "female") (U.Proj U.Fst (U.Var 0)))),
  ("af",U.Pi (U.Sigma (U.Entity) (U.App (U.Con "aProf") (U.Var 0))) (U.App (U.Con "female") (U.Proj U.Fst (U.Var 0)))),
  ("female",U.Pi U.Entity U.Type),
  ("prof",U.Pi U.Entity U.Type),
  ("aProf",U.Pi U.Entity U.Type),
  ("wAtM",U.Pi U.Entity U.Type),
  ("mCancell",U.Type)
  ]

q619 :: TestType 
q619 =
  let
    sigEnv = profsigEs
    varEnv = [U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "aProf") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0))))]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (True,PB.executeWithDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

q623 :: TestType
q623 =
  let
    sigEnv = profsigEs
    varEnv = [U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Con "mCancell")]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (False,PB.executeWithEFQDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

q623' :: TestType
q623' =
  let
    sigEnv = profsigEs
    varEnv = [U.Not ((U.Con "mCancell")), U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Con "mCancell")]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (True,PB.executeWithEFQDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

q623'WithoutEFQ :: TestType
q623'WithoutEFQ =
  let
    sigEnv = profsigEs
    varEnv = [U.Not ((U.Con "mCancell")), U.Disj (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "prof") (U.Var 0))) (U.App (U.Con "wAtM") (U.Proj U.Fst (U.Var 0)))) (U.Con "mCancell")]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "female") (U.Var 0))
  in (False,PB.executeWithDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))

-- {--
-- a. Either it’s a holiday or [a customer]1 will come in.
-- b. And if it’s not a holiday, they1 will want to be served.

-- v : holoday(today) + [[x:entity,customer(x)],WCI(pi1u)], w : not holoday(today) \vdash ? [x: entity, human(x)]

-- a. cf : u : x : [entity,customer(x)] →human(π1u)
-- --}

cussigEs = [
  ("holiday",U.Pi U.Entity U.Type),
  ("customer",U.Pi U.Entity U.Type),
  ("WCI",U.Pi U.Entity U.Type),
  ("today",U.Entity),
  ("human",U.Pi U.Entity U.Type),
  ("cf",U.Pi (U.Sigma (U.Entity) (U.App (U.Con "customer") (U.Var 0))) (U.App (U.Con "human") (U.Proj U.Fst (U.Var 0))))
  ]

q624 :: TestType
q624 =
  let
    sigEnv = cussigEs
    varEnv = [U.Not (U.App (U.Con "holiday") (U.Con "today")),U.Disj (U.App (U.Con "holiday") (U.Con "today")) (U.Sigma (U.Sigma (U.Entity) (U.App (U.Con "customer") (U.Var 0))) (U.App (U.Con "WCI") (U.Proj (U.Fst) (U.Var 0))))]
    pre_type = U.Sigma (U.Entity) (U.App (U.Con "human") (U.Var 0))
  in (True,PB.executeWithEFQDepth 9 (U.ProofSearchQuery sigEnv varEnv pre_type))