module Main where

import qualified Data.Map as M
import Algorithms.Htn

data Term' = HasTarget Bool
           | HasAmmo Bool
           | HasMagazine Bool
           | CanSeeTarget Bool
           | AtTarget Bool
           deriving (Eq, Ord, Show)

data PrimitiveTask' = Melee
                    | Shot
                    | Reload
                    | MoveToTarget
                    | MoveToShootingPoint
                    deriving (Eq, Ord, Enum, Show)

data CompoundTask' = KillTarget
                   | PrepareShooting
                   deriving (Eq, Ord, Enum, Show)

type Condition = [Term']

instance Term Term'
instance PrimitiveTask PrimitiveTask'
instance CompoundTask CompoundTask'

main :: IO ()
main = do
  let startCondition = [HasTarget True, HasAmmo False, HasMagazine True, CanSeeTarget False, AtTarget False]
  print "------------- domain ----------------"
  print buildDomain
  print "------------- target ----------------"
  print $ "start: " ++ show startCondition
  print "------------- plan ----------------"
  let (tasks, cond) = htn buildDomain startCondition [Compound KillTarget]
  mapM_ print tasks
  print cond

buildDomain :: Domain PrimitiveTask' CompoundTask' Term'
buildDomain = Domain buildPrimitiveMap buildCompoundMap
  where
    buildPrimitiveMap = M.fromList $ map buildDomainPrimitive [Melee ..]
    buildCompoundMap = M.fromList $ map buildDomainCompound [KillTarget ..]

buildDomainPrimitive :: PrimitiveTask' -> (PrimitiveTask', [(Condition, Condition)])
buildDomainPrimitive task@Melee = (task, [(pre, post)])
  where pre  = [HasTarget True, AtTarget True]
        post = [HasTarget False, AtTarget False]
buildDomainPrimitive task@Shot = (task, [(pre, post)])
  where pre  = [HasTarget True, HasAmmo True, CanSeeTarget True]
        post = [HasTarget False, HasAmmo False, CanSeeTarget False]
buildDomainPrimitive task@Reload = (task, [(pre, post)])
  where pre  = [HasAmmo False, HasMagazine True]
        post = [HasAmmo True, HasMagazine False]
buildDomainPrimitive task@MoveToTarget = (task, [(pre, post)])
  where pre  = [AtTarget False]
        post = [AtTarget True]
buildDomainPrimitive task@MoveToShootingPoint = (task, [(pre, post)])
  where pre  = [CanSeeTarget False]
        post = [CanSeeTarget True]

buildDomainCompound :: CompoundTask' -> (CompoundTask', [(Condition, [Task PrimitiveTask' CompoundTask'])])
buildDomainCompound task@KillTarget = (task,
  [ ([HasAmmo True],     [Compound PrepareShooting, Primitive Shot])
  , ([HasMagazine True], [Compound PrepareShooting, Primitive Shot])
  , ([AtTarget True],    [Primitive Melee])
  , ([],            [Primitive MoveToTarget, Primitive Melee])
  ])
buildDomainCompound task@PrepareShooting = (task,
  [ ([HasAmmo True, CanSeeTarget True],     [])
  , ([HasAmmo False, HasMagazine True],     [Primitive Reload, Compound PrepareShooting])
  , ([HasAmmo True, CanSeeTarget False], [Primitive MoveToShootingPoint, Compound PrepareShooting])
  , ([], [Invalid "cant prepare shooting"])
  ])
