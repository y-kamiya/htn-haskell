import Test.Hspec
import qualified Data.Map as M
import Data.List (sort)

import Algorithms.Htn
import Algorithms.Htn.Internal

-- example of blocks world
data Block = A | B deriving (Show, Eq, Ord, Enum)
data Object = Table | Object Block deriving (Eq, Ord, Show)

data BWTerm = HandEmpty
            | HandHas Block
            | IsTop Block Bool
            | On Block Object
            deriving (Eq, Ord, Show)

data BWPrimitiveTask = Stack Block Block
                     | Unstack Block Block
                     | Putdown Block
                     | Pickup Block
                     deriving (Eq, Ord, Show)

data BWCompoundTask = Move Block Object
                    | Clear Block
                    | Get Block
                    | Put Block Object
                     deriving (Eq, Ord, Show)

type Condition = [BWTerm]

instance Term BWTerm
instance PrimitiveTask BWPrimitiveTask
instance CompoundTask BWCompoundTask

main :: IO ()
main = hspec $ do
  describe "execute" $ do
    it "gets empty list when it dont find the task in domain" $ do
      execute buildTestDomain [HandEmpty] (Pickup B) `shouldBe` []
    it "gets empty list when it dont find matched conditions in domain" $ do
      execute buildTestDomain [HandEmpty] (Pickup A) `shouldBe` []
    it "gets matched conditions" $ do
      let Just list = M.lookup (Pickup A) $ primitiveMap buildDomain
          (pre, post) = head list
      execute buildDomain pre (Pickup A) `shouldBe` post

  describe "breakdown" $ do
    it "gets Invalid task when it dont find the task in domain" $ do
      let result = breakdown buildTestDomain [HandEmpty] (Move A (Object B))
      isInvalid (head result) `shouldBe` True
    it "gets Invalid task when it dont find the task in domain" $ do
      let result = breakdown buildTestDomain [HandEmpty] (Move A Table)
      isInvalid (head result) `shouldBe` True
    it "gets correct tasks" $ do
      let Just list = M.lookup (Move A (Object B)) $ compoundMap buildDomain
          (pre, post) = head list
      breakdown buildDomain pre (Move A (Object B)) `shouldBe` post

  describe "htn" $ do
    let startCondition = [HandEmpty, IsTop A True, IsTop B False, On A (Object B), On B Table]
        expectedTasks = [Primitive (Unstack A B), Primitive (Putdown A), Primitive (Pickup B), Primitive (Stack B A)]
        expectedCondition = [On A Table,HandEmpty,IsTop B True,IsTop A False,On B (Object A)]
        (tasks, condition) = htn buildDomain startCondition [Compound $ Move A Table, Compound $ Move B (Object A)]
    it "gets correct tasks" $ sort tasks `shouldBe` sort expectedTasks
    it "reaches correct condition" $ sort condition `shouldBe` sort expectedCondition

isInvalid (Invalid _) = True
isInvalid _ = False

buildTestDomain :: Domain BWPrimitiveTask BWCompoundTask BWTerm
buildTestDomain = Domain buildPrimitiveMap buildCompoundMap
  where
    buildPrimitiveMap = M.fromList [(Pickup A, [([On A Table], [HandHas A])])]
    buildCompoundMap = M.fromList [(Move A Table, [([On A Table], [Primitive (Pickup A), Compound (Get A)])])]

buildDomain :: Domain BWPrimitiveTask BWCompoundTask BWTerm
buildDomain = Domain buildPrimitiveMap buildCompoundMap
  where
    buildPrimitiveMap = M.fromList $ stacks ++ unstacks ++ putdowns ++ pickups
    pickups  = map (buildDomainPrimitive . Pickup) [A ..]
    putdowns = map (buildDomainPrimitive . Putdown) [A ..]
    stacks   = map (buildDomainPrimitive . uncurry Stack) perms
    unstacks = map (buildDomainPrimitive . uncurry Unstack) perms
    perms = [(x, y) | x <- [A ..], y <- [A ..], x /= y]

    buildCompoundMap = M.fromList $ moves ++ clears ++ gets ++ puts
    clears = map (buildDomainCompound . Clear) [A ..]
    gets   = map (buildDomainCompound . Get) [A ..]
    puts   = map (buildDomainCompound . uncurry Put) permsObj
    moves  = map (buildDomainCompound . uncurry Move) permsObj
    permsObj = [(x, y) | x <- [A ..], y <- Table:map Object [A ..]]

buildDomainPrimitive :: BWPrimitiveTask -> (BWPrimitiveTask, [(Condition, Condition)])
buildDomainPrimitive task@(Pickup x) = (task, [(pre, post)])
  where pre  = [HandEmpty, IsTop x True, On x Table]
        post = [HandHas x, IsTop x False]
buildDomainPrimitive task@(Putdown x) = (task, [(pre, post)])
  where pre  = [HandHas x, IsTop x False]
        post = [HandEmpty, IsTop x True, On x Table]
buildDomainPrimitive task@(Stack x y) = (task, [(pre, post)])
  where pre  = [HandHas x, IsTop x False, IsTop y True]
        post = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
buildDomainPrimitive task@(Unstack x y) = (task, [(pre, post)])
  where pre  = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
        post = [HandHas x, IsTop x False, IsTop y True]

buildDomainCompound :: BWCompoundTask -> (BWCompoundTask, [(Condition, [Task BWPrimitiveTask BWCompoundTask])])
buildDomainCompound task@(Move x Table) = (task,
  [ ([On x Table], [])
  , ([], [Compound (Get x), Compound (Put x Table)])
  ])
buildDomainCompound task@(Move x (Object y)) = (task,
  [ ([On x (Object y)]         , [])
  , ([HandHas x, IsTop y True] , [Compound (Put x (Object y))])
  , ([HandHas x]               , [Compound (Clear y), Compound (Put x (Object y))])
  , ([IsTop x False]           , [Compound (Clear x), Compound (Move x (Object y))])
  , ([IsTop y False]           , [Compound (Clear y), Compound (Move x (Object y))])
  , ([]                        , [Compound (Get x  ), Compound (Put x (Object y))])
  ])
buildDomainCompound task@(Clear x) = (task,
  [ ([IsTop x True], []) ]
  ++ map clear [A ..]
  ++ [([], [])]
  )
  where clear a = ([IsTop x False, On a (Object x)], [Compound (Move a Table)])
buildDomainCompound task@(Get x) = (task,
  [ ([HandHas x], []) ]
  ++ map get [A ..] ++
  [ ([HandEmpty, IsTop x True, On x Table], [Primitive (Pickup x)])
  , ([HandEmpty, IsTop x False]           , [Compound (Clear x), Compound (Get x)])
  , ([]         , [Invalid $ "cant breakdown " ++ show task])
  ])
  where get a = ([HandEmpty, IsTop x True, On x (Object a)], [Primitive (Unstack x a)])
buildDomainCompound task@(Put x Table) = (task,
  [ ([HandHas x], [Primitive (Putdown x)])
  , ([]         , [Invalid $ "cant breakdown " ++ show task])
  ])
buildDomainCompound task@(Put x (Object y)) = (task,
  [ ([HandHas x, IsTop y True], [Primitive (Stack x y)])
  , ([]                       , [Invalid $ "cant breakdown " ++ show task])
  ])
