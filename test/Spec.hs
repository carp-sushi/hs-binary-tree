import Data.Tree

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import ASCII.Char
import Control.Monad ((>=>))

-- Input
numbers :: [Int]
numbers = [4, 2, 1, 3, 7, 6, 9]

-- Expected tree from mkTree numbers.
tree :: Tree Int
tree =
  Node 4 (Node 2 (pure 1) (pure 3)) (Node 7 (pure 6) (pure 9))

-- Expected tree with 10 added
tree2 :: Tree Int
tree2 =
  Node 4 (Node 2 (pure 1) (pure 3)) (Node 7 (pure 6) (Node 9 Nil (pure 10)))

-- Expected inverted tree
inverted :: Tree Int
inverted =
  Node 4 (Node 7 (pure 9) (pure 6)) (Node 2 (pure 3) (pure 1))

-- Expected tree after removal of 7
removed7 :: Tree Int
removed7 =
  Node 4 (Node 2 (pure 1) (pure 3)) (Node 9 (pure 6) Nil)

-- Expected tree after removal of 4 (the root)
removed4 :: Tree Int
removed4 =
  Node 6 (Node 2 (pure 1) (pure 3)) (Node 7 Nil (pure 9))

-- An empty tree
nilTree :: Tree Int
nilTree =
  Nil

-- Tests basic binary tree operations
spec_basic_ops :: Spec
spec_basic_ops =
  describe "basic operations" $ do
    it "constructor" $ do
      mkTree numbers `shouldBe` tree
      mkTree ([] :: [Int]) `shouldBe` Nil
    it "insert" $ do
      insert 10 tree `shouldBe` tree2
      insert 2 tree `shouldBe` tree
      mkTree [6, 5, 7, 6, 5, 7, 6, 5, 7] `shouldBe` mkTree [6, 5, 7]
      (1 <+ 3 <+ 2 <+ Nil) `shouldBe` (Nil +> 2 +> 3 +> 1)
    it "search" $ do
      search 7 tree `shouldBe` mkTree [7, 6, 9]
      search 99 tree `shouldBe` Nil
    it "remove" $ do
      remove 7 tree `shouldBe` removed7
      remove 4 tree `shouldBe` removed4
      remove 99 tree `shouldBe` tree
    it "min_" $ do
      min_ tree `shouldBe` Just 1
      min_ nilTree `shouldBe` Nothing
    it "max_" $ do
      max_ tree `shouldBe` Just 9
      max_ nilTree `shouldBe` Nothing
    it "invert" $ do
      invert tree `shouldBe` Inverted inverted
    it "flatten" $ do
      flatten tree `shouldBe` numbers

-- Specs for functor laws
spec_functor_laws :: Spec
spec_functor_laws =
  describe "functor laws" $ do
    let f = (+ 1)
        g = (* 2)
        h = tree
    it "identity" $ do
      fmap id h `shouldBe` id h
    it "composition" $ do
      fmap (f . g) h `shouldBe` (fmap f . fmap g) h

-- Tests for apply laws
spec_applicative_laws :: Spec
spec_applicative_laws =
  describe "applicative laws" $ do
    it "associative composition" $ do
      let f = pure (+ 1)
          g = pure (* 2)
          h = tree
      ((.) <$> f <*> g <*> h) `shouldBe` (f <*> (g <*> h))
    it "identity" $ do
      let v = tree
      pure id <*> v `shouldBe` v
    it "composition" $ do
      let f = pure (+ 1)
          g = pure (* 2)
          h = tree
      (pure (.) <*> f <*> g <*> h) `shouldBe` f <*> (g <*> h)
    it "homomorphism" $ do
      let f = flatten
          puref = pure f :: Tree (Tree Int -> [Int])
          x = tree
      puref <*> pure x `shouldBe` (pure (f x) :: Tree [Int])
    it "interchange" $ do
      let u = pure flatten :: Tree (Tree Int -> [Int])
          y = tree
      u <*> pure y `shouldBe` pure ($ y) <*> u

-- Tests for monad laws
spec_monad_laws :: Spec
spec_monad_laws =
  describe "monad laws" $ do
    let x = tree
    it "associativity" $ do
      let f i = pure (i + 1)
      let g i = pure (i * 2)
      ((x >>= f) >>= g) `shouldBe` (x >>= (f >=> g))
    it "apply superclass" $ do
      let f = pure (+ 1)
      (f <*> x) `shouldBe` (f >>= \f' -> fmap f' x)
    it "left identity" $ do
      let f = flatten
      (pure x >>= f) `shouldBe` f x
    it "right identity" $ do
      (x >>= pure) `shouldBe` x

-- Tests for foldable
spec_foldable :: Spec
spec_foldable =
  describe "foldable tests" $ do
    let accShow acc x = acc <> show x
    it "fold left" $ do
      foldl accShow "" tree `shouldBe` "4213769"
    it "fold right" $ do
      foldr (flip accShow) "" tree `shouldBe` "9673124"
    it "fold map" $ do
      foldMap (\x -> [2 * x]) tree `shouldBe` [8, 4, 2, 6, 14, 12, 18]

-- Tests for traversable.
spec_traversable :: Spec
spec_traversable =
  describe "traversable tests" $ do
    let a = CapitalLetterA
        b = CapitalLetterB
        c = CapitalLetterC
        t1 = pure 65 +> 66 +> 67
        t2 = mkTree [Just a, Just b, Just c]
        expected = Just $ pure a +> b +> c -- expected result
    it "traverse" $ do
      traverse fromIntMaybe t1 `shouldBe` expected
      traverse fromIntMaybe (mkTree [65, 66, 128]) `shouldBe` Nothing
    it "sequence" $ do
      sequence t2 `shouldBe` expected
      sequence (mkTree [Just a, Nothing, Just c]) `shouldBe` Nothing
    it "compatibility" $ do
      traverse fromIntMaybe t1 `shouldBe` sequence (fromIntMaybe <$> t1)
      sequence t2 `shouldBe` traverse id t2

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_basic_ops
  , spec_functor_laws
  , spec_applicative_laws
  , spec_monad_laws
  , spec_foldable
  , spec_traversable
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Binary Tree Specs" specs)
