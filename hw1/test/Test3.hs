module Test3 where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           TestLib           (listToIO)

import           Task3

spec :: IO ()
spec = hspec $ do
    describe "Days Of Week" $ do
        it "nextDay" $ listToIO $ zipWith shouldBe (map nextDay [Monday ..]) [Tuesday .. Monday]
        it "afterDays" $ do
            afterDays Monday 1 `shouldBe` Tuesday
            afterDays Friday 2 `shouldBe` Sunday
        it "isWeekend" $ listToIO $ zipWith shouldBe (map isWeekend [Monday ..]) [False, False, False, False, False, True, True]
        it "daysToParty" $ listToIO $  zipWith shouldBe (map daysToParty [Monday ..]) [4, 3, 2, 1, 0, 6, 5]
    describe "Knights & Creeps" $ do
        it "Just fight" $ do
            fight (Knight 3 12) (Creep 4 9) `shouldBe` (Left (Knight 3 12), 4)
    describe "Vectors" $ do
        it "vLength" $ do
            vLength (Vector3D 1 2 2) `shouldBe` 3
            vLength (Vector2D 3 4) `shouldBe` 5

        it "vSum" $ do
            vLength (vSum (Vector3D 0 0 2) (Vector2D 1 2)) `shouldBe` 3
            vLength (vSum (Vector3D 0 0 2) (Vector3D 1 2 0)) `shouldBe` 3

        it "vScalP" $ do
            vScalP (Vector3D 1 1 1) (Vector2D 1 1) `shouldBe` 2
            vScalP (Vector3D 1 1 1) (Vector3D 1 1 1) `shouldBe` 3

        it "vDist" $ vDist (Vector3D 0 0 2) (Vector2D (-1) (-2)) `shouldBe` 3

        it "vVectP" $ do
            vVectP (Vector3D 1 2 3) (Vector3D 3 2 1) `shouldBe` Just (Vector3D (-4) 8 (-4))
            vVectP (Vector3D 1 2 3) (Vector2D 3 2) `shouldBe` Nothing

    describe "Nat" $ do
        it "Eq" $ do
            Z `shouldBe` Z
            Z `shouldNotBe` S Z
            S Z `shouldBe` S Z
        it "Ord" $ do
            Z `shouldSatisfy` (<= S Z)
            S Z `shouldNotSatisfy` (<= Z)
        it "Num" $ do
            Z + Z `shouldBe` Z
            Z + S Z `shouldBe` S Z
            S Z + Z `shouldBe` S Z
    describe "Binary search tree" $ do
        it "tEmpty" $ do
            tEmpty (fromList [1]) `shouldBe` False
            tEmpty (fromList [1, 2, 3]) `shouldBe` False

        it "tSize" $ do
            tSize (fromList [1, 2, 3]) `shouldBe` 3
            tSize (fromList [1, 2, 3, 4]) `shouldBe` 4

        it "tContains" $ do
            tContains (fromList [1, 2, 3]) 3 `shouldBe` True
            tContains (fromList [1, 2, 3]) 4 `shouldBe` False

        it "tInsert" $ do
            tContains (tInsert (fromList [1, 2]) 0) 3 `shouldBe` False
            tSize (tInsert (fromList [1, 2]) 3) `shouldBe` 3
