import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import KarateChop
import Data.Vector.Primitive as V (fromList)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
  [ TestList vectorTests]

vectorTests :: [Test]
vectorTests =
  [ testCase "empty list" $ (Nothing) @=? search (VectorArgs (V.fromList []) 0) 4
  , testCase "Test position" $ (Just 0) @=? search (VectorArgs (V.fromList [1..6]) 6) 1
  , testCase "Test position" $ (Just 1) @=? search (VectorArgs (V.fromList [1..6]) 6) 2
  , testCase "Test position" $ (Just 2) @=? search (VectorArgs (V.fromList [1..6]) 6) 3
  , testCase "Test position" $ (Just 3) @=? search (VectorArgs (V.fromList [1..6]) 6) 4
  , testCase "Test position" $ (Just 4) @=? search (VectorArgs (V.fromList [1..6]) 6) 5
  , testCase "Test position" $ (Just 5) @=? search (VectorArgs (V.fromList [1..6]) 6) 6
  , testCase "Test Invalid position" $ (Nothing) @=? search (VectorArgs (V.fromList [1..6]) 6) 7
  ]


