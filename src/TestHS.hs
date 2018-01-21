module TestHS
    ( Test
    , runTest
    , reportTests
    , testPassed
    , testFailed
    ) where

import Data.Tuple
import Control.Monad
import System.Console.ANSI
import System.Exit


-- | Test data type
data Test = Test
  { name :: String
  , outcome :: Either (String, String) String
  } deriving (Show, Eq)

testPassed :: String -> String -> Test
testPassed t s = Test 
  { name = t
  , outcome = Right s
  }
  
testFailed :: String -> (String,String) -> Test
testFailed t f = Test 
  { name = t
  , outcome = Left f
  }
 
-- | run pure test
runTest :: Test -> IO Test
runTest t = do
  case outcome t of
    Left err -> do
      putStr $ name t
      putStr $ " expected: " ++ fst err
      putStr $ " got: " ++ snd err
      setSGR [SetColor Foreground Vivid Red]
      putStrLn  " ✗" 
      setSGR [Reset]
    Right succe -> do
      putStr $ name t
      putStr $ ": " ++ succe 
      setSGR [SetColor Foreground Vivid Green]
      putStrLn " ✓"
      setSGR [Reset]
  return t

reportTests :: [Test] -> IO ()
reportTests ts = do
  tests <- sequence $ map runTest ts
  let lt = length tests
  let passedtests = filter 
                    (\test -> case outcome test of 
                      Left _ -> False
                      Right _ -> True)
                      tests
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  case passedAll of
       True -> do
         putStrLn $ "Passed all " ++ (show lt) ++ " tests!! 🎉"
       False -> do
         putStrLn $ "Failed "  ++ (show failedTests) ++ " test(s) 😣"
         exitFailure

-- | Run tests with IO
reportTestsIO :: [IO Test] -> IO ()
reportTestsIO ts = do
  putStrLn "Running tests"
  testsIO <- sequence ts
  putStrLn "Reporting tests"
  tests <- sequence $ map runTest testsIO
  let lt = length tests
  let passedtests = filter 
                    (\test -> case outcome test of 
                      Left _ -> False
                      Right _ -> True)
                      tests
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  case passedAll of
       True -> do
         putStrLn $ "Passed all " ++ (show lt) ++ " tests!! 🎉"
       False -> do
         putStrLn $ "Failed "  ++ (show failedTests) ++ " test(s) 😣"
         exitFailure
