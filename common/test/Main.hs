module Main (main) where

import System.Exit (exitFailure)

import qualified TestPlacementValidation

main :: IO ()
main = do
    let failedTests =
            [ name
            | (name, passed) <- TestPlacementValidation.tests
            , not passed
            ]
    if null failedTests
        then putStrLn "All placement validation tests passed."
        else do
            putStrLn "Placement validation tests failed:"
            mapM_ (putStrLn . ("  - " ++)) failedTests
            exitFailure
