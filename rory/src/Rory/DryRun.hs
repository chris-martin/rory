module Rory.DryRun (dryRun) where

import Rory.Args (Args)

dryRun :: Args -> IO ()
dryRun args = do
    putStrLn "Not doing anything because the flag --dry-run was given. If this"
    putStrLn "weren't a dry run, Rory would run with the following arguments:"
    putStrLn ""
    putStrLn $ show args
