{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

    import GhcDriver
    import CoreDraw
    import AnnDraw
    import Data.AnnCore
    import Text.LaTeX
    import System.Environment
    import System.Exit

    import Control.Monad
    import System.Directory

    main = do
        (inFile, outFile) <- getArgs >>= \case
            [inFName] -> return (inFName, (inFName ++ ".tex"))
            [inFName, outFName] -> return (inFName, outFName)
            _ -> printHelp >> exitSuccess

        core <- compileToCore inFile
        let doc :: LaTeX = (texy.annotateModule) core
        renderFile outFile doc
        
    printHelp :: IO ()
    printHelp = putStrLn "CoreDraw v0.1 \n\t -- render core representation after simplification of a haskell file as a LaTeX document"