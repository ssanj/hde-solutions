{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified PD1    as PD1
import qualified PD2.IO as PD2
import qualified PD2    as PD2

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.List.NonEmpty ((<|))

main :: IO ()
main = pd2

pd2 :: IO ()
pd2 = do
  let configDir   = error "define your paths"
      checkoutDir = error "define your paths"
      repoPath    = error "define your paths"
      projectDir  = PD2.ProjectDir checkoutDir repoPath
  result <- PD2.projectSetup PD2.projectOpsIO configDir projectDir PD2.languageMappings
  T.putStrLn ("result:" <> result)

pd1 :: IO ()
pd1 = do
  let path = error "define your path"
  result <- PD1.findLanguage (PD1.ProjectDir path) PD1.languageMappings PD1.findFilesInDirIO
  putStrLn $ maybe "No languages found" show result


