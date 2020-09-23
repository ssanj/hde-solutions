{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified PD1 as PD1

main :: IO ()
main = pd1

pd1 :: IO ()
pd1 = do
  let path = error "define your path"
  result <- PD1.findLanguage (PD1.ProjectDir path) PD1.languageMappings PD1.findFilesInDirIO
  putStrLn $ maybe "No languages found" show result


