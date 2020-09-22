{-# LANGUAGE OverloadedStrings #-}

module Main where

import PD1

main :: IO ()
main = do
  let path = error "define your path"
  result <- findLanguage (ProjectDir path) languageMappings findFilesInDirIO
  putStrLn $ maybe "No languages found" show result
