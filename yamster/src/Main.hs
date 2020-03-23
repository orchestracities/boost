{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Development.Shake (shakeArgs, shakeOptions)
import System.Directory (canonicalizePath)

import Mesh.Make.Deploy


repoRoot ∷ IO FilePath
repoRoot = canonicalizePath "../"
-- TODO. Root dir should be a CLI arg.
-- For now we're just assuming you're always running the build like
-- this:
--     $ cd yamster
--     $ stack run yamster
--

main ∷ IO ()
main = do
  root ← repoRoot
  shakeArgs shakeOptions $ do
    deploymentFiles root
