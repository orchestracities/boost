{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Mesh.Util.Yaml
  ( toYaml
  , yamlFile
  )
where

import Prelude.Unicode
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Peml


-- | Convert PEML to YAML and write it to a byte string.
toYaml ∷ ExprBuilder → ByteString
toYaml = dumpYaml ∘ pure ∘ build

-- | Write all PEML expressions to a single file.
-- If there's more than one expression, intersperse the resulting YAML
-- docs with a YAML document start separator @---@.
yamlFile ∷ FilePath → [ExprBuilder] → IO ()
yamlFile path = B.writeFile path
              ∘ B.intercalate docStart
              ∘ fmap toYaml
  where
    docStart = "\n---\n\n"
-- NOTE. Kubectl weirdness.
-- The apply command won't recognise the YAML stream separator '...'
-- which is what the HsYAML lib puts between docs when encoding a
-- YAML stream. If you give the apply command a YAML file like this:
--
--    doc1:
--      - some
--      - bananas
--    ...
--    doc2:
--      how many: 2
--
-- then only the first doc in the stream gets loaded while the others
-- are ignored. This is why we're using '---' instead of '...' here.
