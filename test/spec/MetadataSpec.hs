module MetadataSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Yaml (decodeEither', encode)
import Test.Hspec
import Test.QuickCheck
import Syfil.Data.Slicin

spec :: Spec
spec = describe "Backup metadata" $ do
  forM_ ["#version1", "#yaba1"] $ \header ->
    forM_ ["\n", "\r\n"] $ \newline ->
      forM_ [False, True] $ \finalNewline ->
        forM_ [Delete, Link "/2018-09|001/demo/příliš žluťoučký.txt"] $ \command ->
          it ("reads " ++ show (header, newline, finalNewline, command)) $ do
            let content = intercalate newline [header, show command]
                       ++ if finalNewline then newline else ""
                parsed = parseMetaFile content
            parsed `shouldBe` command
            evaluate (BS.length (encode (MetaFile parsed))) `shouldReturn`
              BS.length (encode (MetaFile command))

  it "accepts diagnostic text following legacy Windows metadata" $ do
    let text = "#yaba1\r\nLink \"/2018-09|001/demo/file.zip.aes\"\r\n"
            ++ "\r\n----- Operation ---------------------------\r\n~INS_NEW~\r\n"
    parseMetaFile text `shouldBe` Link "/2018-09|001/demo/file.zip.aes"

  forM_ ["", "#unknown\r\nDelete\r\n", "#version1\r\n",
         "#version1\nNotACommand\n"] $ \content ->
    it ("rejects invalid metadata " ++ show content) $
      evaluate (length (show (parseMetaFile content))) `shouldThrow` anyErrorCall

  it "round-trips generated link paths through current and legacy text formats" $
    property $ \path ->
      conjoin [parseMetaFile (intercalate newline [header, show (Link path)]) === Link path
              | header <- ["#version1", "#yaba1"], newline <- ["\n", "\r\n"]]

  it "round-trips generated link paths through the YAML index format" $
    property $ \path ->
      case decodeEither' (encode (MetaFile (Link path))) of
        Left err -> counterexample (show err) False
        Right decoded -> decoded === MetaFile (Link path)
