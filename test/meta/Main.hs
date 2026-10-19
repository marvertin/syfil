import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad (forM_, unless)
import           Data.List (intercalate)
import qualified Data.ByteString as BS
import           Data.Yaml (encode)

import           Syfil.Data.Slicin

main :: IO ()
main = do
  forM_ ["#version1", "#yaba1"] $ \header ->
    forM_ ["\n", "\r\n"] $ \newline ->
      forM_ [False, True] $ \finalNewline ->
        forM_ [Delete, Link "/2018-09|001/dropbox/příliš žluťoučký.txt"] $ \command -> do
          let content = intercalate newline [header, show command]
                     ++ if finalNewline then newline else ""
              parsed = parseMetaFile content
          unless (show parsed == show command) $
            fail ("Incorrect metadata command: " ++ show content)
          -- Index writing must also force the parsed command without throwing.
          _ <- evaluate (BS.length (encode (MetaFile parsed)))
          return ()

  -- Old Windows metadata includes diagnostic text after the command.
  let legacy = "#yaba1\r\nLink \"/2018-09|001/dropbox/file.zip.aes\"\r\n"
            ++ "\r\n----- Operation ---------------------------\r\n~INS_NEW~\r\n"
  unless (show (parseMetaFile legacy) == show (Link "/2018-09|001/dropbox/file.zip.aes")) $
    fail "Cannot read legacy Windows metadata"

  forM_ ["", "#unknown\r\nDelete\r\n", "#version1\r\n"] $ \content -> do
    result <- try (evaluate (length (show (parseMetaFile content)))) :: IO (Either SomeException Int)
    case result of
      Left _ -> return ()
      Right _ -> fail ("Accepted invalid metadata: " ++ show content)

  putStrLn "Metadata regression tests passed"
