{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified AST
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Char                    hiding (Format)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Data.Version
import           Options.Applicative.Simple
import           Parser
import           Pipes
import qualified Pipes.Prelude                as Pipes
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

data Layout
  = Compact
  | Format
  deriving Show

data Args = Args
  { layout :: Layout
  , target :: FilePath
  } deriving Show

baseConfig :: Config
baseConfig = defConfig
  { confCompare = keyOrder
    [ "file", "code" , "kind"      , "name"
    , "type", "index", "properties", "objects"
    ]
  }

newConfig :: Layout -> Config
newConfig Compact = baseConfig { confIndent = Spaces 0 }
newConfig Format  = baseConfig { confIndent = Spaces 2 }

version :: String
version = $(simpleVersion $ fst . last $ readP_to_S parseVersion CURRENT_PACKAGE_VERSION)

parseOptions :: IO (Args, ())
parseOptions = simpleOptions version empty
  "Converts a Delphi Form File (DFM/SFM) to JSON"
  ( Args
    <$> flag Format Compact
      (  long    "compact"
      <> short   'c'
      <> help    "Compact JSON output" )
    <*> strArgument
      (  help    "FILE or DIRECTORY for conversion"
      <> metavar "PATH") )
  empty

-- 支持多扩展名
isExts :: [String] -> FilePath -> Bool
isExts exts fp = any (\e -> map toLower (takeExtension fp) == '.':e) exts

writeJSON :: ToJSON a => Config -> (FilePath, a) -> IO ()
writeJSON cfg (filePath, obj)
  = ByteString.writeFile (filePath -<.> "json")
  . encodePretty' cfg
  . toJSON
  $ obj

readDFM :: FilePath -> IO (FilePath, Text)
readDFM p = do
  Text.putStrLn . Text.pack $ "Reading: " ++ p
  src <- Text.readFile p
  return (p, src)

-- 扫描目录下的 .dfm/.sfm
paths :: FilePath -> Producer FilePath IO ()
paths path = do
  isFile <- lift $ doesFileExist path
  if isFile
    then yield path
    else do
      ps <- lift $ listDirectory path
      mapM_ (paths . (path </>)) ps

parse :: Pipe (FilePath, Text) (FilePath, AST.Object) IO ()
parse = forever $ do
  (path, src) <- await
  case parseDFM path src of
    Left  err -> lift $ Text.putStrLn err
    Right ast -> yield (path, ast)

-- 核心逻辑：区分「单文件」与「目录」
process :: FilePath -> Config -> IO ()
process tgt cfg = do
  absPath <- canonicalizePath tgt
  isFile  <- doesFileExist absPath
  if isFile
    -- ========== 单文件模式 ==========
    then do
      let dir = takeDirectory absPath
      setCurrentDirectory dir
      txt <- Text.readFile (takeFileName absPath)
      case parseDFM (takeFileName absPath) txt of
        Left err -> Text.putStrLn err
        Right ast-> writeJSON cfg (absPath, ast)
    -- ========== 目录递归模式 ==========
    else do
      setCurrentDirectory absPath
      runEffect
        $   paths absPath
        >-> Pipes.filter (isExts ["dfm","sfm"])
        >-> Pipes.map (makeRelative absPath)
        >-> Pipes.mapM readDFM
        >-> parse
        >-> Pipes.mapM_ (writeJSON cfg)

main :: IO ()
main = do
  (args, ()) <- parseOptions
  let cfg = newConfig . layout $ args
      tgt = target args
  if isValid tgt
    then process tgt cfg
    else putStrLn $ "Invalid path: " ++ tgt
