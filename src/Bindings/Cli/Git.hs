{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Bindings.Cli.Git
  ( CommitId
  , gitProc
  , ensureCleanGitRepo
  , readGitProcess
  , isolateGitProc
  , gitProcNoRepo
  , gitLsRemote
  , gitLookupDefaultBranch
  , gitLookupCommitForRef
  , GitRef (..)
  ) where

import Control.Applicative hiding (many)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Log
import Data.Bool (bool)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Exit (ExitCode)
import qualified Text.Megaparsec.Char.Lexer as ML
import Text.Megaparsec as MP
import Text.Megaparsec.Char as MP

import Cli.Extras

-- Check whether the working directory is clean
checkGitCleanStatus
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     )
  => FilePath
  -> Bool
  -> m Bool
checkGitCleanStatus repo withIgnored = do
  let
    runGit = readProcessAndLogStderr Debug . gitProc repo
    gitStatus = runGit $ ["status", "--porcelain"] <> bool [] ["--ignored"] withIgnored
    gitDiff = runGit ["diff"]
  T.null <$> liftA2 (<>) gitStatus gitDiff

-- | Ensure that git repo is clean
ensureCleanGitRepo
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     , HasCliConfig m
     , MonadMask m
     , AsUnstructuredError e
     )
  => FilePath
  -> Bool
  -> Text
  -> m ()
ensureCleanGitRepo path withIgnored s =
  withSpinnerNoTrail ("Ensuring clean git repo at " <> T.pack path) $ do
    checkGitCleanStatus path withIgnored >>= \case
      False -> do
        statusDebug <- readGitProcess path $ ["status"] <> bool [] ["--ignored"] withIgnored
        putLog Warning "Working copy is unsaved; git status:"
        putLog Notice statusDebug
        failWith s
      True -> pure ()

gitProcNoRepo :: [String] -> ProcessSpec
gitProcNoRepo args = setEnvOverride (M.singleton "GIT_TERMINAL_PROMPT" "0" <>) $ proc "git" args

gitProc :: FilePath -> [String] -> ProcessSpec
gitProc repo = gitProcNoRepo . runGitInDir
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

isolateGitProc :: ProcessSpec -> ProcessSpec
isolateGitProc = setEnvOverride (overrides <>)
  where
    overrides = M.fromList
      [ ("HOME", "/dev/null")
      , ("GIT_CONFIG_NOSYSTEM", "1")
      , ("GIT_TERMINAL_PROMPT", "0") -- git 2.3+
      , ("GIT_ASKPASS", "echo") -- pre git 2.3 to just use empty password
      , ("GIT_SSH_COMMAND", "ssh -o PreferredAuthentications password -o PubkeyAuthentication no -o GSSAPIAuthentication no")
      ]

readGitProcess
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     )
  => FilePath -> [String] -> m Text
readGitProcess repo = readProcessAndLogOutput (Debug, Notice) . gitProc repo

gitLookupDefaultBranch :: GitLsRemoteMaps -> Either Text Text
gitLookupDefaultBranch (refs, _) = do
  ref <- case M.lookup GitRef_Head refs of
    Just ref -> pure ref
    Nothing -> throwError
      "No symref entry for HEAD. \
      \ Is your git version at least 1.8.5? \
      \ Otherwise `git ls-remote --symref` will not work."
  case ref of
    GitRef_Branch b -> pure b
    _ -> throwError $
      "Default ref " <> showGitRef ref <> " is not a branch!"

gitLookupCommitForRef :: GitLsRemoteMaps -> GitRef -> Either Text CommitId
gitLookupCommitForRef (_, commits) ref = case M.lookup ref commits of
  Just a -> pure a
  Nothing -> throwError $ "Did not find commit for " <> showGitRef ref

gitLsRemote
  :: ( MonadIO m
     , MonadLog Output m
     , MonadError e m
     , AsProcessFailure e
     , MonadFail m
     , AsUnstructuredError e
     )
  => String
  -> Maybe GitRef
  -> Maybe String
  -> m (ExitCode, GitLsRemoteMaps)
gitLsRemote repository mRef mBranch = do
  (exitCode, out, _err) <- case mBranch of
    Nothing -> readCreateProcessWithExitCode $ gitProcNoRepo $
        ["ls-remote", "--exit-code", "--symref", repository]
        ++ maybeToList (T.unpack . showGitRef <$> mRef)
    Just branchName -> readCreateProcessWithExitCode $ gitProcNoRepo
        ["ls-remote", "--exit-code", repository, branchName]
  let t = T.pack out
  maps <- case MP.runParser parseLsRemote "" t of
    Left err -> failWith $ T.pack $ MP.errorBundlePretty err
    Right table -> pure $ bimap M.fromList M.fromList $ partitionEithers table
  putLog Debug $ "git ls-remote maps: " <> T.pack (show maps)
  pure (exitCode, maps)

lexeme :: Parsec Void Text a -> Parsec Void Text a
lexeme = ML.lexeme $ void $ MP.takeWhileP (Just "within-line white space") $
  flip elem [' ', '\t']

-- $ git ls-remote --symref git@github.com:obsidiansystems/obelisk.git HEAD
-- ref: refs/heads/master	HEAD
-- d0a8d25dc93f0acd096bc4ff2f550da9e2d0c8f5	refs/heads/master
parseLsRemote :: Parsec Void Text [Either (GitRef, GitRef) (GitRef, CommitId)]
parseLsRemote =
  many ((fmap Left (try parseRef) <|> fmap Right parseCommit) <* try MP.eol) <* MP.eof
  where
    parseRef :: Parsec Void Text (GitRef, GitRef)
    parseRef = MP.label "ref and symbolic ref" $ do
      _ <- lexeme "ref:"
      ref <- lexeme $ MP.takeWhileP (Just "ref") $ not . isSpace
      symbolicRef <- lexeme $ MP.takeWhileP (Just "symbolic ref") $ not . isSpace
      return (toGitRef symbolicRef, toGitRef ref)
    parseCommit :: Parsec Void Text (GitRef, CommitId)
    parseCommit = MP.label "commit and ref" $ do
      commitId <- lexeme $ MP.takeWhileP (Just "commit id") $ not . isSpace
      ref <- lexeme $ MP.takeWhileP (Just "ref") $ not . isSpace
      return (toGitRef ref, commitId)

data GitRef
  = GitRef_Head
  | GitRef_Branch Text
  | GitRef_Tag Text
  | GitRef_Other Text
  deriving (Show, Eq, Ord)

showGitRef :: GitRef -> Text
showGitRef = \case
  GitRef_Head -> "HEAD"
  GitRef_Branch x -> "refs/heads/" <> x
  GitRef_Tag x -> "refs/tags/" <> x
  GitRef_Other x -> x

toGitRef :: Text -> GitRef
toGitRef = \case
  "HEAD" -> GitRef_Head
  r -> if
    | Just s <- "refs/heads/" `T.stripPrefix` r -> GitRef_Branch s
    | Just s <- "refs/tags/" `T.stripPrefix` r -> GitRef_Tag s
    | otherwise -> GitRef_Other r

type CommitId = Text

type GitLsRemoteMaps = (Map GitRef GitRef, Map GitRef CommitId)
