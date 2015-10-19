{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipes.FileSystem where

import Control.Applicative                  ( (<|>) )
import Control.Monad                        ( when )
import Data.Monoid                          ( (<>) )
import Prelude                       hiding ( FilePath )
import System.Posix.ByteString              ( RawFilePath )
import System.Posix.Files.ByteString        ( getFileStatus )
import Pipes                                ( (>->)
                                            , liftIO
                                            , yield )

import qualified Control.Monad                      as M
import qualified Data.ByteString                    as B
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P
import qualified Pipes.Safe                         as PS
import qualified System.Directory                   as D
import qualified System.Posix.Directory.ByteString  as PD
import qualified System.Posix.Files.ByteString      as PF

type FilePath = RawFilePath

-- TODO: Fix race condition here.
delete :: FilePath -> IO ()
delete path = readFileType path >>= \case
    Just Directory -> PD.removeDirectory path
    Just File -> PF.removeLink path
    Nothing -> pure ()

deleteRecursively :: FilePath -> IO ()
deleteRecursively path =
    PS.runSafeT $ P.runEffect $ objectsToDelete >-> P.mapM_ (liftIO . delete) where
        objectsToDelete = P.enumerate $
            descendants LeafToRoot path

readFileType :: FilePath -> IO (Maybe FileType)
readFileType p = do
    s <- getFileStatus p
    pure $
        if PF.isDirectory   s then Just Directory else
        if PF.isRegularFile s then Just File      else Nothing

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist p = readFileType p >>= \case
    Just Directory -> pure True
    _ -> pure False

doesFileExist :: FilePath -> IO Bool
doesFileExist p = readFileType p >>= \case
    Just File -> pure True
    _ -> pure False

data FileType = File | Directory deriving Show

data TraversalArgs = TraversalArgs
    { traversalOrder :: TraversalOrder }

--  r
--  ├── a
--  │   ├── f
--  │   └── g
--  ├── b
--  │   ├── f
--  │   └── g
--  ├── f
--  └── g
--
--  leaf-to-root order:  [r, ra, raf, rag, rb, rbf, rbg, rf, rg]
--  root-to-leaf order:  [raf, rag, ra, rbf, rbg, rb, rf, rg, r]

data TraversalOrder = RootToLeaf | LeafToRoot

-- TODO: reinstate check here
canReadDir :: FilePath -> IO Bool
canReadDir path = pure True
    -- D.readable <$> D.getPermissions path

children :: PS.MonadSafe m => FilePath -> P.ListT m FilePath
children path = P.Select $ do
    let pathWithTail = path <> "/"
    canRead <- liftIO $ canReadDir path
    M.when canRead $ PS.bracket openStream closeStream readStream
        >-> P.filter (not . isCurrentOrParentDirectory)
        >-> P.map (pathWithTail <>)
    where
        openStream = liftIO $ PD.openDirStream path
        closeStream = liftIO . PD.closeDirStream
        readStream stream = do
            p <- liftIO $ PD.readDirStream stream
            M.unless (B.null p) $ yield p >> readStream stream

descendants :: PS.MonadSafe m =>
    TraversalOrder -> FilePath -> P.ListT m FilePath
descendants LeafToRoot = descendantsLeafToRoot
descendants RootToLeaf = descendantsRootToLeaf

descendantsLeafToRoot path =
    liftIO (readFileType path) >>= \case
        Just Directory -> ds <|> pure path
        Just File -> pure path
        _   -> P.mzero
    where ds = children path >>= descendantsLeafToRoot
descendantsRootToLeaf path =
    liftIO (readFileType path) >>= \case
        Just Directory -> pure path <|> ds
        Just File -> pure path
        _   -> P.mzero
    where ds = children path >>= descendantsRootToLeaf

onlyDirectories :: P.MonadIO m => P.Pipe FilePath FilePath m r
onlyDirectories = P.filterM (liftIO . doesDirectoryExist)

onlyFiles :: P.MonadIO m => P.Pipe FilePath FilePath m r
onlyFiles = P.filterM (liftIO . doesFileExist)

isCurrentDirectory :: FilePath -> Bool
isCurrentDirectory = (==) "."

isParentDirectory :: FilePath -> Bool
isParentDirectory = (==) ".."

isCurrentOrParentDirectory :: FilePath -> Bool
isCurrentOrParentDirectory p =
    isCurrentDirectory p || isParentDirectory p

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f = p >>= bool f t

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = ifM p m (return ())

bool :: a -> a -> Bool -> a
bool f t p = if' p t f

if' :: Bool -> a -> a -> a
if' p t f = if p then t else f

