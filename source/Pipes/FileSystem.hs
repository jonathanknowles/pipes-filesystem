{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipes.FileSystem where

import Control.Applicative                  ( (<|>) )
import Control.Monad                        ( when )
import Data.Monoid                          ( (<>)
                                            , mempty )
import Prelude                       hiding ( FilePath )
import System.Posix.ByteString              ( RawFilePath )
import System.Posix.Files.ByteString        ( FileStatus
                                            , getFileStatus )
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
deleteRecursively path = undefined
    --PS.runSafeT $ P.runEffect $ objectsToDelete >-> P.mapM_ (liftIO . delete) where
    --    objectsToDelete = P.enumerate $
    --        descendants LeafToRoot path

fileType :: FileStatus -> FileType
fileType s
    | PF.isDirectory   s = Directory
    | PF.isRegularFile s = File
    | otherwise          = Unknown

readFileType :: FilePath -> IO (Maybe FileType)
readFileType p = do
    s <- getFileStatus p
    pure $
        if PF.isDirectory   s then Just Directory else
        if PF.isRegularFile s then Just File      else Nothing

isDirectory :: FileInfo -> Bool
isDirectory = PF.isDirectory . fileStatus

isFile :: FileInfo -> Bool
isFile = PF.isRegularFile . fileStatus

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist p = readFileType p >>= \case
    Just Directory -> pure True
    _ -> pure False

doesFileExist :: FilePath -> IO Bool
doesFileExist p = readFileType p >>= \case
    Just File -> pure True
    _ -> pure False

data FileType = File | Directory | Unknown deriving Show

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

data FileInfo = FileInfo
    { filePath   :: !FilePath
    , fileStatus :: !FileStatus }

-- TODO: reinstate check here
canReadDir :: FilePath -> IO Bool
canReadDir path = pure True
    -- D.readable <$> D.getPermissions path

children :: PS.MonadSafe m => FilePath -> P.ListT m FilePath
children path = P.Select $ do
    let pathWithTail = path <> "/"
    canRead <- liftIO $ canReadDir path
    M.when canRead $ PS.bracket open close read
        >-> P.filter (not . isCurrentOrParentDirectory)
        >-> P.map (pathWithTail <>)
    where
        open = liftIO $ PD.openDirStream path
        close = liftIO . PD.closeDirStream
        read stream = do
            p <- liftIO $ PD.readDirStream stream
            M.unless (B.null p) $ yield p >> read stream

descendants :: PS.MonadSafe m => TraversalOrder -> FilePath -> P.ListT m FileInfo
descendants = \case
        RootToLeaf -> rtl
        LeafToRoot -> ltr
    where
        ltr p = liftIO (getFileStatus p) >>= \s -> case fileType s of
            Directory -> (children p >>= ltr) <|> pure (FileInfo p s)
            _         ->                          pure (FileInfo p s)
        rtl p = liftIO (getFileStatus p) >>= \s -> case fileType s of
            Directory -> pure (FileInfo p s) <|> (children p >>= rtl)
            _         -> pure (FileInfo p s)

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

