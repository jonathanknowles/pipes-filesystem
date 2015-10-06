{-# LANGUAGE LambdaCase #-}

module Pipes.FileSystem where

import Control.Applicative ((<|>))
import Control.Monad
    ( when )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist )
import System.FilePath.Posix ((</>))
import Pipes
    ( (>->)
    , lift
    , liftIO
    , yield )

import qualified Control.Monad          as M

import qualified Pipes                  as P
import qualified Pipes.Prelude          as P
import qualified Pipes.Safe             as PS
import qualified System.Directory       as D
import qualified System.Posix.Directory as PD

delete :: FilePath -> IO ()
delete path =
    readFileType path >>= \case
        Just Directory -> D.removeDirectory path
        Just File -> D.removeFile path
        Nothing -> pure ()

deleteRecursively :: FilePath -> IO ()
deleteRecursively path =
    PS.runSafeT $ P.runEffect $ objectsToDelete >-> P.mapM_ (liftIO . delete) where
        objectsToDelete = P.enumerate $
            descendants LeafToRoot path

readFileType :: FilePath -> IO (Maybe FileType)
readFileType p =
    ifM (D.doesDirectoryExist p) (pure $ Just Directory) $
    ifM (D.doesFileExist p) (pure $ Just File) (pure Nothing)

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

children :: PS.MonadSafe m => FilePath -> P.ListT m FilePath
children path = P.Select $ do
    canRead <- liftIO $ D.readable <$> D.getPermissions path
    M.when canRead $ PS.bracket openStream closeStream readStream
        >-> P.filter (not . isCurrentOrParentDirectory)
        >-> P.map (path </>)
    where
        openStream = liftIO $ PD.openDirStream path
        closeStream = liftIO . PD.closeDirStream
        readStream stream = do
            p <- liftIO $ PD.readDirStream stream
            M.unless (null p) $ yield p >> readStream stream

descendants :: PS.MonadSafe m =>
    TraversalOrder -> FilePath -> P.ListT m FilePath
descendants order path =
    liftIO (readFileType path) >>= \case
        Nothing   -> P.mzero
        Just File -> pure path
        Just Directory ->
            case order of
                RootToLeaf -> pure path <|> ds
                LeafToRoot -> ds <|> pure path
    where ds = children path >>= descendants order

onlyDirectories :: P.MonadIO m => P.Pipe FilePath FilePath m r
onlyDirectories = M.forever $ P.await >>= \p ->
    whenM (liftIO $ D.doesDirectoryExist p) (P.yield p)

onlyFiles :: P.MonadIO m => P.Pipe FilePath FilePath m r
onlyFiles = M.forever $ P.await >>= \p ->
    whenM (liftIO $ D.doesFileExist p) (P.yield p)

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

