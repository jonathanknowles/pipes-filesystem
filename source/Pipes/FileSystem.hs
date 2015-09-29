module Pipes.FileSystem where

import Control.Applicative ((<|>))
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

-- Example:
--
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

data FileType = File | Directory deriving Show

isDirectory :: FileInfo -> Bool
isDirectory (FileInfo Directory _) = True
isDirectory _ = False

filePath :: FileInfo -> FilePath
filePath (FileInfo _ path) = path

data FileInfo = FileInfo FileType FilePath deriving Show

data TraverseOrder = RootToLeaf | LeafToRoot

isCurrentDirectory = (==) "."
isParentDirectory = (==) ".."

isCurrentOrParentDirectory p =
    isCurrentDirectory p ||
    isParentDirectory p

readFileInfo :: PS.MonadSafe m => FilePath -> m FileInfo
readFileInfo f = do
    isDirectory <- liftIO $ D.doesDirectoryExist f
    return $ FileInfo (if isDirectory then Directory else File) f

children :: PS.MonadSafe m => FilePath -> P.ListT m FileInfo
children path = P.Select $ do
    canRead <- liftIO $ D.readable <$> D.getPermissions path
    M.when canRead $ PS.bracket before after loop
        >-> P.filter (not . isCurrentOrParentDirectory)
        >-> P.map (path </>)
        >-> P.mapM readFileInfo
    where
        before = liftIO $ PD.openDirStream path
        after = liftIO . PD.closeDirStream
        loop stream = do
            file <- liftIO $ PD.readDirStream stream
            M.unless (null file) $ yield file >> loop stream

-- Surely this should also return the supplied path.
descendants :: PS.MonadSafe m => TraverseOrder -> FilePath -> P.ListT m FileInfo
descendants RootToLeaf path = do
    child <- children path
    if isDirectory child
        then pure child <|> descendants RootToLeaf (filePath child)
        else pure child
descendants LeafToRoot path = do
    child <- children path
    if isDirectory child
        then descendants RootToLeaf (filePath child) <|> pure child
        else pure child

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f = p >>= bool f t

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = ifM p m (return ())

bool :: a -> a -> Bool -> a
bool f t p = if' p t f

if' :: Bool -> a -> a -> a
if' p t f = if p then t else f

