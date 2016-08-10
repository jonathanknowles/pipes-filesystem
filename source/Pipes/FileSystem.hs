{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Pipes.FileSystem
    Description : Provides functions for efficiently streaming over directory trees.
    Stability   : experimental
    Portability : Linux
-}

module Pipes.FileSystem
    ( FileInfo (..)
    , FilePath
    , FileType (..)
    , TraversalOrder (..)
    , children
    , descendants
    , descendantFiles
    , descendantDirectories
    , directories
    , files
    , isDirectory
    , isFile
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad ( (>=>) )
import Data.Monoid ( (<>), mempty )
import Data.Word ( Word32 )
import Pipes ( (>->), (<-<), liftIO, yield, Pipe, Producer )
import Pipes.Combinators ( filterMap )
import Pipes.Safe ( MonadSafe )
import System.Linux.Directory.ByteString ( FileType (..) )

import Prelude hiding ( FilePath )

import qualified Control.Monad                      as M
import qualified Data.ByteString                    as B
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P
import qualified Pipes.Safe                         as PS
import qualified System.Linux.Directory.ByteString  as S
import qualified System.Posix.ByteString            as S hiding (readDirStream)
import qualified System.Posix.Files.ByteString      as S

{-| Specifies the path and type of a file. A single 'FileInfo' object
    is produced for each iteration of a directory tree traversal. -}
data FileInfo = FileInfo
    { filePath :: !FilePath
    , fileType :: !FileType }

type FilePath = S.RawFilePath

{-| Specifies in which order to traverse a directory tree.
    Given the following tree:
    @
      r
      ├── a
      │   ├── f
      │   └── g
      ├── b
      │   ├── f
      │   └── g
      ├── f
      └── g
    @

    Specifying 'RootToLeaf' yields entries in the following order:
    > [r, ra, raf, rag, rb, rbf, rbg, rf, rg]

    Specifying 'LeafToRoot" yields entries in the following order:
    > [raf, rag, ra, rbf, rbg, rb, rf, rg, r]
-}
data TraversalOrder
    = RootToLeaf
    | LeafToRoot

{-| Iterates (non-recursively) over all children of given directory.
    The exact iteration order depends on the underlying filesystem. -}
children :: MonadSafe m
    => FilePath -> Producer FileInfo m ()
children path = PS.bracket open close read where
    addPrefix p = path <> "/" <> p
    open = liftIO $ S.openDirStream path
    close = liftIO . S.closeDirStream
    read stream = do
        (p, t) <- liftIO $ S.readDirStream stream
        M.unless (B.null p) $ do
            M.unless (isCurrentOrParentDirectory p) $ yield $
                FileInfo (addPrefix p) t
            read stream
{-# INLINE children #-}

{-| Iterates (recursively) over all descendants of the given directory.
    The exact iteration order depends on the underlying filesystem, but
    directories are traversed according to the 'TraversalOrder' argument. -}
descendants :: MonadSafe m
    => TraversalOrder -> FilePath -> Producer FileInfo m ()
descendants = \case
        RootToLeaf -> P.enumerate . rtl
        LeafToRoot -> P.enumerate . ltr
    where
    rtl = P.Select . children >=> \c ->
        if      isFile      c then pure c
        else if isDirectory c then pure c <|> rtl (filePath c)
        else                       mempty
    ltr = P.Select . children >=> \c ->
        if      isFile      c then                      pure c
        else if isDirectory c then ltr (filePath c) <|> pure c
        else                                            mempty

{-| Filter a stream of file and directory entries,
    removing everything that isn't a directory. -}
directories :: Monad m => Pipe FileInfo FilePath m r
directories = filterMap isDirectory filePath

{-| Filter a stream of file and directory entries,
    removing everything that isn't an ordinary file. -}
files :: Monad m => Pipe FileInfo FilePath m r
files = filterMap isFile filePath

{-| Iterates (recursively) over all descendant directories of the given directory. -}
descendantDirectories :: MonadSafe m
    => TraversalOrder -> FilePath -> Producer FilePath m ()
descendantDirectories = ((directories <-<) .) . descendants

{-| Iterates (recursively) over all descendant files of the given directory. -}
descendantFiles :: MonadSafe m
    => TraversalOrder -> FilePath -> Producer FilePath m ()
descendantFiles = ((files <-<) .) . descendants

-- Helper functions:

currentDirectory :: FilePath
currentDirectory = "."

parentDirectory :: FilePath
parentDirectory = ".."

isCurrentDirectory :: FilePath -> Bool
isCurrentDirectory = (==) currentDirectory
{-# INLINE isCurrentDirectory #-}

isParentDirectory :: FilePath -> Bool
isParentDirectory = (==) parentDirectory
{-# INLINE isParentDirectory #-}

isCurrentOrParentDirectory :: FilePath -> Bool
isCurrentOrParentDirectory p =
    isCurrentDirectory p || isParentDirectory p
{-# INLINE isCurrentOrParentDirectory #-}

isDirectory :: FileInfo -> Bool
isDirectory = (== Directory) . fileType
{-# INLINE isDirectory #-}

isFile :: FileInfo -> Bool
isFile = (== File) . fileType
{-# INLINE isFile #-}

isSymbolicLink :: FileInfo -> Bool
isSymbolicLink = (== SymbolicLink) . fileType
{-# INLINE isSymbolicLink #-}
