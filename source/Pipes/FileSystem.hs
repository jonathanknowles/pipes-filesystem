{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipes.FileSystem where

import Control.Applicative
    ( (<|>) )
import Control.Monad
    ( (>=>) )
import Data.Monoid
    ( (<>)
    , mempty )
import Data.Word
    ( Word32 )
import Pipes
    ( (>->)
    , (<-<)
    , liftIO
    , yield
    , Producer )
import Pipes.Safe
    ( MonadSafe )

import Prelude hiding
    ( FilePath )

import qualified Control.Monad                      as M
import qualified Data.ByteString                    as B
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P
import qualified Pipes.Safe                         as PS
import qualified System.Posix.ByteString            as S
import qualified System.Posix.Directory.ByteString  as S
import qualified System.Posix.Files.ByteString      as S

type FilePath = S.RawFilePath

data FileType
    = File
    | Directory
    | NamedPipe
    | LocalDomainSocket
    | CharacterDevice
    | BlockDevice
    | SymbolicLink
    | Unknown
    deriving (Eq, Show)

data FileInfo = FileInfo
    { filePath :: !FilePath
    , fileType :: !FileType }

isDirectory :: FileInfo -> Bool
isDirectory = (== Directory) . fileType
{-# INLINE isDirectory #-}

isFile :: FileInfo -> Bool
isFile = (== File) . fileType
{-# INLINE isFile #-}

isSymbolicLink :: FileInfo -> Bool
isSymbolicLink = (== SymbolicLink) . fileType
{-# INLINE isSymbolicLink #-}

decodeFileType :: Word32 -> FileType
decodeFileType = \case
    01 -> NamedPipe
    02 -> CharacterDevice
    04 -> Directory
    06 -> BlockDevice
    08 -> File
    10 -> SymbolicLink
    12 -> LocalDomainSocket
    _  -> Unknown
{-# INLINE decodeFileType #-}

data TraversalOrder
    = RootToLeaf
    | LeafToRoot

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

children :: MonadSafe m
    => FilePath
    -> Producer FileInfo m ()
children path = PS.bracket open close read where
    addPrefix p = path <> "/" <> p
    open = liftIO $ S.openDirStream path
    close stream = liftIO $ S.closeDirStream stream
    read stream = do
        (t, p) <- liftIO $ S.readDirStream' stream
        M.unless (B.null p) $ do
            M.unless (isCurrentOrParentDirectory p) $ yield $
                FileInfo (addPrefix p) (decodeFileType t)
            read stream
{-# INLINE children #-}

descendants :: MonadSafe m
    => TraversalOrder
    -> FilePath
    -> Producer FileInfo m ()
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

descendantFiles :: MonadSafe m
    => TraversalOrder
    -> FilePath
    -> Producer FileInfo m ()
descendantFiles order path =
    P.filter isFile <-< descendants order path

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

