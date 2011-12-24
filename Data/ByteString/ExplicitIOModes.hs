{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.ExplicitIOModes
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the bytestring IO operations to handles with explicit
-- IOModes.
--
-------------------------------------------------------------------------------

module Data.ByteString.ExplicitIOModes
    ( hGetLine
    , hGetContents
    , hGet
#if MIN_VERSION_bytestring(0,9,1)
    , hGetSome
#endif
    , hGetNonBlocking

    , hPut
#if MIN_VERSION_bytestring(0,9,2)
    , hPutNonBlocking
#endif
    , hPutStr
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Int  ( Int )
import System.IO ( IO )

-- from bytestring:
import Data.ByteString                ( ByteString )
import qualified Data.ByteString as B ( hGetLine
                                      , hGetContents
                                      , hGet
#if MIN_VERSION_bytestring(0,9,1)
                                      , hGetSome
#endif
                                      , hGetNonBlocking

                                      , hPut
#if MIN_VERSION_bytestring(0,9,2)
                                      , hPutNonBlocking
#endif
                                      , hPutStr
                                      )

-- from explicit-iomodes:
import System.IO.ExplicitIOModes        ( Handle, ReadModes, WriteModes )
import System.IO.ExplicitIOModes.Unsafe ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- ByteString I/O with file handles with explicit IOModes
-------------------------------------------------------------------------------

-- | Wraps @Data.ByteString.@'B.hGetLine'.
hGetLine ∷ ReadModes ioMode ⇒ Handle ioMode → IO ByteString
hGetLine = wrap B.hGetLine

-- | Wraps @Data.ByteString.@'B.hGetContents'.
hGetContents ∷ ReadModes ioMode ⇒ Handle ioMode → IO ByteString
hGetContents = wrap B.hGetContents

-- | Wraps @Data.ByteString.@'B.hGet'.
hGet ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO ByteString
hGet = wrap2 B.hGet

#if MIN_VERSION_bytestring(0,9,1)
-- | Wraps @Data.ByteString.@'B.hGetSome'.
hGetSome ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO ByteString
hGetSome = wrap2 B.hGetSome
#endif

-- | Wraps @Data.ByteString.@'B.hGetNonBlocking'.
hGetNonBlocking ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO ByteString
hGetNonBlocking = wrap2 B.hGetNonBlocking


-- | Wraps @Data.ByteString.@'B.hPut'.
hPut ∷ WriteModes ioMode ⇒ Handle ioMode → ByteString → IO ()
hPut = wrap2 B.hPut

#if MIN_VERSION_bytestring(0,9,2)
-- | Wraps @Data.ByteString.@'B.hPutNonBlocking'.
hPutNonBlocking ∷ WriteModes ioMode ⇒ Handle ioMode → ByteString → IO ByteString
hPutNonBlocking = wrap2 B.hPutNonBlocking
#endif

-- | Wraps @Data.ByteString.@'B.hPutStr'.
hPutStr ∷ WriteModes ioMode ⇒ Handle ioMode → ByteString → IO ()
hPutStr = wrap2 B.hPutStr


-- The End ---------------------------------------------------------------------
