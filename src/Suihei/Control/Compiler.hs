{-# LANGUAGE DefaultSignatures #-}

module Suihei.Control.Compiler (
    Compile (..),
) where

import Suihei.Data.Abstract (File)
import Suihei.Data.Chordal (Key)

import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)

import System.IO (withFile, IOMode (..))

class Compile a where
    compile  :: a -> Key -> Doc ann
    cout     :: a -> Key -> File -> IO ()

    default cout     :: a -> Key -> File -> IO ()
    cout a key file'
        | Nothing   <- file' = withFile "a.ly"          WriteMode (`hPutDoc` doc)
        | Just file <- file' = withFile (file ++ ".ly") WriteMode (`hPutDoc` doc)
        where doc = compile a key
    {-# MINIMAL compile #-}