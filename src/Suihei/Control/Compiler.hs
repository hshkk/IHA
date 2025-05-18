{-# LANGUAGE DefaultSignatures #-}

module Suihei.Control.Compiler (
    Compile (..),
) where

import Suihei.Data.Abstract (File)

import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)

import System.IO (withFile, IOMode (..))

class Compile a where
    compile  :: a -> Doc ann
    cout     :: a -> File -> IO ()

    default cout     :: a -> File -> IO ()
    cout a file'
        | Nothing   <- file' = withFile "a.ly"          WriteMode (`hPutDoc` doc)
        | Just file <- file' = withFile (file ++ ".ly") WriteMode (`hPutDoc` doc)
        where doc = compile a
    {-# MINIMAL compile #-}