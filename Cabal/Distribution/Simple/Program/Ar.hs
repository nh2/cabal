-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ar
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ar@ program.

module Distribution.Simple.Program.Ar (
    createArLibArchive,
    multiStageProgramInvocation,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..) )
import Distribution.Simple.Program.Run
         ( programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
         ( Verbosity, deafening, verbose )
import System.Directory
         ( copyFile, doesFileExist, removeFile )
import System.FilePath
         ( (<.>) )

-- | Call @ar@ to create a library archive from a bunch of object files.
--
createArLibArchive :: Verbosity -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
createArLibArchive verbosity ar target files = do

  -- The args to use with "ar" are actually rather subtle and system-dependent.
  -- In particular we have the following issues:
  --
  --  -- On OS X, "ar q" does not make an archive index. Archives with no
  --     index cannot be used.
  --
  --  -- GNU "ar r" will not let us add duplicate objects, only "ar q" lets us
  --     do that. We have duplicates because of modules like "A.M" and "B.M"
  --     both make an object file "M.o" and ar does not consider the directory.
  --
  -- Our solution is to use "ar r" in the simple case when one call is enough.
  -- When we need to call ar multiple times we use "ar q" and for the last
  -- call on OSX we use "ar qs" so that it'll make the index.
  --
  -- In all cases we use "ar D" for deterministic mode. This will prevent "ar"
  -- from including a timestamp, which would generate different outputs for
  -- same inputs and break re-linking avoidance.
  --
  -- If there is an old target file and the are produces the very same output,
  -- we avoid touching the old target file to help tools like GHC and make
  -- exiting early.

  let simpleArgs  = case buildOS of
             OSX -> ["-D", "-r", "-s"]
             _   -> ["-D", "-r"]

      initialArgs = ["-D", "-q"]
      finalArgs   = case buildOS of
             OSX -> ["-D", "-q", "-s"]
             _   -> ["-D", "-q"]

      tmpTarget   = target <.> "tmp"

      extraArgs   = verbosityOpts verbosity ++ [tmpTarget]

      simple  = programInvocation ar (simpleArgs  ++ extraArgs)
      initial = programInvocation ar (initialArgs ++ extraArgs)
      middle  = initial
      final   = programInvocation ar (finalArgs   ++ extraArgs)

  -- Delete old .a.tmp file (we use -r, which fails if the file is malformed)
  tmpExists <- doesFileExist tmpTarget
  when tmpExists $ removeFile tmpTarget

  sequence_
    [ runProgramInvocation verbosity inv
    | inv <- multiStageProgramInvocation
               simple (initial, middle, final) files ]

  -- If this "ar" invocation has actually created something new,
  -- copy the temporary file to the target.

  writeTarget <- do
    oldExists <- doesFileExist target
    if not oldExists then return True
                     -- Lazy IO comparison
                     else (/=) <$> BSL.readFile target
                               <*> BSL.readFile tmpTarget

  when writeTarget $ copyFile tmpTarget target

  where
    verbosityOpts v | v >= deafening = ["-v"]
                    | v >= verbose   = []
                    | otherwise      = ["-c"]
