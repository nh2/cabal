{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Exception (
  catchIO,
  catchExit,
  tryIO,
  mask,
  mask_,
  ) where

import System.Exit
import qualified Control.Exception as Exception

-- For base < 4.3 (GHC 7), mask and mask_ don't exist.
-- We cannot compare against MIN_VERSION_base since that is not available
-- durinc Cabal bootstrapping.
-- Therefore we use __GLASGOW_HASKELL__; cabal currently doesn't build on
-- anything but GHC anyway.

#if __GLASGOW_HASKELL__ < 700
import Control.Exception (block, unblock)
#else
import Control.Exception (mask, mask_)
#endif

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch

-- Can't use CPP with MIN_VERSION_base in the Cabal package,
-- so we have to use the deprecated versions :(

#if __GLASGOW_HASKELL__ < 700
-- note: less polymorphic than 'real' mask, to avoid RankNTypes
-- we don't need the full generality where we use it
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask handler = Exception.block (handler Exception.unblock)

mask_ :: IO a -> IO a
mask_ = Exception.block
#endif
