module Repl (repl) where

import Control.Monad.Trans

import qualified Top


repl :: MonadIO m => [String] -> m ()
repl _ = Top.repl
