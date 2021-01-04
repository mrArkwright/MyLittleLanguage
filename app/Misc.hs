module Misc where

import Control.Monad.Except


maybeToError :: MonadError e m => Maybe a -> e -> m a
maybeToError (Just x) _ = return x
maybeToError Nothing err = throwError err
