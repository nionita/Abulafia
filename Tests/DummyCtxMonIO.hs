module Tests.DummyCtxMonIO where

-- import Control.Monad
-- import System.IO

import Moves.BaseTypes

instance CtxMon IO where
    tellCtx _ = return ()
    timeCtx   = return 0
