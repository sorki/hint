module Hint.GHC (
    -- * Shims
    dynamicGhc,
    Message,
    Logger,
    -- * Re-exports
    module X,
) where

import GHC as X hiding (Phase, GhcT, runGhcT)
import Control.Monad.Ghc as X (GhcT, runGhcT)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types as X (SourceError, srcErrorMessages, GhcApiError)

import GHC.Utils.Outputable as X (PprStyle, SDoc, Outputable(ppr),
                                  showSDoc, showSDocForUser, showSDocUnqual,
                                  withPprStyle, defaultErrStyle, vcat)

import GHC.Utils.Error as X (mkLocMessage, pprErrMsgBagWithLoc, errMsgSpan)

import GHC.Driver.Phases as X (Phase(Cpp), HscSource(HsSrcFile))
import GHC.Data.StringBuffer as X (stringToStringBuffer)
import GHC.Parser.Lexer as X (P(..), ParseResult(..), mkPState,
                              getErrorMessages)
import GHC.Parser as X (parseStmt, parseType)
import GHC.Data.FastString as X (fsLit)

import GHC.Driver.Session as X (xFlags, xopt, LogAction, FlagSpec(..),
                                WarnReason(NoReason), addWay')
import GHC.Driver.Ways as X (Way (..))

import GHC.Core.Ppr.TyThing as X (pprTypeForUser)
import GHC.Types.SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import GHC.Core.ConLike as X (ConLike(RealDataCon))
#else
import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)
import HscTypes as X (mgModSummaries)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        showSDoc, showSDocForUser, showSDocUnqual,
                        withPprStyle, defaultErrStyle, vcat)

import ErrUtils as X (mkLocMessage, pprErrMsgBagWithLoc
#if __GLASGOW_HASKELL__ >= 810
  , errMsgSpan
#endif
  )

import DriverPhases as X (Phase(Cpp), HscSource(HsSrcFile))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState
#if __GLASGOW_HASKELL__ >= 810
  , getErrorMessages
#endif
  )
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

import DynFlags as X (xFlags, xopt, LogAction, FlagSpec(..),
                      WarnReason(NoReason), addWay', Way(..))

import PprTyThing as X (pprTypeForUser)
import SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))
#endif

{-------------------- Imports for Shims --------------------}

#if MIN_VERSION_ghc(9,0,0)
-- dynamicGhc
import GHC.Driver.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (MsgDoc)
#else
-- dynamicGhc
import qualified DynFlags as GHC (dynamicGhc)

-- Message
import qualified ErrUtils as GHC (MsgDoc)
#endif

{-------------------- Shims --------------------}

-- dynamicGhc
dynamicGhc :: Bool
#if MIN_VERSION_ghc(9,0,0)
dynamicGhc = hostIsDynamic
#else
dynamicGhc = GHC.dynamicGhc
#endif

-- Message
type Message = GHC.MsgDoc

-- Logger
type Logger = LogAction
