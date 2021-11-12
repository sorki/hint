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

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceError as X (SourceError, srcErrorMessages)

import GHC.Driver.Ppr as X (showSDoc)

import GHC.Types.SourceFile as X (HscSource(HsSrcFile))

import GHC.Utils.Logger as X (LogAction)
import GHC.Platform.Ways as X (Way (..))

import GHC.Types.TyThing.Ppr as X (pprTypeForUser)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types as X (SourceError, srcErrorMessages, GhcApiError)

import GHC.Utils.Outputable as X (showSDoc, showSDocForUser, showSDocUnqual)

import GHC.Utils.Error as X (pprErrMsgBagWithLoc)

import GHC.Driver.Phases as X (HscSource(HsSrcFile))

import GHC.Parser.Lexer as X (mkPState)

import GHC.Driver.Session as X (LogAction, addWay')
import GHC.Driver.Ways as X (Way (..))

import GHC.Core.Ppr.TyThing as X (pprTypeForUser)
#else
import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)

import Outputable as X (showSDoc, showSDocForUser, showSDocUnqual)

import ErrUtils as X (pprErrMsgBagWithLoc)

import DriverPhases as X (HscSource(HsSrcFile))

import Lexer as X (mkPState)

import DynFlags as X (LogAction, addWay', Way(..))

import PprTyThing as X (pprTypeForUser)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Outputable as X (PprStyle, SDoc, Outputable(ppr),
                                  withPprStyle, defaultErrStyle, vcat)

import GHC.Utils.Error as X (mkLocMessage, errMsgSpan)

import GHC.Driver.Phases as X (Phase(Cpp))
import GHC.Data.StringBuffer as X (stringToStringBuffer)
import GHC.Parser.Lexer as X (P(..), ParseResult(..),
                              getErrorMessages)
import GHC.Parser as X (parseStmt, parseType)
import GHC.Data.FastString as X (fsLit)

import GHC.Driver.Session as X (xFlags, xopt, FlagSpec(..), WarnReason(NoReason))

import GHC.Types.SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import GHC.Core.ConLike as X (ConLike(RealDataCon))
#else
import HscTypes as X (mgModSummaries)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        withPprStyle, defaultErrStyle, vcat)

import ErrUtils as X (mkLocMessage
#if __GLASGOW_HASKELL__ >= 810
  , errMsgSpan
#endif
  )

import DriverPhases as X (Phase(Cpp))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState
#if __GLASGOW_HASKELL__ >= 810
  , getErrorMessages
#endif
  )
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

import DynFlags as X (xFlags, xopt, FlagSpec(..), WarnReason(NoReason))

import SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))
#endif

{-------------------- Imports for Shims --------------------}

#if MIN_VERSION_ghc(9,2,0)
-- dynamicGhc
import GHC.Platform.Ways (hostIsDynamic)
#elif MIN_VERSION_ghc(9,0,0)
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
#if MIN_VERSION_ghc(9,2,0)
type Message = SDoc
#else
type Message = GHC.MsgDoc
#endif

-- Logger
type Logger = LogAction
