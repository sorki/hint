module Hint.GHC (
    -- * Shims
    dynamicGhc,
    Message,
    Logger,
    initLogger,
    putLogMsg,
    pushLogHook,
    modifyLogger,
    UnitState,
    emptyUnitState,
    showSDocForUser,
    -- * Re-exports
    module X,
) where

import GHC as X hiding (Phase, GhcT, runGhcT
#if MIN_VERSION_ghc(9,2,0)
                       , Logger
                       , modifyLogger
                       , pushLogHook
#endif
                       )
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

import GHC.Utils.Outputable as X (showSDoc, showSDocUnqual)

import GHC.Utils.Error as X (pprErrMsgBagWithLoc)

import GHC.Driver.Phases as X (HscSource(HsSrcFile))

import GHC.Parser.Lexer as X (mkPState)

import GHC.Driver.Session as X (LogAction, addWay')
import GHC.Driver.Ways as X (Way (..))

import GHC.Core.Ppr.TyThing as X (pprTypeForUser)
#else
import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)

import Outputable as X (showSDoc, showSDocUnqual)

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

-- Logger
import qualified GHC.Utils.Logger as GHC (Logger, initLogger, putLogMsg, pushLogHook)
import qualified GHC.Driver.Monad as GHC (modifyLogger)

-- UnitState
import qualified GHC.Unit.State as GHC (UnitState, emptyUnitState)

-- showSDocForUser
import qualified GHC.Driver.Ppr as GHC (showSDocForUser)
#elif MIN_VERSION_ghc(9,0,0)
-- dynamicGhc
import GHC.Driver.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (MsgDoc)

-- Logger
import qualified GHC.Driver.Session as GHC (defaultLogAction)
import qualified GHC.Driver.Session as DynFlags (log_action)

-- showSDocForUser
import qualified GHC.Utils.Outputable as GHC (showSDocForUser)
#else
-- dynamicGhc
import qualified DynFlags as GHC (dynamicGhc)

-- Message
import qualified ErrUtils as GHC (MsgDoc)

-- Logger
import qualified DynFlags as GHC (defaultLogAction)
import qualified DynFlags (log_action)

-- showSDocForUser
import qualified Outputable as GHC (showSDocForUser)
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
initLogger :: IO Logger
putLogMsg :: Logger -> LogAction
pushLogHook :: (LogAction -> LogAction) -> Logger -> Logger
modifyLogger :: GhcMonad m => (Logger -> Logger) -> m ()
#if MIN_VERSION_ghc(9,2,0)
type Logger = GHC.Logger
initLogger = GHC.initLogger
putLogMsg = GHC.putLogMsg
pushLogHook = GHC.pushLogHook
modifyLogger = GHC.modifyLogger
#else
type Logger = LogAction
initLogger = pure GHC.defaultLogAction
putLogMsg = id
pushLogHook = id
modifyLogger f = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df{log_action = f $ DynFlags.log_action df}
  return ()
#endif

-- UnitState
emptyUnitState :: UnitState
#if MIN_VERSION_ghc(9,2,0)
type UnitState = GHC.UnitState
emptyUnitState = GHC.emptyUnitState
#else
type UnitState = ()
emptyUnitState = ()
#endif

-- showSDocForUser
showSDocForUser :: DynFlags -> UnitState -> PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
showSDocForUser = GHC.showSDocForUser
#else
showSDocForUser df _ = GHC.showSDocForUser df
#endif
