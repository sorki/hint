module Hint.GHC (
    Message, module X
#if MIN_VERSION_ghc(9,0,0)
    , dynamicGhc
#endif
) where

import GHC as X hiding (Phase, GhcT, runGhcT)
import Control.Monad.Ghc as X (GhcT, runGhcT)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types as X (SourceError, srcErrorMessages, GhcApiError)
-- import GHC.Driver.Types as X (mgModSummaries)

import GHC.Utils.Outputable as X (PprStyle, SDoc, Outputable(ppr),
                                  showSDoc, showSDocForUser, showSDocUnqual,
                                  withPprStyle, defaultErrStyle, vcat)

import GHC.Utils.Error as X (mkLocMessage, pprErrMsgBagWithLoc, MsgDoc,
                             errMsgSpan, pprErrMsgBagWithLoc)
                             -- we alias MsgDoc as Message below

import GHC.Driver.Phases as X (Phase(Cpp), HscSource(HsSrcFile))
import GHC.Data.StringBuffer as X (stringToStringBuffer)
import GHC.Parser.Lexer as X (P(..), ParseResult(..), mkPState,
                              getErrorMessages)
import GHC.Parser as X (parseStmt, parseType)
import GHC.Data.FastString as X (fsLit)

import GHC.Driver.Session as X (xFlags, xopt, LogAction, FlagSpec(..),
                                WarnReason(NoReason), addWay')
import GHC.Driver.Ways as X (Way (..), hostIsDynamic)

import GHC.Core.Ppr.TyThing as X (pprTypeForUser)
import GHC.Types.SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import GHC.Core.ConLike as X (ConLike(RealDataCon))

dynamicGhc :: Bool
dynamicGhc = hostIsDynamic
#else
import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)
import HscTypes as X (mgModSummaries)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        showSDoc, showSDocForUser, showSDocUnqual,
                        withPprStyle, defaultErrStyle, vcat)

import ErrUtils as X (mkLocMessage, pprErrMsgBagWithLoc, MsgDoc
#if __GLASGOW_HASKELL__ >= 810
  , errMsgSpan, pprErrMsgBagWithLoc
#endif
  ) -- we alias MsgDoc as Message below

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
                      WarnReason(NoReason), addWay', Way(..), dynamicGhc)

import PprTyThing as X (pprTypeForUser)
import SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))
#endif

type Message = MsgDoc
