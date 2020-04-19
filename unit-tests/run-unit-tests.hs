{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Say
import Data.Traversable
import Data.Foldable
import qualified Data.Text as Text
import Prelude hiding (catch)

import Control.Exception.Extensible (ArithException(..), AsyncException(UserInterrupt))
import Control.Monad.Catch as MC

import Control.Monad (liftM, when, void, (>=>))

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

import Data.IORef

import System.IO
import System.FilePath
import System.Directory
import System.Exit
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
import System.Posix.Signals
#endif

import Test.HUnit ((@?=), (@?))
import qualified Test.HUnit as HUnit

import Language.Haskell.Interpreter

test_reload_modified :: String -> TestCase
test_reload_modified prefix = TestCase "reload_modified" [mod_file] $ do
                            liftIO $ writeFile mod_file mod_v1
                            f_v1 <- get_f
                            --
                            liftIO $ writeFile mod_file mod_v2
                            f_v2 <- get_f
                            --
                            liftIO $ (f_v1 5, f_v2 5) @?= (5, 6)
    --
    where mod_name = prefix ++ "TEST_ReloadModified"
          mod_file = mod_name ++ ".hs"
          --
          mod_v1   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = id"]
          mod_v2   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = (1 +)"]
          --
          get_f    = do loadModules [mod_file]
                        setTopLevelModules [mod_name]
                        interpret "f" (as :: Int -> Int)

test_lang_exts :: String -> TestCase
test_lang_exts prefix = TestCase "lang_exts" [mod_file] $ do
                      liftIO $ writeFile mod_file "data T where T :: T"
                      fails do_load @@? "first time, it shouldn't load"
                      --
                      set [languageExtensions := [GADTs]]
                      succeeds do_load @@? "now, it should load"
                      --
                      set [languageExtensions := []]
                      fails do_load @@? "it shouldn't load, again"
    --
    where mod_name = prefix ++ "TEST_LangExts"
          mod_file = mod_name ++ ".hs"
          --
          do_load  = loadModules [mod_name]

test_work_in_main :: String -> TestCase
test_work_in_main prefix = TestCase "work_in_main" [mod_file] $ do
                        liftIO $ writeFile mod_file "f = id"
                        loadModules [mod_file]
                        setTopLevelModules ["Main"]
                        setImportsQ [("Prelude", Nothing),
                                     ("Data.Maybe", Just "Mb")]
                        --
                        typeOf "f $ (1 + 1 :: Int)" @@?= "Int"
                        eval "f . Mb.fromJust $ Just [1,2]" @@?= "[1,2]"
                        interpret "f $ 1 == 2" infer @@?= False
    --
    where mod_file     = prefix ++ "TEST_WorkInMain.hs"

test_priv_syms_in_scope :: String -> TestCase
test_priv_syms_in_scope prefix = TestCase "private_syms_in_scope" [mod_file] $ do
                               -- must set to True, otherwise won't work with
                               -- ghc 6.8
                               set [installedModulesInScope := True]
                               liftIO $ writeFile mod_file mod_text
                               loadModules [mod_file]
                               setTopLevelModules ["T"]
                               typeChecks "g" @@? "g is hidden"
    where mod_text = unlines ["module T(f) where", "f = g", "g = id"]
          mod_file = prefix ++ "TEST_PrivateSymbolsInScope.hs"

test_comments_in_expr :: String -> TestCase
test_comments_in_expr _ = TestCase "comments_in_expr" [] $ do
                            setImports ["Prelude"]
                            let expr = "length $ concat [[1,2],[3]] -- bla"
                            typeChecks expr @@? "comment on expression"
                            _ <- eval expr
                            _ <- interpret expr (as :: Int)
                            return ()

test_qual_import :: String -> TestCase
test_qual_import _ = TestCase "qual_import" [] $ do
                           setImportsQ [("Prelude", Nothing),
                                        ("Data.Map", Just "M")]
                           typeChecks "null []" @@? "Unqual null"
                           typeChecks "M.null M.empty" @@? "Qual null"

test_full_import :: String -> TestCase
test_full_import _ = TestCase "full_import" [] $ do
                           setImportsF [ ModuleImport "Prelude" (QualifiedAs Nothing) NoImportList
                                       , ModuleImport "Data.List" (QualifiedAs $ Just "List") $ ImportList ["null"]
                                       ]
                           typeChecks "Prelude.null []" @@? "Qual prelude null"
                           typeChecks "List.null []" @@? "Qual list null"

test_basic_eval :: String -> TestCase
test_basic_eval _ = TestCase "basic_eval" [] $ eval "()" @@?= "()"

test_eval_layout :: String -> TestCase
test_eval_layout _ = TestCase "eval_layout" [] $ eval layout_expr @@?= "10"
    where layout_expr = unlines ["let x = let y = 10",
                                 "        in y",
                                 "in x"]

test_show_in_scope :: String -> TestCase
test_show_in_scope _ = TestCase "show_in_scope" [] $ do
                       setImports ["Prelude"]
                       eval "show ([] :: String)" @@?= show (show "")

test_installed_not_in_scope :: String -> TestCase
test_installed_not_in_scope _ = TestCase "installed_not_in_scope" [] $ do
                                b <- get installedModulesInScope
                                succeeds action @@?= b
                                set [installedModulesInScope := False]
                                fails action @@? "now must be out of scope"
                                set [installedModulesInScope := True]
                                succeeds action @@? "must be in scope again"
    where action = typeOf "Data.Map.singleton"

test_search_path :: String -> TestCase
test_search_path prefix =
    TestCase "search_path" files $ do
           liftIO setup
           fails (loadModules [mod_1]) @@? "mod_1 should not be in path (1)"
           fails (loadModules [mod_2]) @@? "mod_2 should not be in path (1)"
           --
           set [searchPath := [dir_1]]
           succeeds (loadModules [mod_1]) @@? "mod_1 should be in path (2)"
           fails    (loadModules [mod_2]) @@? "mod_2 should not be in path (2)"
           --
           set [searchPath := [dir_2]]
           fails    (loadModules [mod_1]) @@? "mod_1 should not be in path (3)"
           succeeds (loadModules [mod_2]) @@? "mod_2 should be in path (3)"
           --
           set [searchPath := [dir_1,dir_2]]
           succeeds (loadModules [mod_1]) @@? "mod_1 should be in path (4)"
           succeeds (loadModules [mod_2]) @@? "mod_2 should be in path (4)"
    where dir_1  = prefix ++ "search_path_test_dir_1"
          mod_1  = "M1"
          file_1 = dir_1 </> mod_1 <.> "hs"
          dir_2  = prefix ++ "search_path_test_dir_2"
          mod_2  = "M2"
          file_2 = dir_2 </> mod_2 <.> "hs"
          files  = [file_1, file_2, dir_1, dir_2]
          setup  = do createDirectory dir_1
                      createDirectory dir_2
                      writeFile file_1 $
                         unlines ["module " ++ mod_1,
                                  "where",
                                  "x :: Int",
                                  "x = 42"]
                      writeFile file_2 $
                         unlines ["module " ++ mod_2,
                                  "where",
                                  "y :: Bool",
                                  "y = False"]

test_search_path_dot :: String -> TestCase
test_search_path_dot prefix =
    TestCase "search_path_dot" [mod_file, dir] $ do
           liftIO setup
           succeeds (loadModules [mod1]) @@? "mod1 must be initially in path"
           set [searchPath := [dir]]
           succeeds (loadModules [mod1]) @@? "mod1 must be still in path"
    --
    where dir      = prefix ++ "search_path_dot_dir"
          mod1     = "M1"
          mod_file = mod1 <.> "hs"
          setup    = do createDirectory dir
                        writeFile mod_file $
                           unlines ["x :: Int", "x = 42"]

test_catch :: String -> TestCase
test_catch _ = TestCase "catch" [] $ do
        setImports ["Prelude"]
        succeeds (action `catch` handler) @@? "catch failed"
    where handler DivideByZero = return "catched"
          handler e = throwM e
          action = do s <- eval "1 `div` 0 :: Int"
                      return $! s

test_only_one_instance :: String -> TestCase
test_only_one_instance _ = TestCase "only_one_instance" [] $ liftIO $ do
        r <- newEmptyMVar
        let concurrent = runInterpreter (liftIO $ putMVar r False)
                          `catch` \MultipleInstancesNotAllowed ->
                                    do liftIO $ putMVar r True
                                       return $ Right ()
        _ <- forkIO $ Control.Monad.void concurrent
        readMVar r @?  "concurrent instance did not fail"

test_normalize_type :: String -> TestCase
test_normalize_type prefix = TestCase "normalize_type" [mod_file] $ do
        liftIO $ writeFile mod_file mod_text
        loadModules [mod_file]
        setTopLevelModules ["T"]
        normalizeType "Foo Int" @@?= "()"

    where mod_text = unlines ["{-# LANGUAGE TypeFamilies #-}"
                             ,"module T where"
                             ,"type family Foo x"
                             ,"type instance Foo x = ()"]
          mod_file = prefix ++ "TEST_NormalizeType.hs"

-- earlier versions of hint were accidentally overwriting the signal handlers
-- for ^C and others.
--
-- note that hint was _not_ overwriting the signal handlers when the hint interpreter
-- was itself executed inside the ghci interpreter. for this reason, this test always
-- succeeds when executed from ghci and ghcid, regardless of whether the problematic
-- behaviour has been fixed or not.
test_signal_handlers :: IOTestCase
test_signal_handlers = IOTestCase "signal_handlers" [] $ \runInterp -> do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        runInterp $ do
          pure ()
#else
        signalDetectedRef <- newIORef False
        interruptDetectedRef <- newIORef False
        let detectSignal = writeIORef signalDetectedRef True
            detectInterrupt = writeIORef interruptDetectedRef True
            acquire = installHandler sigINT (Catch detectSignal) Nothing
            release handler = installHandler sigINT handler Nothing
        r <- bracket acquire release $ \_ -> do
          runInterp $ do
            liftIO $ do
              r <- try $ do
                raiseSignal sigINT
                threadDelay 1000000  -- will be interrupted by the above signal
              case r of
                Left UserInterrupt -> do
                  -- hint is _still_ accidentally overwriting the signal handler :(
                  detectInterrupt
                Left e -> do
                  -- some other async exception, rethrow
                  throwM e
                Right () ->
                  return ()
        signalDetected <- readIORef signalDetectedRef
        signalDetected @?= True
        interruptDetected <- readIORef interruptDetectedRef
        interruptDetected @?= False
        return r
#endif

tests :: String -> [TestCase]
tests prefix = [test_reload_modified prefix
        ,test_lang_exts prefix
        ,test_work_in_main prefix
        ,test_comments_in_expr prefix
        ,test_qual_import prefix
        ,test_full_import prefix
        ,test_basic_eval prefix
        ,test_eval_layout prefix
        ,test_show_in_scope prefix
        ,test_installed_not_in_scope prefix
        ,test_priv_syms_in_scope prefix
        ,test_search_path prefix
        ,test_search_path_dot prefix
        ,test_catch prefix
        --,test_only_one_instance
        ,test_normalize_type prefix
        ]

ioTests :: [IOTestCase]
ioTests = [test_signal_handlers
          ]

main :: IO ()
main = do
  let forkThread :: Int -> IO (MVar ())
      forkThread i = do
        mvar <- newEmptyMVar
        _ <- forkIO $ do
          flip finally (putMVar mvar ()) $ do
            let threadName = "Thread" ++ show i
            let prefix = threadName ++ "_"
            say $ Text.pack threadName <> " started"
            main2 prefix
            say $ Text.pack threadName <> " done"
        pure mvar
  threads <- traverse forkThread [1..2]
  traverse_ takeMVar threads

main2 :: String -> IO ()
main2 prefix = do -- run the tests...
          c1 <- runTests False (tests prefix)
          -- then run again, but with sandboxing on...
          c3 <- runTests True (tests prefix)
          --
          let failures  = HUnit.errors c1 + HUnit.failures c1 +
                          HUnit.errors c3 + HUnit.failures c3
              exit_code
                  | failures > 0 = ExitFailure failures
                  | otherwise    = ExitSuccess
          exitWith exit_code
       -- `catch` (\_ -> exitWith (ExitFailure $ -1))

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError = say . Text.pack . show

setSandbox :: Interpreter ()
setSandbox = set [installedModulesInScope := False]

(@@?) :: (HUnit.AssertionPredicable p, MonadIO m) => m p -> String -> m ()
p @@? msg = do b <- p; liftIO (b @? msg)

(@@?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m_a @@?= b = do a <- m_a; liftIO (a @?= b)

fails :: (MonadCatch m, MonadIO m) => m a -> m Bool
fails action = (action >> return False) `catchIE` (\_ -> return True)

catchIE :: MonadCatch m => m a -> (InterpreterError -> m a) -> m a
catchIE = MC.catch

succeeds :: (MonadCatch m, MonadIO m) => m a -> m Bool
succeeds = fmap not . fails

data IOTestCase = IOTestCase String [FilePath] ((Interpreter () -> IO (Either InterpreterError ())) -> IO (Either InterpreterError ()))

runIOTests :: Bool -> [IOTestCase] -> IO HUnit.Counts
runIOTests sandboxed = HUnit.runTestTT . HUnit.TestList . map build
    where build (IOTestCase title tmps test) = HUnit.TestLabel title $
                                                   HUnit.TestCase test_case
            where test_case = go `finally` clean_up
                  clean_up = mapM_ removeIfExists tmps
                  go       = do r <- test (\body -> runInterpreter
                                            (when sandboxed setSandbox >> body))
                                either (printInterpreterError >=> (fail . show))
                                       return r
                  removeIfExists f = do existsF <- doesFileExist f
                                        if existsF
                                          then removeFile f
                                          else
                                            do existsD <- doesDirectoryExist f
                                               when existsD $
                                                  removeDirectory f

data TestCase = TestCase String [FilePath] (Interpreter ())

runTests :: Bool -> [TestCase] -> IO HUnit.Counts
runTests sandboxed = runIOTests sandboxed . map toIOTestCase
  where
    toIOTestCase :: TestCase -> IOTestCase
    toIOTestCase (TestCase title tmps test) = IOTestCase title tmps ($ test)
