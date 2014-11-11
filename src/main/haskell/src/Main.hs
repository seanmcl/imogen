
{- 
| Imogen top level.  This module handles a few flags like the logging
level, and passes the remaining options to the individual provers.

For interactive use, use

  ghci Main -lcvc3 Imogen/Util/CVC/cvc-haskell.o
-} 

-- @ Pragmas

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports  #-} 
{-# LANGUAGE Rank2Types #-} 

-- @ Signature

module Main
  ( main ) 
where

-- @ Imports

import Imogen.Util.Prelude hiding (log)
import qualified Codec.Binary.UTF8.String as UString
import qualified Control.Exception as Exn
import qualified Data.Maybe as Maybe
import qualified Imogen.Atom as Atom
import Imogen.Constr (Ψ)
import qualified Imogen.Constr.Dlo as Dlo
import qualified Imogen.Focus as Focus
import qualified Imogen.Linear as LL
import Imogen.Linear ((⊸))
import qualified Imogen.Modal as Modal
import qualified Imogen.Ordered as OL
import qualified Imogen.Parse as Parse
import qualified Imogen.PFormula as F
import Imogen.PFormula (Neg, (⊃), (⊥))
import qualified Imogen.Prover as Prover
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
-- import qualified Imogen.Test.Ants
-- import qualified Imogen.Test.Dlo
import qualified Imogen.Test.Linear
import qualified Imogen.Test.Modal
import qualified Imogen.Test.Ordered
-- import qualified Imogen.Test.OrdWorld
import qualified Imogen.Test.Seq
import qualified Imogen.Test.TString
import qualified Imogen.Test.Unif
import qualified Imogen.Test.Uninterp
-- import qualified Imogen.Test.World 
import qualified Imogen.Uninterp as Uninterp
import Imogen.Util.Impossible (catchImpossible)
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Priority)
import qualified Imogen.Misc as Misc
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint, (<+>))
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test(Test(..), (~:))
import Imogen.Util.Three(Three(..))
import System.Console.GetOpt (OptDescr(Option), ArgDescr(..))
import qualified System
import qualified System.Console.GetOpt as Opt
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.IO.UTF8 as S

-- @ Flags

data Flag = Classical
          | Format Format
          | IltpDir FilePath
          | IltpPropDir FilePath
          | LogToFile
          | LogToTerm
          | Modality Modal.Mode
          | Syntax Syntax
          | TptpDir FilePath
          | Verbose Priority
  deriving (Eq, Show)

instance Print Flag where
  pPrint = PP.text . show

-- @@ Input syntax

data Syntax = PolarSyn | NonPolarSyn | TptpSyn
  deriving (Eq, Ord, Show)

defaultSyntax :: Syntax
defaultSyntax = PolarSyn

syntax :: Maybe String -> Flag
syntax Nothing = Syntax defaultSyntax
syntax (Just s) = case s of 
  "Polar" -> Syntax PolarSyn
  "NonPolar" -> Syntax NonPolarSyn
  "Tptp" -> Syntax TptpSyn
  _ -> error $ "No such setting for 'syntax': " ++ s

-- @@ Input format

data Format = String | File
  deriving (Eq, Ord, Show)

defaultFormat :: Format
defaultFormat = String

format :: Maybe String -> Flag
format Nothing = Format defaultFormat
format (Just s) = case s of 
  "String" -> Format String
  "File" -> Format File
  _ -> error $ "No such setting for 'format': " ++ s

mkInput :: Format -> String -> Misc.Input
mkInput String = Misc.String 
mkInput File = Misc.File

-- @@ Tptp stuff

data Tptp = Iltp | IltpProp | Tptp

tptpDir :: Tptp -> [Flag] -> FilePath
tptpDir k flags = case k of
  Iltp -> case List.findFirst isIltp flags of
    Just d -> d
    Nothing -> defaultIltpDir
  IltpProp -> case List.findFirst isIltpProp flags of
    Just d -> d
    Nothing -> defaultIltpPropDir
  Tptp -> case List.findFirst isTptp flags of
    Just d -> d
    Nothing -> defaultTptpDir
 where
  isIltp (IltpDir d) = Just d 
  isIltp _ = Nothing
  isIltpProp (IltpPropDir d) = Just d 
  isIltpProp _ = Nothing
  isTptp (TptpDir d) = Just d 
  isTptp _ = Nothing

tptpPath :: FilePath -> String -> IO Misc.Input
tptpPath path prob = 
  let suffixes = ["", "+stdfof", "+stdfof+eq_rstfp", "+stdfof+noeq"] 
      grp = take 3 prob
      fname suf = concat [path, "/", grp, "/", prob, suf, ".tptp"]
  in do
    m <- M.findM (Dir.doesFileExist . fname) suffixes
    case m of 
      Nothing -> error $ "Can't find file for (path, problem): (" ++ path ++ ", " ++ prob ++ ")"
      Just suf -> return . Misc.File $ fname suf

mkTptpInput :: Tptp -> [Flag] -> String -> IO Misc.Input
mkTptpInput tptp flags = tptpPath (tptpDir tptp flags) 

defaultIltpPropDir, defaultIltpDir, defaultTptpDir :: FilePath
defaultIltpPropDir = "../test/iltp/prop/tptp"
defaultIltpDir = "../test/iltp/fol/tptp"
defaultTptpDir = "../test/tptp/tptp"

tptpOpt :: FilePath -> (FilePath -> Flag) -> ArgDescr Flag
tptpOpt def con = OptArg (maybe (con def) con) def

-- @@ Verbose

verbose :: Maybe String -> Flag
verbose Nothing = Verbose Log.defaultPrio
verbose (Just s) = case Log.readPrio s of 
  Nothing -> error $ "No such setting for 'verbose': " ++ s
  Just p -> Verbose p

-- @@ Formula transformations



-- @ Options

options :: [OptDescr Flag]
options =
 [ 

 -- Logging 

   Option ['V'] ["Verbose"] (OptArg verbose (show Log.defaultPrio)) 
     "Verbosity level.  (debug | info | warn | error)"

 , Option ['T'] ["LogToTerm"] (NoArg LogToTerm)
     "Log to terminal on stdout."

 , Option ['F'] ["LogToFile"] (NoArg LogToFile)
     "Log to the log file."

 -- Input syntax

 , Option ['s'] ["Syntax"] (OptArg syntax (show defaultSyntax))
     "Input formula syntax."

 , Option ['f'] ["Format"] (OptArg format (show defaultFormat))
     "Input formula format (File | String)."

 -- Transformations

 , Option [] ["Classical"] (NoArg Classical)
     "Use a double-negation translation to emulate classical logic."

 , Option ['M'] ["Modality"] (ReqArg (Modality . P.parse) "Modality")
     "Which modal logic to use. (K | D | K4 | D4 | T | S4 | S5)"

 -- Test problems 

 , Option [] ["TptpDir"] (tptpOpt defaultTptpDir TptpDir)
     "Input formula syntax."

 , Option [] ["IltpPropDir"] (tptpOpt defaultIltpPropDir IltpPropDir)
     "Input formula syntax."

 , Option [] ["IltpDir"] (tptpOpt defaultIltpDir IltpDir)
     "Input formula syntax."

 ]

parseOptions :: [String] -> IO ([Flag], [String], [String])
parseOptions argv = case Opt.getOpt' Opt.Permute options argv of
  (o, n, u, []) -> return (o, n, u)
  (_, _, _, errs) -> ioError $ userError $ concat errs ++ usage

usage :: String
usage = Opt.usageInfo usageMsg options
  where usageMsg = List.concat $ List.intersperse "\n"
          [ "usage: imogen dlo ..."
          , "       imogen help"
          , "       imogen iltp ..."
          , "       imogen iltpProp ..."
          , "       imogen linear ..."
          , "       imogen modal ..."
          , "       imogen ordered ..."
          , "       imogen test"
          , "       imogen tptp ..."
          , "       imogen unicode <string>"
          , "       imogen uninterp ..."
          , "       imogen version"
          , "options: "
          ]

-- @ Commands 

defaultModality :: Modal.Mode
defaultModality = Modal.S4

data Command = DloCmd String
             | HelpCmd
             | IltpCmd String
             | IltpPropCmd String
             | ImogenCmd String
             | LinearCmd String
             | ModalCmd String
             | OrderedCmd String
             | TestCmd (Maybe String)
             | TptpCmd String
             | UnicodeCmd String
             | UninterpCmd String
             | VersionCmd

-- Determine the command to execute.

classify :: [String] -> Either Command String
classify args = case args of
  [] -> Right "I need a command to execute."
  ["dlo", form] -> Left $ DloCmd form
  "dlo" : _ -> Right "'dlo' takes one argument."
  ["help"] -> Left HelpCmd
  "help" : _ -> Right "'help' takes no arguments."
  ["iltp", prob] -> Left $ IltpCmd prob
  "iltp" : _ -> Right "'iltp' takes one argument."
  ["iltpProp", prob] -> Left $ IltpPropCmd prob
  "iltpProp" : _ -> Right "'iltpProp' takes one argument."
  ["linear", form] -> Left $ LinearCmd form
  "linear" : _ -> Right "'linear' takes one argument."
  ["modal", form] -> Left $ ModalCmd form
  "modal" : _ -> Right "'modal' takes at least one argument."
  ["ordered", form] -> Left $ OrderedCmd form
  "ordered" : _ -> Right "'ordered' takes one argument."
  ["test"] -> Left (TestCmd Nothing)
  ["test", t]  -> Left (TestCmd $ Just t)
  "test" : _ -> Right "'test' takes no arguments"
  ["tptp", prob] -> Left $ TptpCmd prob
  "tptp" : _ -> Right "'tptp' takes one argument."
  ["unicode", f] -> Left $ UnicodeCmd f
  "unicode" : _ -> Right "'unicode' takes one argument"
  ["uninterp", form] -> Left $ UninterpCmd form
  "uninterp" : _ -> Right "'uninterp' takes one argument."
  ["version"] -> Left VersionCmd 
  "version" : _ -> Right "'version' takes no arguments"
  [file] -> Left $ ImogenCmd file
  _ -> Right "Unrecognized command."

-- @ Provers

mkProver :: (Misc.Input -> IO Misc.Output) -> Misc.Input -> IO ()
mkProver solver inp = do
  res <- solver inp
  case res of 
    Misc.Yes _ -> S.putStrLn "The formula is true!"
    Misc.No -> S.putStrLn "The formula is false!"
    Misc.Maybe -> S.putStrLn "Mmm... I can't figure this one out."

-- @ Top 

getPrio :: Flag -> Maybe Priority
getPrio (Verbose p) = Just p
getPrio _ = Nothing

main :: IO ()
main = do 
  S.putStrLn "Welcome to the Imogen theorem prover!"
  -- Get command line arguments
  args <- System.getArgs
  -- Run the command
  doit args

doit :: [String] -> IO ()
doit args = do 
  -- Parse arguments.  Opts is the unknown options that will be parsed by
  -- the individual prover.  
  (flags, args', opts) <- parseOptions args
  -- It's important to decode the command line arguments, since they may
  -- be in Unicode syntax.  UString seems to work.
  let uopts = map UString.decodeString opts
      uargs = map UString.decodeString args'
  -- Initialize the log file
      logToFile = elem LogToFile flags
      prio = maybe Log.defaultPrio id (List.findFirst getPrio flags)
  Log.initialize prio logToFile (elem LogToTerm flags)
  -- Now we can start logging.  Start with a notification that
  -- logging is taking place.
  if logToFile
    then Log.infoM "Main" ("Logging output to file: " ++ Log.logFileName) 
    else return ()
  Log.infoM' "Imogen" $
    PP.vcat [ PP.text "input        :" <+> pPrint args
            , PP.text "flags        :" <+> pPrint flags
            , PP.text "unknown opts :" <+> pPrint uopts 
            , PP.text "args         :" <+> pPrint uargs ] 
  -- Get the input format
  let fmt = maybe defaultFormat id (List.findFirst getFmt flags)
      getFmt (Format f) = Just f
      getFmt _ = Nothing
      classical = elem Classical flags
  -- Run the requested command
  case classify uargs of
    -- Error message
    Right s -> do
      S.putStrLn s   
      S.putStrLn usage
    -- Dlo
    Left (DloCmd form) -> mkProver Prover.dloProve (mkInput fmt form)
    -- Print a help message
    Left HelpCmd -> S.putStrLn usage
    -- Determine prover from the file header
    Left (ImogenCmd _file) -> error "Unimplemented" 
    -- Iltp 
    Left (IltpCmd prob) -> do 
      inp <- mkTptpInput Iltp flags prob
      mkProver Prover.tptpProve inp
    -- IltpProp
    Left (IltpPropCmd prob) -> do
      Log.debugM "doit" $ "IltpPropCmd:" ++ prob
      inp <- mkTptpInput IltpProp flags prob
      Log.debugM "doit" $ "Input: " ++ show inp
      mkProver Prover.tptpProve inp
    -- Linear logic
    Left (LinearCmd form) -> mkProver Prover.linearProve (mkInput fmt form)
    -- Ordered logic
    Left (ModalCmd form) -> 
      let mode = maybe defaultModality id (List.findFirst hasMode flags) in
      mkProver (Prover.modalProve mode) (mkInput fmt form)
       where hasMode (Modality m) = Just m
             hasMode _ = Nothing
    -- Run tests
    Left (OrderedCmd form) -> mkProver Prover.orderedProve (mkInput fmt form)
    -- Run tests
    Left (TestCmd Nothing) -> do
      S.putStrLn "Running all tests.  This may take awhile."
      M.ignore $ Test.runTestTT tests
    Left (TestCmd (Just t)) -> do
      S.putStrLn $ "Running tests for module: " ++ t
      M.ignore $ Test.runTestTT (getTests t)
    -- Tptp
    Left (TptpCmd prob) -> do
      inp <- mkTptpInput Tptp flags prob
      mkProver Prover.tptpProve inp
    -- Check unicode input and output
    Left (UnicodeCmd s) -> do
      S.putStrLn $ "Found " ++ s ++ "!"
      S.putStrLn "Does this look right?"
    -- Uninterpreted functions
    Left (UninterpCmd form) -> mkProver Prover.uninterpProve (mkInput fmt form)
    -- Print the version number
    Left VersionCmd -> S.putStrLn $ "Version " ++ Misc.version 
 `catchImpossible` \e -> do
   S.putStrLn $ show e
   Exit.exitFailure

-- @ Tests

tests :: Test
tests = "All" ~: 
  TestList [ Imogen.Test.Unif.tests
           -- , Imogen.Test.World.tests
           -- , Imogen.Test.OrdWorld.tests
           , Imogen.Test.TString.tests
           -- , Imogen.Test.Ants.tests
           , Imogen.Test.Seq.tests
           , Imogen.Test.Uninterp.tests
           , Imogen.Test.Linear.tests
           , Imogen.Test.Modal.tests
           , Imogen.Test.Ordered.tests
           -- , Imogen.Test.Dlo.tests
           ]

getTests :: String -> Test
getTests s = case s of
  "Unif" -> Imogen.Test.Unif.tests
  -- "Ants" -> Imogen.Test.Ants.tests
  -- "Norm" -> Imogen.Test.Norm.tests
  "Seq" -> Imogen.Test.Seq.tests
  "Uninterp" -> Imogen.Test.Uninterp.tests
  "Modal" -> Imogen.Test.Modal.tests
  -- "World" -> Imogen.Test.World.tests
  "Linear" -> Imogen.Test.Linear.tests
  -- "OrdWorld" -> Imogen.Test.OrdWorld.tests
  "Ordered" -> Imogen.Test.Ordered.tests
  -- "Dlo" -> Imogen.Test.Dlo.tests
  "TString" -> Imogen.Test.TString.tests
  _ -> error $ "Can't find tests for module: " ++ s
