{-# LANGUAGE OverloadedStrings #-}

module Config.GetOpt (configValueGetOpt) where

import Config

import Data.Either (partitionEithers)
import Data.Foldable (find)
import Data.Tuple (swap)
import Data.Maybe (mapMaybe)
import System.Console.GetOpt
import qualified Data.Text as Text

-- | Process the given list of options using a configuration 'Value'.
-- The configuration value must be a section map. The section names
-- correspond to the long-names of the options.
--
-- Arguments with required parameters should have the option value
-- follow immediately. To omit a parameter to an option, set it to `yes`.
--
-- To omit an option entirely, set it to `no` or remove it from the file.
--
-- Options can be provided as decimal literals or string literals.
configValueGetOpt :: [OptDescr a] -> Value -> ([a], [String])
configValueGetOpt descrs (Sections sections) =
  swap (partitionEithers (mapMaybe (sectionToOpts descrs) sections))
configValueGetOpt _ _ = ([],["invalid configuration value"])

sectionToOpts :: [OptDescr a] -> Section -> Maybe (Either String a)
sectionToOpts _ (Section _ (Atom "no")) = Nothing
sectionToOpts descrs (Section k v) = Just $
  let k' = Text.unpack k in
  case lookupOption k' descrs of
    Nothing -> Left (k' ++ ": unknown option")
    Just o ->
      case optionArgDescr o of
        NoArg  x   -> noArg k' v x
        OptArg f _ -> optArg k' v f
        ReqArg f _ -> reqArg k' v f

noArg :: String -> Value -> a -> Either String a
noArg _ (Atom "yes") x = Right x
noArg k _ _ = Left (k ++ ": invalid parameter, expected `yes` or `no`")

optArg :: String -> Value -> (Maybe String -> a) -> Either String a
optArg _ (Atom "yes") f = Right (f Nothing)
optArg k v f =
  case valueString v of
    Just x -> Right (f (Just x))
    Nothing -> Left (k ++ ": invalid parameter")

reqArg :: String -> Value -> (String -> a) -> Either String a
reqArg k v f =
  case valueString v of
    Just x -> Right (f x)
    Nothing -> Left (k ++ ": invalid parameter")

valueString              :: Value -> Maybe String
valueString (Text t)      = Just (Text.unpack t)
valueString (Number 10 n) = Just (show n)
valueString _             = Nothing

lookupOption :: String -> [OptDescr a] -> Maybe (OptDescr a)
lookupOption name = find $ \o -> name `elem` optionLongNames o

optionLongNames :: OptDescr a -> [String]
optionLongNames (Option _ names _ _) = names

optionArgDescr :: OptDescr a -> ArgDescr a
optionArgDescr (Option _ _ arg _) = arg
