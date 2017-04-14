{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Lib
    ( generateConstraintMigrations
    ) where
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO as IO
import Data.Monoid
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data ConstraintMigrationInfo = ConstraintMigrationInfo
    { cstntMigTableName :: Text
    , cstntMigOldConstraintName :: Text
    , cstntMigNewConstraintName :: Text
    } deriving Show

formatConstraint :: ConstraintMigrationInfo -> Text
formatConstraint ConstraintMigrationInfo{..} =
    "ALTER TABLE " <> cstntMigTableName <> " RENAME CONSTRAINT \"" <> cstntMigOldConstraintName
    <> "\" to \"" <> cstntMigNewConstraintName <> "\";"

scanToList :: Regex -> Text -> [[Text]]
scanToList regex cts = sortOn head $ snd <$> scan regex cts

listToPair :: Show a => [a] -> (a,a)
listToPair [a,b] = (a,b)
listToPair list = error ("list not a pair: " ++ show list)

-- return contraint name => spec
readOldConstraintsSpecs :: FilePath -> IO (Map Text Text)
readOldConstraintsSpecs schemaFile = do
    -- must sort so that the last match is kept, the most recent one
    cts <- T.replace "\n" "" <$> IO.readFile schemaFile
    let constraints = scanToList [re|ADD CONSTRAINT (?<constraint>\w+) (.+?);|] cts
    return $ Map.fromList $ listToPair <$> constraints

data NewConstraintInfo = NewConstraintInfo
    { newConstraintTableName :: Text
    , newConstraintName      :: Text
    , newConstraintSpec      :: Text
    }

readNewConstraintInfo :: [Text] -> NewConstraintInfo
readNewConstraintInfo [t,n,s] = NewConstraintInfo t n s
readNewConstraintInfo sth = error $ "readNewConstraintInfo unexpected format " ++ show sth

unsafeMapLookup :: (Ord k, Show k) => String -> Map k a -> k -> a
unsafeMapLookup msg map_ key = Map.findWithDefault (error $ msg <> " " <> show key) key map_

getConstraintMigrationInfo :: Map Text [Text] -> Map Text Text -> NewConstraintInfo -> ConstraintMigrationInfo
getConstraintMigrationInfo tableNameToOldConstraints oldConstraintsSpecs NewConstraintInfo{..} =
    let
        candidateOldConstraintNames =
            unsafeMapLookup ("can't find old constraints for " <> show newConstraintName <> " for table") tableNameToOldConstraints newConstraintTableName
        specMatches = (==) newConstraintSpec
        oldConstraintName =
            fromMaybe (error $ "can't find exact old constraint " ++ show newConstraintTableName ++  " "
                        ++ show newConstraintName ++ " " ++ show candidateOldConstraintNames
                        ++ "\n" ++ show (unsafeMapLookup "can't find old constraint spec" oldConstraintsSpecs <$> candidateOldConstraintNames)
                        ++ "\n" ++ show newConstraintSpec) $
            find (specMatches . unsafeMapLookup "can't find old constraint spec" oldConstraintsSpecs) candidateOldConstraintNames
    in
      ConstraintMigrationInfo newConstraintTableName oldConstraintName newConstraintName

generateConstraintMigrations :: FilePath -> IO ()
generateConstraintMigrations sqlSchemaPath = do
    oldConstraintsSpecs <- readOldConstraintsSpecs sqlSchemaPath
    cts <- T.replace "\n" "" <$> IO.readFile "constraints_drop_add.txt"
    let tableToOldConstraint = Map.fromListWith (++) $ fmap (replicate 1) . listToPair <$>
            scanToList [re|ALTER TABLE (?<table>\w+).+?DROP CONSTRAINT (?<constraint>\w+);|] cts
    let tableToNewConstraint = readNewConstraintInfo <$>
            scanToList [re|ALTER TABLE (?:ONLY)?\s*(?<table>\w+).+?ADD CONSTRAINT (?<constraint>\w+) (.+?);|] cts
    let constraintMigrationInfos =
            getConstraintMigrationInfo tableToOldConstraint oldConstraintsSpecs <$> tableToNewConstraint
    IO.putStrLn $ T.intercalate "\n" $ formatConstraint <$> constraintMigrationInfos
