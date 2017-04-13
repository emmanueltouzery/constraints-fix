{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Lib
    ( someFunc
    ) where
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO as IO
import Data.Monoid
import Data.List

listToPair :: Show a => [a] -> (a,a)
listToPair [a,b] = (a,b)
listToPair v@_ = error ("Not a pair => " ++ show v)

data ConstraintInfo = ConstraintInfo
                      { tableName :: Text
                      , oldConstraintName :: Text
                      , newConstraintName :: Text
                      } deriving Show

formatConstraint :: ConstraintInfo -> Text
formatConstraint ConstraintInfo{..} =
    "ALTER TABLE " <> tableName <> " RENAME CONSTRAINT \"" <> oldConstraintName
    <> "\" to \"" <> newConstraintName <> "\";"

pairConstraints :: (Text, Text) -> (Text,Text) -> ConstraintInfo
pairConstraints (t1, c1) (t2, c2)
    | t1 == t2  = ConstraintInfo t1 c1 c2
    | otherwise = error ("expected " ++ show t1 ++ " == " ++ show t2)

someFunc :: IO ()
someFunc = do
    cts <- T.replace "\n" "" <$> IO.readFile "constraints.txt"
    let scanToMap regex = sortOn fst $ listToPair . snd <$> scan regex cts
    let tableToOldConstraint =
            scanToMap [re|ALTER TABLE (?<table>\w+).+?DROP CONSTRAINT (?<constraint>\w+);|]
    let tableToNewConstraint =
            scanToMap [re|ALTER TABLE (?<table>\w+).+?ADD CONSTRAINT (?<constraint>\w+).+?;|]
    let constraintInfos = zipWith pairConstraints tableToOldConstraint tableToNewConstraint
    IO.putStrLn $ T.intercalate "\n" $ formatConstraint <$> constraintInfos
