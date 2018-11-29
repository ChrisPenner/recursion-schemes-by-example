{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JSONF where

import           Data.Functor.Foldable
import qualified Data.Map                      as M
import           Text.Show.Deriving
import           Data.Eq.Deriving
import qualified Data.Aeson                    as A
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Bifunctor
import           Data.Maybe
import           Data.ByteString.Lazy          as BS
                                                   hiding ( pack, unpack )
import           Data.ByteString.Lazy.Char8    as BS

data JSON
  = Object (M.Map String JSON)
  | Array [JSON]
  | String String
  | Number Double
  | Bool Bool
  | Null
  deriving (Eq, Show)

data JSONF r
  = ObjectF (M.Map String r)
  | ArrayF [r]
  | StringF String
  | NumberF Double
  | BoolF Bool
  | NullF
  deriving (Show, Eq, Functor)

deriveShow1 ''JSONF
deriveEq1 ''JSONF

type instance Base JSON = JSONF

instance Recursive JSON where
  project (Object obj) = ObjectF obj
  project (Array  arr) = ArrayF arr
  project (String s  ) = StringF s
  project (Number n  ) = NumberF n
  project (Bool b) = BoolF b
  project Null     = NullF

instance Corecursive JSON where
  embed (ObjectF o) = Object o
  embed (ArrayF  a) = Array a
  embed (StringF s) = String s
  embed (NumberF n) = Number n
  embed (BoolF b) = Bool b
  embed NullF     = Null

--------------------------------------------------------------------------------
-- Aeson serialization/deserialization
--------------------------------------------------------------------------------

-- Orphan type family instance
type instance Base A.Value = JSONF

instance A.ToJSON (Fix JSONF) where
  toJSON = refix

instance A.FromJSON (Fix JSONF) where
  parseJSON = return . refix

instance A.ToJSON JSON where
  toJSON = refix

instance A.FromJSON JSON where
  parseJSON = return . refix

instance Recursive A.Value where
  project (A.Object obj) = ObjectF (M.fromList . fmap (first T.unpack) . HM.toList $ obj)
  project (A.Array  arr) = ArrayF $ V.toList arr
  project (A.String s  ) = StringF $ T.unpack s
  project (A.Number n  ) = NumberF doubleN
    where doubleN = fromRational . toRational $ n
  project (A.Bool b) = BoolF b
  project A.Null     = NullF

instance Corecursive A.Value where
  embed (ObjectF obj) = A.Object (HM.fromList . fmap (first T.pack) . M.toList $ obj)
  embed (ArrayF  arr) = A.Array $ V.fromList arr
  embed (StringF s) = A.String (T.pack s)
  embed (NumberF n) = A.Number scientificN
    where scientificN = fromRational . toRational $ n
  embed (BoolF b) = A.Bool b
  embed NullF     = A.Null

-- A simple helper to parse JSON for use in examples
jsonFromString :: String -> JSON
jsonFromString = fromJust . A.decode . BS.pack

jsonToString :: JSON -> String
jsonToString = BS.unpack . A.encode
