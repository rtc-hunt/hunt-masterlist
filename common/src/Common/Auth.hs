module Common.Auth where

import Data.Typeable
import Control.Monad.Identity

import Common.Schema

newtype NotAuthToken = AuthToken { unAuthToken :: PrimaryKey Account Identity } deriving (Typeable)
