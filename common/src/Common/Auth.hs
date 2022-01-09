module Common.Auth where

import Data.Typeable
import Control.Monad.Identity

import Common.Schema

newtype AuthToken = AuthToken { unAuthToken :: PrimaryKey Account Identity } deriving (Typeable)
