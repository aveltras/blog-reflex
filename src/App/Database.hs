{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Database where

import qualified Generics.SOP      as SOP
import           RIO
import           Squeal.PostgreSQL hiding (view)

class HasPool env (schema :: SchemasType) where
  poolL :: Lens' env (DbPool schema)

type DbPool (schema :: SchemasType) = Pool (K Connection schema)

data Entity entId entData = Entity { entId :: entId, entData :: entData}
  deriving (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

runPool :: (HasPool env db) => PQ db db (RIO env) a -> RIO env a
runPool session = flip usingConnectionPool session =<< asks view poolL

