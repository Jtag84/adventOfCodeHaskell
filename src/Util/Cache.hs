module Util.Cache where

import Data.Map.Strict (Map)
import Data.Map.Strict as Map
import Control.Monad.State (State, modify, MonadState (get))

type Cache key value = Map key value

caching :: Ord key => key -> (key -> value) -> State (Cache key value) value
caching key getValue = do
        inCache <- isInCache
        if inCache 
        then 
            getFromCache
        else do
            let value = getValue key 
            modify (Map.insert key value)
            return value
    where
        isInCache = do
            cache <- get
            return $ key `Map.member` cache

        getFromCache = do
            cache <- get
            return $ cache Map.! key
