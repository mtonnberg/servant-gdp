{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.GDP.ProveInIsolation (ProvableInIsolation, proveInIsolation) where

import Data.Text (Text)
import GDP (Proof, introAnd, introOrL, introOrR, (...), type (&&), type (:::), type (||), type (~~))
import Servant
  ( FromHttpApiData,
    parseUrlPiece,
  )

-- | Check if property P holds for value a or not.
-- Is used to parse a value from the request with a required proof
class ProvableInIsolation a p where
  proveInIsolation :: a -> Either Text (Proof p)

instance (ProvableInIsolation a p1, ProvableInIsolation a p2) => ProvableInIsolation a (p1 && p2) where
  proveInIsolation x = do
    p1 <- proveInIsolation x
    p2 <- proveInIsolation x
    return $ introAnd p1 p2

instance (ProvableInIsolation a p1, ProvableInIsolation a p2) => ProvableInIsolation a (p1 || p2) where
  proveInIsolation x = do
    case (proveInIsolation x, proveInIsolation x) of
      (Right p1, _) -> return $ introOrL p1
      (_, Right p2) -> return $ introOrR p2
      (Left e1, Left e2) -> Left $ e1 <> e2

instance
  ( FromHttpApiData (a ~~ n),
    FromHttpApiData a,
    ProvableInIsolation (a ~~ n) p
  ) =>
  FromHttpApiData (a ~~ n ::: p)
  where
  parseUrlPiece t =
    do
      (j :: a ~~ n) <- parseUrlPiece t
      (j ...) <$> proveInIsolation j
