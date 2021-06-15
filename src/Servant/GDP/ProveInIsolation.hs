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
import GDP (Proof)
import Servant
  ( FromHttpApiData,
    parseUrlPiece,
  )
import Servant.GDP.HumanLanguage

-- | Check if property P holds for value a or not
class ProvableInIsolation a p where
  proveInIsolation :: a -> Either Text (Proof p)

instance
  ( FromHttpApiData (a `Named` n),
    FromHttpApiData a,
    ProvableInIsolation (a `Named` n) p
  ) =>
  FromHttpApiData (a `Named` n `SuchThat` p)
  where
  parseUrlPiece t =
    do
      (j :: a `Named` n) <- parseUrlPiece t
      (j `withProof`) <$> proveInIsolation j
