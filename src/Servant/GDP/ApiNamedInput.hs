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

module Servant.GDP.ApiNamedInput
  ( ApiName,
    CaptureNamed,
  )
where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, parseJSON)
import GHC.TypeLits (Nat)
import GDP (Defn, defn, type (~~))
import Servant (Capture, FromHttpApiData, parseUrlPiece)

-- | Capture a value from the url path.
-- TODO(Sam): What value does this provide?
type CaptureNamed a = Capture "named input" a

newtype ApiName (n :: Nat) = ApiName Defn
type role ApiName nominal

instance (FromJSON a) => FromJSON (a ~~ ApiName n) where
  parseJSON =
    return . defn <=< parseJSON

instance (FromHttpApiData a) => FromHttpApiData (a ~~ ApiName n) where
  parseUrlPiece t =
    defn <$> parseUrlPiece t
