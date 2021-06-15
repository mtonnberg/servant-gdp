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

module Servant.GDP
  ( module Servant.GDP.ApiNamedInput,
    module Servant.GDP.HumanLanguage,
    module Servant.GDP.ProveInIsolation,
  )
where

import Servant.GDP.ApiNamedInput
import Servant.GDP.HumanLanguage
import Servant.GDP.ProveInIsolation

import Data.Aeson (ToJSON, toJSON)
import GDP (exorcise, rename, the, type (:::), type (?), type (~~))

instance (ToJSON a) => ToJSON (a ~~ p) where
  toJSON = toJSON . the

instance (ToJSON a) => ToJSON (a ::: p) where
  toJSON = toJSON . exorcise

instance (ToJSON a) => ToJSON (a ? p) where
  toJSON x =
    rename x toJSON
