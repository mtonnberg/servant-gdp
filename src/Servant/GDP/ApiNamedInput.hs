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
  ( ApiName0,
    ApiName1,
    ApiName2,
    ApiName3,
    ApiName4,
    ApiName6,
    ApiName7,
    ApiName8,
    ApiName9,
    ApiName10,
    ApiName11,
    ApiName12,
    ApiName13,
    ApiName14,
    ApiName15,
    ApiName16,
    ApiName17,
    ApiName18,
    ApiName19,
    ApiName20,
    CaptureNamed,
  )
where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, parseJSON)
import GDP (Defn, defn, type (~~))
import Servant (Capture, FromHttpApiData, parseUrlPiece)

-- | Capture a value from the url path.
type CaptureNamed a = Capture "named input" a

newtype ApiName0 = ApiName0 Defn

type role ApiName0

instance ApiName ApiName0 where
  doDef = defn

newtype ApiName1 = ApiName1 Defn

type role ApiName1

instance ApiName ApiName1 where
  doDef = defn

newtype ApiName2 = ApiName2 Defn

type role ApiName2

instance ApiName ApiName2 where
  doDef = defn

newtype ApiName3 = ApiName3 Defn

type role ApiName3

instance ApiName ApiName3 where
  doDef = defn

newtype ApiName4 = ApiName4 Defn

type role ApiName4

instance ApiName ApiName4 where
  doDef = defn

newtype ApiName5 = ApiName5 Defn

type role ApiName5

instance ApiName ApiName5 where
  doDef = defn

newtype ApiName6 = ApiName6 Defn

type role ApiName6

instance ApiName ApiName6 where
  doDef = defn

newtype ApiName7 = ApiName7 Defn

type role ApiName7

instance ApiName ApiName7 where
  doDef = defn

newtype ApiName8 = ApiName8 Defn

type role ApiName8

instance ApiName ApiName8 where
  doDef = defn

newtype ApiName9 = ApiName9 Defn

type role ApiName9

instance ApiName ApiName9 where
  doDef = defn

newtype ApiName10 = ApiName10 Defn

type role ApiName10

instance ApiName ApiName10 where
  doDef = defn

newtype ApiName11 = ApiName11 Defn

type role ApiName11

instance ApiName ApiName11 where
  doDef = defn

newtype ApiName12 = ApiName12 Defn

type role ApiName12

instance ApiName ApiName12 where
  doDef = defn

newtype ApiName13 = ApiName13 Defn

type role ApiName13

instance ApiName ApiName13 where
  doDef = defn

newtype ApiName14 = ApiName14 Defn

type role ApiName14

instance ApiName ApiName14 where
  doDef = defn

newtype ApiName15 = ApiName15 Defn

type role ApiName15

instance ApiName ApiName15 where
  doDef = defn

newtype ApiName16 = ApiName16 Defn

type role ApiName16

instance ApiName ApiName16 where
  doDef = defn

newtype ApiName17 = ApiName17 Defn

type role ApiName17

instance ApiName ApiName17 where
  doDef = defn

newtype ApiName18 = ApiName18 Defn

type role ApiName18

instance ApiName ApiName18 where
  doDef = defn

newtype ApiName19 = ApiName19 Defn

type role ApiName19

instance ApiName ApiName19 where
  doDef = defn

newtype ApiName20 = ApiName20 Defn

type role ApiName20

instance ApiName ApiName20 where
  doDef = defn

class ApiName b where
  doDef :: a -> a ~~ b

instance (ApiName n, FromJSON a) => FromJSON (a ~~ n) where
  parseJSON =
    return . doDef <=< parseJSON

instance (ApiName n, FromHttpApiData a) => FromHttpApiData (a ~~ n) where
  parseUrlPiece t =
    doDef <$> parseUrlPiece t
