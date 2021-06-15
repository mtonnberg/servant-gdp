{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.GDP.HumanLanguage
  ( Named,
    SuchThatIt,
    SuchThat,
    extractProof,
    withoutProof,
    withProof,
  )
where

import GDP (Proof, conjure, exorcise, (...), type (:::), type (?), type (~~))

-- | ### Alias for GDP (~~)
-- | A type a named b (used to cross reference proofs)
type Named a b = (~~) a b

-- | ### Alias for GDP (?)
-- | Used instead of SuchThat when you need to exit a named context with the Proof intact
-- (such as returning the data to the api client)
type SuchThatIt a b = (?) a b

-- | ### Alias for GDP (:::)
-- | A value a with the proof p attached to it
type SuchThat a p = (:::) a p

-- | ### Alias for GDP conjure
-- | takes the proof and discards the value. This is a safe operation.
extractProof :: (a ::: p) -> Proof p
extractProof = conjure

-- | ### Alias for GDP exorcise
-- | Takes the value and discards the proof. This is a safe operation.
withoutProof :: (a ::: p) -> a
withoutProof = exorcise

-- | ### Alias for GDP (...)
-- | Attach the proof p to the value a 
withProof :: a -> Proof p -> a ::: p
withProof = (...)
