{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Network.SOAP.WSDL.THTest where

import Data.Proxy
import Network.SOAP.WSDL
import Network.SOAP.WSDL.TH

generateBindings (case wsdl of Right e -> e)
