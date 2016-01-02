module Network.SOAP.WSDLTest where

import Control.Exception
import Data.Conduit
import Network.SOAP.WSDL
import Text.XML.Stream.Parse

wsdl :: Either SomeException WSDL
wsdl = parseLBS def ecommText $$ ignoreSpaces =$ parseWSDL
