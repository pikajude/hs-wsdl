{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveGeneric      #-}
#define GENERIC , Generic
#else
#define GENERIC
#endif

#define INSTANCES (Eq, Show, Typeable GENERIC)

-- | A description of WSDLs. Most of the verbiage here is copied verbatim
-- from the WSDL spec: see http://www.w3.org/TR/wsdl.
--
-- TODO:
--   * Documentation support
--   * Schema support
module Text.XML.WSDL.Types where

import Data.Text      (Text)
import Data.Typeable  (Typeable)
import Data.XML.Types (Name, Node)
import Network.URI    (URI)
#ifdef __GLASGOW_HASKELL__
import GHC.Generics
#endif

-- | Represents a WSDL document; this contains the six WSDL elements as
-- described below, as well as @name@ and @targetNamespace@ as defined in
-- the spec.
--
-- WSDL documents can be assigned an optional @name@ attribute that serves
-- as a lightweight form of documentation. Optionally, a @targetNamespace@
-- attribute may be specified. This represents the URI associated with the
-- WSDL.
data WSDL = WSDL
          { targetNamespace    :: Maybe URI
          , documentName       :: Maybe Name
          , types              :: Maybe WSDLTypes
          , messages           :: [WSDLMessage]
          , portTypes          :: [WSDLPortType]
          , bindings           :: [WSDLBinding]
          , services           :: [WSDLService]
          , additionalWSDLInfo :: [ExtensibilityElement]
          } deriving INSTANCES

-- * Document elements

-- *** Type definitions
-- | Data type definitions used to describe the messages exchanged.
data WSDLTypes = WSDLTypes
               { schemas            :: [Schema]
               , additionalTypeInfo :: [ExtensibilityElement]
               } deriving INSTANCES

-- *** Messages
-- | Represents an abstract definition of the data being transmitted.
-- A message consists of logical parts, each of which is associated with
-- a definition within some type system.
data WSDLMessage = WSDLMessage
                 { wsdlMessageName  :: Text
                 , wsdlMessageParts :: [WSDLMessagePart]
                 } deriving INSTANCES

-- | Messages consist of one or more logical 'WSDLMessagePart's.
data WSDLMessagePart = WSDLMessagePart
                     { wsdlMessagePartName    :: Name
                     , wsdlMessagePartElement :: Maybe Name
                     , wsdlMessagePartType    :: Maybe Name
                     } deriving INSTANCES

-- *** Port types
-- | A set of abstract operations. Each operation refers to an input
-- 'WSDLMessage' and output 'WSDLMessage's.
--
-- NB: Though the WSDL spec explicitly mentions that each operation refers
-- to an input message, the grammar provided on the W3 site makes
-- @\<input\>@ optional. The wording of the spec, rather than the example
-- grammar, is followed here.
data WSDLPortType = WSDLPortType
                  { wsdlPortTypeName       :: Text
                  , wsdlPortTypeOperations :: [AbstractOperation]
                  } deriving INSTANCES

-- | A 'WSDLPortType' is a named set of 'AbstractOperation's and the
-- abstract messages involved.
data AbstractOperation = AbstractOneWayOperation
                       { operationName           :: Text
                       , operationInput          :: InputMessage
                       , operationParameterOrder :: [Text] }
                       | AbstractRequestResponseOperation
                       { operationName           :: Text
                       , operationInput          :: InputMessage
                       , operationOutput         :: OutputMessage
                       , operationFaults         :: [FaultMessage]
                       , operationParameterOrder :: [Text] }
                       | AbstractSolicitResponseOperation
                       { operationName           :: Text
                       , operationOutput         :: OutputMessage
                       , operationInput          :: InputMessage
                       , operationFaults         :: [FaultMessage]
                       , operationParameterOrder :: [Text] }
                       | AbstractNotificationOperation
                       { operationName           :: Text
                       , operationOutput         :: OutputMessage
                       , operationParameterOrder :: [Text] }
                       deriving INSTANCES

-- | An abstract message representing input to the given operation.
data InputMessage = InputMessage
                  { inputMessageName :: Maybe Text
                  , inputMessageType :: Name
                  } deriving INSTANCES

-- | An abstract message representing output from the given operation.
data OutputMessage = OutputMessage
                   { outputMessageName :: Maybe Text
                   , outputMessageType :: Name
                   } deriving INSTANCES

-- | An abstract message representing faults returned by the given operation.
data FaultMessage = FaultMessage
                  { faultMessageName :: Text
                  , faultMessageType :: Name
                  } deriving INSTANCES

-- *** Bindings
-- | Specifies a concrete protocol and data format specifications for the
-- operations and messages defined by a particular 'WSDLPortType'.
data WSDLBinding = WSDLBinding
                 { wsdlBindingName       :: Text
                 , wsdlBindingType       :: Name
                 , wsdlBindingOperations :: [ConcreteOperation]
                 , additionalBindingInfo :: [ExtensibilityElement]
                 } deriving INSTANCES

-- | Describes a concrete operation.
data ConcreteOperation = ConcreteOperation
                       { cOperationName          :: Text
                       , cOperationInput         :: Maybe ConcreteInputMessage
                       , cOperationOutput        :: Maybe ConcreteOutputMessage
                       , cOperationFault         :: [ConcreteFaultMessage]
                       , additionalOperationInfo :: [ExtensibilityElement]
                       } deriving INSTANCES

-- | A concrete message representing input to the given operation.
data ConcreteInputMessage = ConcreteInputMessage
                          { cInputMessageName           :: Maybe Text
                          , additionalConcreteInputInfo :: [ExtensibilityElement]
                          } deriving INSTANCES

-- | A concrete message representing output from the given operation.
data ConcreteOutputMessage = ConcreteOutputMessage
                           { cOutputMessageName           :: Maybe Text
                           , additionalConcreteOutputInfo :: [ExtensibilityElement]
                           } deriving INSTANCES

-- | A concrete message representing faults returned by the given operation.
data ConcreteFaultMessage = ConcreteFaultMessage
                          { cFaultMessageName           :: Text
                          , additionalConcreteFaultInfo :: [ExtensibilityElement]
                          } deriving INSTANCES

-- *** Services
-- | Used to aggregate a set of related 'WSDLPort's.
data WSDLService = WSDLService
                 { wsdlServiceName       :: Text
                 , wsdlServicePorts      :: [WSDLPort]
                 , additionalServiceInfo :: [ExtensibilityElement]
                 } deriving INSTANCES

-- *** Ports
-- | Specifies the address for a 'WSDLBinding', thus defining a single
-- communication endpoint.
data WSDLPort = WSDLPort
              { wsdlPortName       :: Text
              , wsdlPortBinding    :: Name
              , additionalPortInfo :: [ExtensibilityElement]
              } deriving INSTANCES

-- * Additional properties
-- *** Extensibility elements
-- | Certain tags in a WSDL document can have additional information
-- associated with them by way of Extensibility Elements. These are
-- generally used to specify binding information for a specific protocol
-- or format (e.g. SOAP).
type ExtensibilityElement = Node

-- *** Documentation
-- | Documentation can appear inside any WSDL element.
type Documentation = Maybe Node

-- | Placeholder
data Schema = Schema deriving INSTANCES
