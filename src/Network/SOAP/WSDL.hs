{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Network.SOAP.WSDL where

import           Control.Exception
import           Control.Monad.Except
import           Data.ByteString.Lazy
import           Data.Char
import           Data.Conduit
import           Data.FileEmbed
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

data WSDL = WSDL
          { wsdlTargetNamespace :: Text
          , wsdlTypes           :: Maybe WSDLTypes
          , wsdlMessages        :: [WSDLMessage]
          , wsdlPortTypes       :: [WSDLPortType]
          , wsdlBindings        :: [WSDLBinding]
          , wsdlServices        :: [WSDLService]
          } deriving Show

data WSDLTypes = WSDLTypes
               { wsdlTypesSchemas :: [WSDLSchema]
               } deriving Show

data WSDLSchema = ImportedSchema Text deriving Show

data WSDLMessage = WSDLMessage
                 { wsdlMessageName  :: Text
                 , wsdlMessageParts :: [WSDLMessagePart]
                 } deriving Show

data WSDLMessagePart = WSDLMessagePart
                     { wsdlMessagePartName :: Name
                     , wsdlMessagePartType :: Text
                     } deriving Show

data WSDLPortType = WSDLPortType
                  { wsdlPortTypeName       :: Text
                  , wsdlPortTypeOperations :: [WSDLOperation]
                  } deriving Show

data WSDLOperation = WSDLOperation
                   { wsdlOperationName   :: Name
                   , wsdlOperationInput  :: Maybe WSDLOperationInput
                   , wsdlOperationOutput :: Maybe WSDLOperationOutput
                   , wsdlOperationFaults :: [WSDLOperationFault]
                   } deriving Show

data WSDLOperationInput = WSDLOperationInput
                        { wsdlOperationInputName    :: Maybe Text
                        , wsdlOperationInputMessage :: Name
                        } deriving Show

data WSDLOperationOutput = WSDLOperationOutput
                         { wsdlOperationOutputName    :: Maybe Text
                         , wsdlOperationOutputMessage :: Name
                         } deriving Show

data WSDLOperationFault = WSDLOperationFault
                        { wsdlOperationFaultName    :: Maybe Text
                        , wsdlOperationFaultMessage :: Name
                        } deriving Show

data WSDLBinding = WSDLBinding
                 { wsdlBindingSoapBinding :: Maybe WSDLSoapBinding
                 , wsdlBindingName        :: Text
                 , wsdlBindingType        :: Name
                 , wsdlBindingOperations  :: [WSDLBindingOperation]
                 } deriving Show

data WSDLSoapBinding = WSDLSoapBinding
                     { wsdlSoapBindingTransport :: Text
                     , wsdlSoapBindingStyle     :: Text
                     } deriving Show

data WSDLBindingOperation = WSDLBindingOperation
                          { wsdlBindingOperationSoapOperation :: Maybe WSDLSoapBindingOperation
                          , wsdlBindingOperationName          :: Text
                          , wsdlBindingOperationInput         :: Maybe WSDLBindingOperationInput
                          , wsdlBindingOperationOutput        :: Maybe WSDLBindingOperationOutput
                          , wsdlBindingOperationFaults        :: [WSDLBindingOperationFault]
                          } deriving Show

data WSDLSoapBindingOperation = WSDLSoapBindingOperation
                              { wsdlSoapBindingOperationAction :: Text
                              , wsdlSoapBindingOperationType   :: Text
                              } deriving Show

data WSDLBindingOperationInput = WSDLBindingOperationInput
                               { wsdlBindingOperationInputName  :: Maybe Text
                               , wsdlBindingOperationInputAttrs :: [WSDLBindingSoapAttribute]
                               } deriving Show

data WSDLBindingOperationOutput = WSDLBindingOperationOutput
                                { wsdlBindingOperationOutputName  :: Maybe Text
                                , wsdlBindingOperationOutputAttrs :: [WSDLBindingSoapAttribute]
                                } deriving Show

data WSDLBindingSoapAttribute = WSDLBindingSoapAttribute
                              { wsdlBindingSoapAttributeName  :: Name
                              , wsdlBindingSoapAttributeAttrs :: Map Name [Content]
                              } deriving Show

data WSDLBindingOperationFault = WSDLBindingOperationFault
                               { wsdlBindingOperationFaultName    :: Maybe Text
                               , wsdlBindingOperationFaultMessage :: Name
                               } deriving Show

data WSDLService = WSDLService
                 { wsdlServiceName  :: Text
                 , wsdlServicePorts :: [WSDLServicePort]
                 } deriving Show

data WSDLServicePort = WSDLServicePort
                     { wsdlServicePortName    :: Text
                     , wsdlServicePortBinding :: Name
                     , wsdlServiceAttrs       :: [WSDLBindingSoapAttribute]
                     } deriving Show

ecommText = fromStrict $(embedFile "data/ecomm-basket.wsdl")

wsdl :: Either SomeException WSDL
wsdl = parseLBS def ecommText $$ ignoreSpaces =$ parseWSDL

ignoreSpaces = forever $ do
    c <- await
    case c of
        Just (EventContent (ContentText t)) | T.all isSpace t -> return ()
        Just x -> yield x
        Nothing -> return ()

parseWSDL :: Sink Event (Either SomeException) WSDL
parseWSDL = force "Missing definitions" $ tagName "{http://schemas.xmlsoap.org/wsdl/}definitions" parseNS $ \ ns -> do
    ts <- parseTypes
    ms <- many parseMessage
    ps <- many parsePortType
    bs <- many parseBinding
    ss <- many parseService
    many ignoreAllTreesContent
    return $ WSDL ns ts ms ps bs ss

parseNS = requireAttr "targetNamespace"

parseTypes = tagNoAttr "{http://schemas.xmlsoap.org/wsdl/}types" $ do
    schemas <- many parseSchema
    return $ WSDLTypes schemas

parseSchema = tagIgnoreAttrs "{http://www.w3.org/2001/XMLSchema}schema" $ do
    importTag <- force "No import schema"
        $ tagName "{http://www.w3.org/2001/XMLSchema}import" (requireAttr "namespace") return
    return $ ImportedSchema importTag

parseMessage = tagName "{http://schemas.xmlsoap.org/wsdl/}message" (requireAttr "name") $ \ n -> do
    parts <- many parsePart
    return $ WSDLMessage n parts

parsePart = tagName "{http://schemas.xmlsoap.org/wsdl/}part" (do
    n <- requireAttr "name"
    t <- requireAttr "type"
    return (n, t)
    ) (\ (n, t) -> return $ WSDLMessagePart (toName n) t)

parsePortType = tagName "{http://schemas.xmlsoap.org/wsdl/}portType" (requireAttr "name") $ \ n -> do
    operations <- many parseOperation
    return $ WSDLPortType n operations

parseOperation = tagName "{http://schemas.xmlsoap.org/wsdl/}operation" (requireAttr "name") $ \ n -> do
    input <- parseOperationAttr "input" WSDLOperationInput
    output <- parseOperationAttr "output" WSDLOperationOutput
    faults <- many parseFault
    return $ WSDLOperation (toName n) input output faults

parseOperationAttr tag con =
    tagName (tag { nameNamespace = Just "http://schemas.xmlsoap.org/wsdl/" })
            (liftM2 (,) (attr "name") (toName <$> requireAttr "message"))
            (return . uncurry con)

parseFault = return Nothing

parseBinding = tagName "{http://schemas.xmlsoap.org/wsdl/}binding"
    (liftM2 (,) (requireAttr "name") (toName <$> requireAttr "type"))
    (\ (n,t) -> do
        soapbind <- parseSoapBinding
        ops <- many parseBindingOperation
        return $ WSDLBinding soapbind n t ops)

parseSoapBinding = tagName "{http://schemas.xmlsoap.org/wsdl/soap/}binding"
    (liftM2 (,) (requireAttr "transport") (requireAttr "style"))
    (return . uncurry WSDLSoapBinding)

parseBindingOperation = tagName "{http://schemas.xmlsoap.org/wsdl/}operation"
    (requireAttr "name")
    (\ n -> do
        sbop <- parseBindingSoapOperation
        input <- parseBindingOperationAttr "input" WSDLBindingOperationInput
        output <- parseBindingOperationAttr "output" WSDLBindingOperationOutput
        return $ WSDLBindingOperation sbop n input output []
    )

parseBindingOperationAttr tag con =
    tagName (tag { nameNamespace = Just "http://schemas.xmlsoap.org/wsdl/" })
            (attr "name")
            (\ n -> do
                ts <- many parseSoapStuff
                return $ con n ts)

parseSoapStuff = tag Just (\ a -> do
    attrs <- grabAttrs
    return $ WSDLBindingSoapAttribute a (M.fromList attrs)
    ) return
    where
        grabAttrs = manyA $ optionalAttrRaw return
        manyA x = do
            x' <- x
            case x' of
                Just y -> (y:) <$> manyA x
                Nothing -> pure []

parseBindingSoapOperation = tagPredicate (\ n -> nameLocalName n == "operation")
    (liftM2 (,) (requireAttr "soapAction") (requireAttr "style"))
    (return . uncurry WSDLSoapBindingOperation)

parseService = tagName "{http://schemas.xmlsoap.org/wsdl/}service" (requireAttr "name") $ \ n -> do
    ports <- many parsePort
    return $ WSDLService n ports

parsePort = tagName "{http://schemas.xmlsoap.org/wsdl/}port"
    (liftM2 (,) (requireAttr "name") (requireAttr "binding"))
    (\ (n,b) -> do
        as <- many parseSoapStuff
        return $ WSDLServicePort n (toName b) as)

toName :: Text -> Name
toName s = case fromString (T.unpack s) of
    Name {..} -> case T.split (== ':') nameLocalName of
                    [pre, x] -> Name x nameNamespace (Just pre)
                    [x] -> Name x nameNamespace Nothing
                    _ -> error "invalid name"
