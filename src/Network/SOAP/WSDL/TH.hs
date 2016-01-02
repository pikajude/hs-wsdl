{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}

module Network.SOAP.WSDL.TH where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Char
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.XML.Types         hiding (Document, Name)
import qualified Data.XML.Types         as XML
import           GHC.Generics
import           GHC.Stack
import           GHC.TypeLits
import           Language.Haskell.TH
import           Network.SOAP.WSDL
import           Text.XML               (Document)
import           Text.XML.Writer        (XML)
import qualified Text.XML.Writer        as X

type WSDLQ = ReaderT WSDL Q

generateBindings :: WSDL -> Q [Dec]
generateBindings w@WSDL{..} = flip runReaderT w $ do
    ms <- forM wsdlMessages decMessage
    ops <- forM wsdlPortTypes decPort
    binds <- forM wsdlBindings (decBinding w)
    services <- forM wsdlServices decService
    return $ ms ++ concat ops ++ concat binds ++ concat services

decMessage WSDLMessage{..} = lift $
    dataD (return []) conName []
        [recC conName (map (decPart conName) wsdlMessageParts)]
        (sequence [[t|Show|], [t|Eq|], [t|Ord|], [t|Generic|]])
    where conName = tName wsdlMessageName

decPart conName WSDLMessagePart{..} =
    return (mkName (lcfirst (nameBase conName) <> ucfirst (nameBase (nName wsdlMessagePartName))), IsStrict, ConT ''T.Text)
    where
        lcfirst (x:xs) = toLower x : xs
        lcfirst x = x
        ucfirst (x:xs) = toUpper x : xs
        ucfirst x = x

decPort WSDLPortType{..} = do
    let portName = tName $ "P_" <> wsdlPortTypeName
    ins <- mapM (genReturnsInstance wsdlPortTypeName) wsdlPortTypeOperations
    lift $ sequence $
        instanceD (return []) [t|Port $(conT (tName ("P_" <> wsdlPortTypeName)))|] []
        : dataD (return []) (tName $ "P_" <> wsdlPortTypeName) [] [] (return [])
        : map return (concat ins)

mkOpCon WSDLOperation{..} = normalC (nName wsdlOperationName) [inputToVst wsdlOperationInput]

genReturnsInstance portName op@WSDLOperation{..} = do
    ((_, i), (_, o)) <- lift $ liftM2 (,) (inputToVst wsdlOperationInput) (outputToVst wsdlOperationOutput)
    encimpl <- encodeImpl op i o
    lift $ sequence [
        instanceD (return [])
            [t|Operation $(conT (tName ("P_" <> portName)))
                         $(conT (nName wsdlOperationName))
                         |]
            []
      , instanceD (return [])
            [t|PortOperation $(conT (tName ("P_" <> portName)))
                             $(conT (nName wsdlOperationName))
                             $(return i)
                             $(return o)
                             |]
            [return encimpl]
      , dataD (return []) (nName wsdlOperationName) []
          [ recC (nName wsdlOperationName) [] ] (return [])
      ]

encodeImpl WSDLOperation{..} intype outtype = do
    let inputMsgName = wsdlOperationInputMessage (fromJust wsdlOperationInput)
    opmsg <- asks (getMessageByName inputMsgName)
    let mparts = wsdlMessageParts opmsg
    partvars <- lift $ replicateM (length mparts) (newName "arg")
    let mpartvs = zip mparts partvars
        ConT incon = intype
    lift $ funD 'encode [clause [wildP, wildP, conP incon (map varP partvars)]
        (normalB [e|X.element $(liftT inputMsgName) $(doE (map attrStmt mpartvs))|]) []]
    where
        liftT (XML.Name (T.unpack -> a)
                    (fmap T.unpack -> b)
                    (fmap T.unpack -> c))
            = [e|XML.Name (maybe id (\ x y -> T.pack x <> T.pack ":" <> y) c (T.pack a)) Nothing Nothing|]
        getMessageByName (nameLocalName -> nm) wsdl = go (wsdlMessages wsdl) where
            go [] = error $ "Unknown message " ++ show nm
            go (wm:wms) | wsdlMessageName wm == nm = wm
                        | otherwise = go wms
        attrStmt (WSDLMessagePart{..}, n) = noBindS
            [e|X.element $(liftT wsdlMessagePartName) $(varE n)|]

inputToVst = liftM ((,) NotStrict)
    . maybe [t|()|] (conT . nName . wsdlOperationInputMessage)

outputToVst = liftM ((,) NotStrict)
    . maybe [t|()|] (conT . nName . wsdlOperationOutputMessage)

bipType Nothing = [t|()|]
bipType (Just WSDLBindingOperationInput{..}) =
    maybe [t|()|] (conT . tName) wsdlBindingOperationInputName

bopType Nothing = [t|()|]
bopType (Just WSDLBindingOperationOutput{..}) =
    maybe [t|()|] (conT . tName) wsdlBindingOperationOutputName

decBinding wsdl WSDLBinding{..} = lift $ sequence $
    instanceD (return [])
       [t|Binding $(conT (tName ("B_" <> wsdlBindingName)))
                  $(conT (tName ("P_" <> nameLocalName wsdlBindingType)))|] []
    : dataD (return []) (tName $ "B_" <> wsdlBindingName) [] [] (return [])
    : map (decBindingOp wsdlBindingName wsdlBindingType) wsdlBindingOperations

getPortByType WSDL{..} n = go wsdlPortTypes where
    go [] = error $ "Missing port type " ++ show n
    go (pt:pts) | wsdlPortTypeName pt == nameLocalName n = pt
                | otherwise = go pts

getOpByName WSDLPortType{..} n = go wsdlPortTypeOperations where
    go [] = error $ "Missing port operation " ++ show n
    go (op:ops) | wsdlOperationName op == n = op
                | otherwise = go ops

decBindingOp bindname bindtype WSDLBindingOperation{..} = instanceD (return [])
    [t|BindingOperation $(conT (tName ("B_" <> bindname)))
                        $(conT (tName ("P_" <> nameLocalName bindtype)))
                        $(conT (mkName (T.unpack wsdlBindingOperationName)))
    |]
    [funD 'soapAction [clause [wildP, wildP, wildP] (normalB [e|$(litE (stringL actionName))|]) []]]
    where
        actionName = maybe "N/A" (T.unpack . wsdlSoapBindingOperationAction) wsdlBindingOperationSoapOperation

decService WSDLService{..} = do
    spins <- mapM (genSPort wsdlServiceName) wsdlServicePorts
    lift $ sequence $
        dataD (return []) (tName wsdlServiceName) [] [] (return [])
        : instanceD (return []) [t|Service $(conT (tName wsdlServiceName))|] []
        : map return (concat spins)

genSPort sname WSDLServicePort{..} = lift $ sequence
    [
    {- sigD (tName $ "call" <> wsdlServicePortName)
        [t|(MonadIO m, PortOperation $(conT portTypeName) operation input output)
           => Proxy operation -> input -> m output
                            |]
    : -} funD (tName $ "call" <> wsdlServicePortName)
        [clause [] (normalB [e|
            implServicePort (Proxy :: Proxy $(conT (tName sname)))
                            (Proxy :: Proxy $(conT (tName $ "B_" <> nameLocalName wsdlServicePortBinding)))
                            (Proxy :: Proxy $(conT (tName $ "P_" <> wsdlServicePortName)))
                            (Proxy :: Proxy $(conT portTypeName))
                            |]) []]
    , instanceD (return []) [t|ServicePort $(conT (tName sname))
                                           $(conT (tName $ "B_" <> nameLocalName wsdlServicePortBinding))
                                           $(conT (tName $ "P_" <> wsdlServicePortName))
                            |]
        [funD 'soapAddress [clause [wildP, wildP, wildP]
            (normalB (litE (stringL $ T.unpack $ catContent $ getSoapAddress wsdlServiceAttrs)))
            []]]
    ]
    where
        portTypeName = tName $ "P_" <> wsdlServicePortName
        catContent (ContentText t:ts) = t <> catContent ts
        catContent (ContentEntity t:ts) = t <> catContent ts
        catContent [] = ""
        getSoapAddress [] = error "No SOAP address found"
        getSoapAddress (wbsa:wbsas)
            | nameLocalName (wsdlBindingSoapAttributeName wbsa) == "address"
                = fromMaybe (getSoapAddress wbsas) (M.lookup "location" (wsdlBindingSoapAttributeAttrs wbsa))
            | otherwise = getSoapAddress wbsas

tName = mkName . T.unpack

nName = tName . nameLocalName

-- | @\<portType name=\"MyPort\"\>@
class Port portType

-- | An RPC operation. This superclass doesn't strictly represent anything
-- in the WSDL, but is useful to enforce that Bindings and Ports both
-- use the same operations.
class Port portType => Operation portType operation

-- | Associate an input message type and an output message type with an
-- abstract message representation.
class Operation portType operation => PortOperation portType operation input output | portType operation -> input, portType operation -> output where
    encode :: Proxy portType -> operation -> input -> XML
    decode :: Proxy portType -> operation -> Document -> output
    decode = undefined

-- | @\<binding name=\"MyBindingName\" type=\"MyPort\"\>@
class Port portType => Binding bindName portType

-- | Attaches a SOAP action name to an operation, and governs how to
-- marshal input and output messages.
class (Binding bindName portType, Operation portType operation) => BindingOperation bindName portType operation where
    -- | SOAP action name to include in the request header.
    soapAction :: Proxy bindName -> Proxy portType -> operation -> String
    -- | Where the input argument is positioned.
    inputPosition :: Proxy bindName -> Proxy portType -> operation -> SOAPPosition
    -- | Where the output argument is positioned.
    outputPosition :: Proxy bindName -> Proxy portType -> operation -> SOAPPosition

data SOAPPosition = SOAPBody | SOAPHeader

-- | @\<service name=\"MyService\"\>@
class Service service

class (Service service, Binding bindName portType) => ServicePort service bindName portType where
    soapAddress :: Proxy service -> Proxy bindName -> Proxy portType -> String

implServicePort :: (PortOperation portType1 operation input output
                   , BindingOperation bindName portType operation
                   , ServicePort service bindName portType
                   , MonadIO m
                   , ?callStack :: CallStack
                   ) => Proxy service -> Proxy bindName
                   -> Proxy portType -> Proxy portType1
                   -> operation -> input -> m output
implServicePort p1 p2 p3 p4 action input = do
    liftIO $ putStrLn $ "Calling " ++ soapAction p2 p3 action
    liftIO $ putStrLn $ "This endpoint goes to " ++ soapAddress p1 p2 p3
    let encoded = encode p4 action input
        encodedDoc = X.document "root" encoded
    liftIO $ X.pprint encodedDoc
    return $ decode p4 action (undefined encoded)
