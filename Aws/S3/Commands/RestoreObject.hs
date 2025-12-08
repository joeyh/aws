{-# LANGUAGE CPP #-}
module Aws.S3.Commands.RestoreObject
where

import           Aws.Core
import           Aws.S3.Core
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML as XML
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif
import           Prelude

data RestoreObject
  = RestoreObject { roObjectName :: Object
                  , roBucket :: Bucket
                  , roVersionId :: Maybe T.Text
                  , roTier :: RestoreObjectTier
                  , roObjectLifetimeDays :: RestoreObjectLifetimeDays
                  }
  deriving (Show)

data RestoreObjectTier
  = RestoreObjectTierExpedited
  | RestoreObjectTierStandard
  | RestoreObjectTierBulk
  deriving (Show)

data RestoreObjectLifetimeDays = RestoreObjectLifetimeDays Integer
  deriving (Show)

restoreObject :: Bucket -> T.Text -> RestoreObjectTier -> RestoreObjectLifetimeDays -> RestoreObject
restoreObject bucket obj tier lifetime = RestoreObject obj bucket Nothing tier lifetime

data RestoreObjectResponse
  = RestoreObjectAccepted
  | RestoreObjectAlreadyRestored
  | RestoreObjectAlreadyInProgress
  deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery RestoreObject where
    type ServiceConfiguration RestoreObject = S3Configuration
    signQuery RestoreObject {..} = s3SignQuery S3Query
      { s3QMethod = Post
      , s3QBucket = Just $ T.encodeUtf8 roBucket
      , s3QObject = Just $ T.encodeUtf8 roObjectName
      , s3QSubresources = HTTP.toQuery
         [ Just ( "restore" :: B8.ByteString, Nothing :: Maybe T.Text)
         , case roVersionId of
           Nothing -> Nothing
           Just v -> Just ("versionId" :: B8.ByteString, Just v)
         ]
      , s3QQuery = []
      , s3QContentType = Nothing
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = []
      , s3QOtherHeaders = []
      , s3QRequestBody = (Just . HTTP.RequestBodyLBS . XML.renderLBS XML.def)
         XML.Document
          { XML.documentPrologue = XML.Prologue [] Nothing []
          , XML.documentRoot = XML.Element
            { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}RestoreRequest"
            , XML.elementAttributes = M.empty
            , XML.elementNodes =
              [ XML.NodeElement (XML.Element
                { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}Days"
                , XML.elementAttributes = M.empty
                , XML.elementNodes = case roObjectLifetimeDays of
                        RestoreObjectLifetimeDays n -> [XML.NodeContent (T.pack (show n))]
                })
              , XML.NodeElement (XML.Element
                { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}GlacierJobParameters"
                , XML.elementAttributes = M.empty
                , XML.elementNodes =
                  [ XML.NodeElement (XML.Element
                    { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}Tier"
                    , XML.elementAttributes = M.empty
                    , XML.elementNodes = case roTier of
                      RestoreObjectTierExpedited -> [XML.NodeContent "Expedited"]
                      RestoreObjectTierStandard ->  [XML.NodeContent "Standard"]
                      RestoreObjectTierBulk ->      [XML.NodeContent "Bulk"] 
                    })
                  ]
                })
              ]
            }
          , XML.documentEpilogue = []
          }
      }

instance ResponseConsumer RestoreObject RestoreObjectResponse where
    type ResponseMetadata RestoreObjectResponse = S3Metadata
    responseConsumer httpReq _ _ resp
        | status == HTTP.status202 = return RestoreObjectAccepted
        | status == HTTP.status200 = return RestoreObjectAlreadyRestored
        | status == HTTP.status409 = return RestoreObjectAlreadyInProgress
        | otherwise = throwStatusCodeException httpReq resp
      where
        status = HTTP.responseStatus resp

instance Transaction RestoreObject RestoreObjectResponse

instance AsMemoryResponse RestoreObjectResponse where
    type MemoryResponse RestoreObjectResponse = RestoreObjectResponse
    loadToMemory = return
