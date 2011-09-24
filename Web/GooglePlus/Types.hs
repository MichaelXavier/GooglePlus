{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus.Types (Person(..),
                             PersonID(..),
                             ActivityCollection(..),
                             ID,
                             Actor(..),
                             Verb(..),
                             ActivityObject(..),
                             ActivityObjectType(..),
                             Provider(..),
                             Access(..),
                             AccessItem(..),
                             AccessItemType(..),
                             Geocode(..),
                             Activity(..),
                             ActivityFeed(..),
                             Attachment(..),
                             AttachmentType(..),
                             Embed(..),
                             Gender(..),
                             PersonName(..),
                             Image(..),
                             Email(..),
                             EmailType(..),
                             PersonURL(..),
                             PersonURLType(..),
                             Organization(..),
                             OrganizationType(..),
                             Place(..),
                             RelationshipStatus(..)) where

import           Data.Aeson.Types (Parser, typeMismatch)
import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import qualified Data.Map as M
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..), zonedTimeToUTC)
import           Data.Time.RFC3339 (readRFC3339)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Network.URL (URL(..), importURL)

type ID = Text

data ActivityCollection = PublicCollection deriving (Show, Eq)

data ActivityFeed = ActivityFeed { activityFeedTitle   :: Text,
                                   activityFeedUpdated :: ZonedTime,
                                   activityFeedId      :: ID,
                                   activityFeedItems   :: [Activity] 
                                   } deriving (Show, Eq)

instance FromJSON ActivityFeed where
  parseJSON (Object v) = ActivityFeed <$> v .: "title"
                                      <*> v .: "updated"
                                      <*> v .: "id"
                                      <*> v .: "items"
  parseJSON v          = typeMismatch "ActivityFeed" v

data Activity = Activity { activityPlaceholder     :: Maybe Bool,
                           activityTitle           :: Text,
                           activityPublished       :: ZonedTime,
                           activityUpdated         :: ZonedTime,
                           activityId              :: ID,
                           activityURL             :: URL,
                           activityActor           :: Actor,
                           activityVerb            :: Verb,
                           activityObject          :: ActivityObject,
                           activityAnnotation      :: Maybe Text,
                           activityCrosspostSource :: Maybe ID, --ID
                           activityProvider        :: Provider,
                           activityAccess          :: Access,
                           activityGeocode         :: Maybe Geocode,
                           activityAddress         :: Maybe Text,
                           activityRadius          :: Maybe Integer, -- meters
                           activityPlaceId         :: Maybe ID,
                           activityPlaceName       :: Maybe Text } deriving (Show, Eq)

--TODO: activities
instance FromJSON Activity where
  parseJSON (Object v) = Activity <$> v .:? "placeholder"
                                  <*> v .:  "title"
                                  <*> v .:  "published"
                                  <*> v .:  "updated"
                                  <*> v .:  "id"
                                  <*> v .:  "url"
                                  <*> v .:  "actor"
                                  <*> v .:| ("verb", Post)
                                  <*> v .:  "object"
                                  <*> v .:? "annotation"
                                  <*> v .:? "crosspostSource"
                                  <*> v .:  "provider"
                                  <*> v .:  "access"
                                  <*> v .:? "geocode"
                                  <*> v .:? "address"
                                  <*> v .:? "radius"
                                  <*> v .:? "placeId"
                                  <*> v .:? "placeName"
  parseJSON v          = typeMismatch "Activity" v

instance Eq ZonedTime where
  a == b = zonedTimeToUTC a == zonedTimeToUTC b

instance FromJSON ZonedTime where
  parseJSON (String str) = maybe (fail $ "Failed to parse ZonedTime " ++ unpack str) pure parsed
    where parsed = readRFC3339 . unpack $ str
  parseJSON v            = typeMismatch "ZonedTime" v


data Actor = Actor { actorDisplayName :: Text,
                     actorId          :: ID,
                     actorImage       :: Image,
                     actorUrl         :: URL } deriving (Show, Eq)

instance FromJSON Actor where
  parseJSON (Object v) = Actor <$> v .:  "displayName"
                               <*> v .:  "id"
                               <*> v .:  "image"
                               <*> v .:  "url"
  parseJSON v          =  typeMismatch "Actor" v

data Verb = Post |
            Checkin |
            Share deriving (Show, Eq)

instance FromJSON Verb where
  parseJSON (String "post")    = pure Post
  parseJSON (String "checkin") = pure Checkin
  parseJSON (String "share")   = pure Share
  parseJSON v                  = typeMismatch "Verb" v

data ActivityObject = ActivityObject { activityObjectActor           :: Maybe Actor,
                                       activityObjectAttachments     :: [Attachment],
                                       activityObjectContent         :: Text,
                                       activityObjectId              :: Maybe ID,
                                       activityObjectType            :: ActivityObjectType,
                                       activityObjectOriginalContent :: Maybe Text,
                                       activityObjectPlusOners       :: Integer,
                                       activityObjectReplies         :: Integer,
                                       activityObjectResharers       :: Integer,
                                       activityObjectURL             :: URL } deriving (Show, Eq)

instance FromJSON ActivityObject where
  parseJSON (Object v) = ActivityObject <$> v .:? "actor"
                                        <*> v .:| ("attachments", [])
                                        <*> v .:  "content"
                                        <*> v .:? "id"
                                        <*> v .:| ("objectType", Note)
                                        <*> v .:? "originalContent"
                                        <*> parseTotalItems "plusoners"
                                        <*> parseTotalItems "replies"
                                        <*> parseTotalItems "resharers"
                                        <*> v .:  "url"
    where parseTotalItems key = maybe (fail $ "failed to find " ++ unpack key ++ "/totalItems in " ++ show v) parseJSON $ totalItems' key
          totalItems' key     = case M.lookup key v of
                                  Just (Object obj) -> M.lookup "totalItems" obj
                                  _                 -> Nothing
  parseJSON v          =  typeMismatch "ActivityObject" v

data ActivityObjectType = Note |
                          GooglePlusActivity deriving (Show, Eq)

instance FromJSON ActivityObjectType where
  parseJSON (String "note")     = pure Note
  parseJSON (String "activity") = pure GooglePlusActivity
  parseJSON v                   = typeMismatch "ActivityObjectType" v

data Attachment = Attachment { attachmentContent     :: Maybe Text,
                               attachmentDisplayName :: Maybe Text,
                               attachmentEmbed       :: Maybe Embed,
                               attachmentFullImage   :: Maybe Image,
                               attachmentId          :: Maybe ID,
                               attachmentImage       :: Maybe Image, -- preview image
                               attachmentType        :: AttachmentType,
                               attachmentURL         :: Maybe URL } deriving (Show, Eq)

instance FromJSON Attachment where
  parseJSON (Object v) = Attachment <$> v .:? "content"
                                    <*> v .:? "displayName"
                                    <*> v .:? "embed"
                                    <*> v .:? "fullImage"
                                    <*> v .:? "id"
                                    <*> v .:? "image"
                                    <*> v .:  "objectType"
                                    <*> v .:? "url"
  parseJSON v          =  typeMismatch "AttachmentType" v

data Embed = Embed { embedType :: Text,
                     embedUrl  :: URL } deriving (Show, Eq)

instance FromJSON Embed where
  parseJSON (Object v) = Embed <$> v .: "type"
                               <*> v .: "url"
  parseJSON v          =  typeMismatch "Embed" v

data AttachmentType = Photo |
                      PhotoAlbum | -- undocumented
                      Video |
                      Article deriving (Show, Eq)

instance FromJSON AttachmentType where
  parseJSON (String "photo")       = pure Photo
  parseJSON (String "photo-album") = pure PhotoAlbum
  parseJSON (String "video")       = pure Video
  parseJSON (String "article")     = pure Article
  parseJSON v                      = typeMismatch "AttachmentType" v

data Geocode = Geocode { latitude  :: Double,
                         longitude :: Double } deriving (Show, Eq)

instance FromJSON Geocode where
  parseJSON (String str) = pure $ Geocode long lat
    where (longT, latT) = spanSkip ' ' str
          long          = read . unpack $ longT
          lat           = read . unpack $ latT
  parseJSON v            =  typeMismatch "Geocode" v

data Access = Access { accessDescription :: Maybe Text,
                       accessItems       :: [AccessItem] } deriving (Show, Eq)

instance FromJSON Access where
  parseJSON (Object v) = Access <$> v .:? "description"
                                <*> v .:  "items"
  parseJSON v          =  typeMismatch "Access" v

data AccessItem = AccessItem { accessItemId   :: Maybe ID,
                               accessItemType :: AccessItemType } deriving (Show, Eq)

instance FromJSON AccessItem where
  parseJSON (Object v) = AccessItem <$> v .:? "id"
                                    <*> v .:  "type"
  parseJSON v          =  typeMismatch "AccessItem" v

data AccessItemType = PersonAccess |
                      CircleAccess |
                      MyCirclesAccess |
                      ExtendedCirclesAccess |
                      PublicAccess deriving (Show, Eq)

instance FromJSON AccessItemType where
  parseJSON (String "person")          = pure PersonAccess
  parseJSON (String "circle")          = pure CircleAccess
  parseJSON (String "myCircles")       = pure MyCirclesAccess
  parseJSON (String "extendedCircles") = pure ExtendedCirclesAccess
  parseJSON (String "public")          = pure PublicAccess
  parseJSON v                          = typeMismatch "AccessItemType" v

data Provider = Provider { providerTitle :: Text } deriving (Show, Eq)

instance FromJSON Provider where
  parseJSON (Object v) = Provider <$> v .: "title"
  parseJSON v          =  typeMismatch "Provider" v

data Person = Person { personId                 :: ID,
                       personDisplayName        :: Text,
                       personName               :: Maybe PersonName,
                       personNickName           :: Maybe Text,
                       personTagline            :: Maybe Text,
                       personBirthday           :: Maybe Day,
                       personGender             :: Maybe Gender,
                       personAboutMe            :: Maybe Text,
                       personCurrentLocation    :: Maybe Text,
                       personRelationshipStatus :: Maybe RelationshipStatus,
                       profileURL               :: URL,
                       personImage              :: Image,
                       personEmails             :: [Email],
                       personURLs               :: [PersonURL],
                       personOrganizations      :: [Organization],
                       personPlacesLived        :: [Place],
                       personLanguagesSpoken    :: [Language],
                       personHasApp             :: Maybe Bool } deriving (Show, Eq)

instance FromJSON Person where
  parseJSON (Object v) = Person <$> v .:  "id"
                                <*> v .:  "displayName"
                                <*> v .:? "name"
                                <*> v .:? "nickname"
                                <*> v .:? "tagline"
                                <*> v .:? "birthday"
                                <*> v .:? "gender"
                                <*> v .:? "aboutMe"
                                <*> v .:? "currentLocation"
                                <*> v .:? "relationshipStatus"
                                <*> v .:  "url"
                                <*> v .:  "image"
                                <*> v .:| ("emails", [])
                                <*> v .:| ("urls", [])
                                <*> v .:| ("organizations", [])
                                <*> v .:| ("placesLived", [])
                                <*> v .:| ("languagesSpoken", [])
                                <*> v .:? "hasApp"
  parseJSON v          = typeMismatch "Person" v

instance FromJSON Day where
  parseJSON (String str) = pure . read . unpack $ str
  parseJSON v            = typeMismatch "Day" v

-- Maybe there will be more types in the future?
data Gender = Male | Female | OtherGender deriving (Show, Eq)

instance FromJSON Gender where
  parseJSON (String "male")   = pure Male
  parseJSON (String "female") = pure Female
  parseJSON (String "other")  = pure OtherGender
  parseJSON v                 = typeMismatch "Gender" v


data PersonID = PersonID Text | Me deriving (Show, Eq)

data PersonName = PersonName { familyName      :: Text,
                               formatted       :: Text,
                               givenName       :: Text,
                               honorificPrefix :: Text,
                               honorificSuffix :: Text,
                               middleName      :: Text } deriving (Show, Eq)

instance FromJSON PersonName where
  parseJSON (Object v) = PersonName <$> v .: "familyName"
                                    <*> v .: "formatted"
                                    <*> v .: "givenName"
                                    <*> v .: "honorificPrefix"
                                    <*> v .: "honorificSuffix"
                                    <*> v .: "middleName"
  parseJSON v          = typeMismatch "PersonName" v

data Image = Image { imageURL :: URL,
                     imageType :: Maybe Text,
                     imageWidth :: Maybe Integer,
                     imageHeight :: Maybe Integer } deriving (Show, Eq)

instance FromJSON Image where
  parseJSON (Object v) = Image <$> v .:  "url"
                               <*> v .:? "type"
                               <*> v .:? "width"
                               <*> v .:? "height"
  parseJSON v          = typeMismatch "Image" v

data Email = Email { emailPrimary      :: Bool,
                     emailType         :: EmailType,
                     emailAddressValue :: Text } deriving (Show, Eq)

instance FromJSON Email where
  parseJSON (Object v) = Email <$> v .: "primary"
                               <*> v .: "type"
                               <*> v .: "value"
  parseJSON v          = typeMismatch "Email" v

data EmailType = HomeEmail | WorkEmail | OtherEmail deriving (Show, Eq)

instance FromJSON EmailType where
  parseJSON (String "home")  = pure HomeEmail
  parseJSON (String "work")  = pure WorkEmail
  parseJSON (String "other") = pure OtherEmail
  parseJSON v                = typeMismatch "EmailType" v

data PersonURL = PersonURL { personUrlPrimary :: Bool,
                             personUrlType    :: PersonURLType,
                             personURLValue   :: URL } deriving (Show, Eq)

instance FromJSON PersonURL where
  parseJSON (Object v) = PersonURL <$> v .:| ("primary", False)
                                   <*> v .:  "type"
                                   <*> v .:  "value"
  parseJSON v          = typeMismatch "PersonURL" v

data PersonURLType = HomeURL |
                     WorkURL |
                     BlogURL |
                     ProfileURL |
                     JsonURL | -- this is not a documented value, yet I've encountered it in the wild. Lame
                     OtherURL deriving (Show, Eq)

instance FromJSON PersonURLType where
  parseJSON (String "home")    = pure HomeURL
  parseJSON (String "work")    = pure WorkURL
  parseJSON (String "blog")    = pure BlogURL
  parseJSON (String "profile") = pure ProfileURL
  parseJSON (String "other")   = pure OtherURL
  parseJSON (String "json")    = pure JsonURL
  parseJSON v                  = typeMismatch "PersonURLType" v

instance FromJSON URL where
  parseJSON (String str) = maybe (fail $ "Failed to parse URL " ++ unpack str) pure parsed
    where parsed = importURL . unpack $ str
  parseJSON v          = typeMismatch "URL" v


data Organization = Organization { organizationDepartment  :: Maybe Text,
                                   organizationDescription :: Maybe Text,
                                   organizationEndDate     :: Maybe Text, -- What format is this going to be in?
                                   organizationLocation    :: Maybe Text,
                                   organizationName        :: Text,
                                   organizationPrimary     :: Bool,
                                   organizationstartDate   :: Maybe Text, -- same here
                                   organizationTitle       :: Text,
                                   organizationType        :: OrganizationType } deriving (Show, Eq)

instance FromJSON Organization where
  parseJSON (Object v) = Organization <$> v .:? "department"
                                      <*> v .:? "description"
                                      <*> v .:? "endDate"
                                      <*> v .:? "location"
                                      <*> v .:  "name"
                                      <*> v .:| ("primary", False)
                                      <*> v .:? "startDate"
                                      <*> v .:  "title"
                                      <*> v .:  "type"
  parseJSON v          = typeMismatch "Organization" v

data OrganizationType = Work | School deriving (Show, Eq)

instance FromJSON OrganizationType where
  parseJSON (String "work")   = pure Work -- spec says job. fail.
  parseJSON (String "school") = pure School
  parseJSON v                = typeMismatch "OrganizationType" v

data Place = Place { placePrimary :: Bool,
                     placeValue   :: Text } deriving (Show, Eq)

instance FromJSON Place where
  parseJSON (Object v) = Place <$> v .: "primary"
                               <*> v .: "value"
  parseJSON v          = typeMismatch "Place" v

type Language = Text

data RelationshipStatus = Single |
                          InARelationship |
                          Engaged |
                          Married |
                          ItsComplicated |
                          OpenRelationship |
                          Widowed |
                          InDomesticPartnership |
                          InCivilUnion deriving (Show, Eq)

instance FromJSON RelationshipStatus where
  parseJSON (String "single")                  = pure Single
  parseJSON (String "in_a_relationship")       = pure InARelationship
  parseJSON (String "engaged")                 = pure Engaged
  parseJSON (String "married")                 = pure Married
  parseJSON (String "its_complicated")         = pure ItsComplicated
  parseJSON (String "open_relationship")       = pure OpenRelationship
  parseJSON (String "widowed")                 = pure Widowed
  parseJSON (String "in_domestic_partnership") = pure InDomesticPartnership
  parseJSON (String "in_civil_union")          = pure InCivilUnion
  parseJSON v                                  = typeMismatch "RelationshipStatus" v

(.:|) :: (FromJSON a) => Object -> (Text, a) -> Parser a
obj .:| (key, d) = case M.lookup key obj of
                        Nothing -> pure d
                        Just v  -> parseJSON v

spanSkip :: Char -> Text -> (Text, Text)
spanSkip cond xs = (left, T.tail right)
  where (left, right) = T.span (/= cond) xs
