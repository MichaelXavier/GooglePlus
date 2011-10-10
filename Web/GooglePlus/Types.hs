--------------------------------------------------------------------
-- |
-- Module      : Web.GooglePlus.Types
-- Description : Types returned by the Google+ API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus.Types (Person(..),
                             PersonSearchResult(..),
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

import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Map as M
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..), zonedTimeToUTC)
import           Data.Time.RFC3339 (readRFC3339)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Network.URL (URL(..), importURL)

type ID = Text

---------- Activity Types

-- |A feed of user activity
data ActivityFeed = ActivityFeed { activityFeedTitle   :: Text,      -- ^ Title of the feed in Google+
                                   activityFeedUpdated :: ZonedTime, -- ^ Time updated
                                   activityFeedId      :: ID,        -- ^ Unique ID of the feed
                                   activityFeedItems   :: [Activity] -- ^ Activities in the feed (currently limited to first page
                                   } deriving (Show, Eq)

-- |Specifies the type of Activities to get. Currently the API only allows public.
data ActivityCollection = PublicCollection deriving (Show, Eq)

instance FromJSON ActivityFeed where
  parseJSON (Object v) = ActivityFeed <$> v .: "title"
                                      <*> v .: "updated"
                                      <*> v .: "id"
                                      <*> v .: "items"
  parseJSON v          = typeMismatch "ActivityFeed" v

-- |Activity on Google+, such as a post
data Activity = Activity { activityPlaceholder     :: Maybe Bool,     -- ^ Meaning undocumented
                           activityTitle           :: Text,           -- ^ Title of the activity
                           activityPublished       :: ZonedTime,      -- ^ Date originally published
                           activityUpdated         :: ZonedTime,      -- ^ Date updated
                           activityId              :: ID,             -- ^ Activity ID
                           activityURL             :: URL,            -- ^ URL to view the Activity
                           activityActor           :: Actor,          -- ^ The person who performed the Activity
                           activityVerb            :: Verb,           -- ^ Indicates what action was performed
                           activityObject          :: ActivityObject, -- ^ The object of the Activity
                           activityAnnotation      :: Maybe Text,     -- ^ Additional content added by the person who shared this activity, applicable only when resharing an activity
                           activityCrosspostSource :: Maybe ID,       -- ^ ID of original activity if this activity is a crosspost from another system
                           activityProvider        :: Provider,       -- ^ Service provider initially providing the activity
                           activityAccess          :: Access,         -- ^ Identifies who has access to this activity
                           activityGeocode         :: Maybe Geocode,  -- ^ Where the activity occurred (Latitude/Longitude)
                           activityAddress         :: Maybe Text,     -- ^ Street address where the activity occurred
                           activityRadius          :: Maybe Integer,  -- ^ Radius of the region where the activity ocurred, centered at the Geocode
                           activityPlaceId         :: Maybe ID,       -- ^ ID of the place where the activity occurred
                           activityPlaceName       :: Maybe Text
                         } deriving (Show, Eq)

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


-- |A person who may be associated with an Activity
data Actor = Actor { actorDisplayName :: Text,  -- ^ The public display name of the Actor
                     actorId          :: ID,    -- ^ The ID of the Actor
                     actorImage       :: Image, -- ^ Data pertaining to the Actor's main profile image
                     actorUrl         :: URL    -- ^ URL of the user's profile
                   } deriving (Show, Eq)

instance FromJSON Actor where
  parseJSON (Object v) = Actor <$> v .:  "displayName"
                               <*> v .:  "id"
                               <*> v .:  "image"
                               <*> v .:  "url"
  parseJSON v          =  typeMismatch "Actor" v

-- |Type of activity being performed
data Verb = Post |    -- ^ Publish content to the stream
            Checkin | -- ^ Check into a location
            Share     -- ^ Reshare an activity
            deriving (Show, Eq)

instance FromJSON Verb where
  parseJSON (String "post")    = pure Post
  parseJSON (String "checkin") = pure Checkin
  parseJSON (String "share")   = pure Share
  parseJSON v                  = typeMismatch "Verb" v

-- |Object to which an activity pertains
data ActivityObject = ActivityObject { activityObjectActor           :: Maybe Actor,        -- ^ If the object is another Activity, this refers to the actor for that Activity
                                       activityObjectAttachments     :: [Attachment],       -- ^ Media objects attached to this activity object
                                       activityObjectContent         :: Text,               -- ^ Snipped of text if the object is an article
                                       activityObjectId              :: Maybe ID,           -- ^ ID of the media object's resource
                                       activityObjectType            :: ActivityObjectType, -- ^ Type of Object
                                       activityObjectOriginalContent :: Maybe Text,         -- ^ Content text as provided by the author without any HTML formatting
                                       activityObjectPlusOners       :: Integer,            -- ^ Number of people giving the Activity a +1
                                       activityObjectReplies         :: Integer,            -- ^ Number of replies to the Activity
                                       activityObjectResharers       :: Integer,            -- ^ Number of people resharing the Activity
                                       activityObjectURL             :: URL                 -- ^ URL pointing to the linked resource
                                     } deriving (Show, Eq)

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

-- |Types of objects that can be associated with an Activity
data ActivityObjectType = Note |             -- ^ Textual content
                          GooglePlusActivity -- ^ A Google+ Activity
                          deriving (Show, Eq)

instance FromJSON ActivityObjectType where
  parseJSON (String "note")     = pure Note
  parseJSON (String "activity") = pure GooglePlusActivity
  parseJSON v                   = typeMismatch "ActivityObjectType" v

-- |Media attached to an Activity
data Attachment = Attachment { attachmentContent     :: Maybe Text,     -- ^ Snippet of text if the Attachment is an article
                               attachmentDisplayName :: Maybe Text,     -- ^ Title of the Attachment
                               attachmentEmbed       :: Maybe Embed,    -- ^ Embeddable link if the Attachment is a video
                               attachmentFullImage   :: Maybe Image,    -- ^ Full image if the Attachment is a photo
                               attachmentId          :: Maybe ID,       -- ^ ID of the Attachment's resource
                               attachmentImage       :: Maybe Image,    -- ^ Preview image
                               attachmentType        :: AttachmentType, -- ^ Type of attachment
                               attachmentURL         :: Maybe URL       -- ^ Lin k to text/html attachment
                             } deriving (Show, Eq)

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

-- |Embeddable link for an attachment if it is a video
data Embed = Embed { embedType :: Text, -- ^ Type of embeddable link
                     embedUrl  :: URL   -- ^ Embeddable link
                   } deriving (Show, Eq)

instance FromJSON Embed where
  parseJSON (Object v) = Embed <$> v .: "type"
                               <*> v .: "url"
  parseJSON v          =  typeMismatch "Embed" v

-- |Type of Activity Attachment
data AttachmentType = Photo |
                      PhotoAlbum | -- ^ A type that occurs in the wild but is not mentioned in the Google+ API docs
                      Video |
                      Article      -- ^ An article attachment specified by a link by the poster
                      deriving (Show, Eq)

instance FromJSON AttachmentType where
  parseJSON (String "photo")       = pure Photo
  parseJSON (String "photo-album") = pure PhotoAlbum
  parseJSON (String "video")       = pure Video
  parseJSON (String "article")     = pure Article
  parseJSON v                      = typeMismatch "AttachmentType" v

-- |Geolocation based on longitude and latitude
data Geocode = Geocode { latitude  :: Double,
                         longitude :: Double } deriving (Show, Eq)

instance FromJSON Geocode where
  parseJSON (String str) = pure $ Geocode long lat
    where (longT, latT) = spanSkip ' ' str
          long          = read . unpack $ longT
          lat           = read . unpack $ latT
  parseJSON v            =  typeMismatch "Geocode" v

-- |Describes who has access to a given Activity resource
data Access = Access { accessDescription :: Maybe Text,  -- ^ Description of the access, suitable for display
                       accessItems       :: [AccessItem] -- ^ List of access entries
                     } deriving (Show, Eq)

instance FromJSON Access where
  parseJSON (Object v) = Access <$> v .:? "description"
                                <*> v .:  "items"
  parseJSON v          =  typeMismatch "Access" v

-- |AccessEntry that describes the type of access someone may have to an Activity
data AccessItem = AccessItem { accessItemId   :: Maybe ID,      -- ^ ID of the entry. Only set when this AccessItem refers to a Person or Circle
                               accessItemType :: AccessItemType -- ^ Type of entity which has access to the associated Activity
                             } deriving (Show, Eq)

instance FromJSON AccessItem where
  parseJSON (Object v) = AccessItem <$> v .:? "id"
                                    <*> v .:  "type"
  parseJSON v          =  typeMismatch "AccessItem" v

-- |Type of entity which may access an Activity
data AccessItemType = PersonAccess |
                      CircleAccess |
                      MyCirclesAccess |       -- ^ Access granted to all members of the Actor's circles
                      ExtendedCirclesAccess | -- ^ Access granted to members of the Actor's circles and their circles as well
                      PublicAccess            -- ^ Access to anyone on the internet
                      deriving (Show, Eq)

instance FromJSON AccessItemType where
  parseJSON (String "person")          = pure PersonAccess
  parseJSON (String "circle")          = pure CircleAccess
  parseJSON (String "myCircles")       = pure MyCirclesAccess
  parseJSON (String "extendedCircles") = pure ExtendedCirclesAccess
  parseJSON (String "public")          = pure PublicAccess
  parseJSON v                          = typeMismatch "AccessItemType" v

-- |Service provider who originally published an Activity
data Provider = Provider { providerTitle :: Text } deriving (Show, Eq)

instance FromJSON Provider where
  parseJSON (Object v) = Provider <$> v .: "title"
  parseJSON v          =  typeMismatch "Provider" v

---------- Person Types

-- |A member of Google+
data Person = Person { personId                 :: ID,                       -- ^ Id of the Person
                       personDisplayName        :: Text,                     -- ^ Name of the Person, suitable for display
                       personName               :: Maybe PersonName,         -- ^ Person's actual, full name
                       personNickName           :: Maybe Text,               -- ^ Optional nickname of the Person
                       personTagline            :: Maybe Text,               -- ^ Brief description of the Person
                       personBirthday           :: Maybe Day,                -- ^ Person's Birthday
                       personGender             :: Maybe Gender,             -- ^ Person's gender
                       personAboutMe            :: Maybe Text,               -- ^ About Me profile section
                       personCurrentLocation    :: Maybe Text,               -- ^ Current location of the Person
                       personRelationshipStatus :: Maybe RelationshipStatus, -- ^ Person's current relationship status
                       personProfileURL         :: URL,                      -- ^ URL to the person's profile
                       personImage              :: Image,                    -- ^ Profile image for the Person
                       personEmails             :: [Email],                  -- ^ Email addresses that the person uses
                       personURLs               :: [PersonURL],              -- ^ External URLs on the Person's profile
                       personOrganizations      :: [Organization],           -- ^ Organizations that the Person has belonged to, past and present
                       personPlacesLived        :: [Place],                  -- ^ Places in which the Person has lived
                       personLanguagesSpoken    :: [Language],               -- ^ Languages the Person speaks
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

-- |A Person search result with limited informaiton. The full person's profile must be retrieved to get the rest
data PersonSearchResult = PersonSearchResult { personSRId          :: ID,    -- ^ Id of the Person
                                               personSRDisplayName :: Text,  -- ^ Name of the Person, suitable for display
                                               personSRImage       :: Image, -- ^ Profile image for the Person
                                               personSRProfileURL  :: URL    -- ^ URL to the person's profile
                                             } deriving (Show, Eq)

instance FromJSON PersonSearchResult where
  parseJSON (Object v) = PersonSearchResult <$> v .:  "id"
                                            <*> v .:  "displayName"
                                            <*> v .:  "image"
                                            <*> v .:  "url"
  parseJSON v          = typeMismatch "PersonSearchResult" v

instance FromJSON Day where
  parseJSON (String str) = pure . read . unpack $ str
  parseJSON v            = typeMismatch "Day" v

-- |Person's gender
data Gender = Male |
              Female |
              OtherGender
              deriving (Show, Eq)

instance FromJSON Gender where
  parseJSON (String "male")   = pure Male
  parseJSON (String "female") = pure Female
  parseJSON (String "other")  = pure OtherGender
  parseJSON v                 = typeMismatch "Gender" v


-- |Identifier used for finding a Person
data PersonID = PersonID Text | -- ^ ID for a specific user
                Me              -- ^ The authenticated user
                deriving (Show, Eq)

-- |Full, real name of a Person
data PersonName = PersonName { familyName      :: Text,
                               formatted       :: Text, -- ^ Fully formatted name of a Person including middle names, suffixes, etc.
                               givenName       :: Text, -- ^ The given (first) name of a Person
                               honorificPrefix :: Text, -- ^ Prefix to a Person's name such as Dr. or Mrs.
                               honorificSuffix :: Text, -- ^ Suffix of a Person's name such as Jr.
                               middleName      :: Text
                             } deriving (Show, Eq)

instance FromJSON PersonName where
  parseJSON (Object v) = PersonName <$> v .: "familyName"
                                    <*> v .: "formatted"
                                    <*> v .: "givenName"
                                    <*> v .: "honorificPrefix"
                                    <*> v .: "honorificSuffix"
                                    <*> v .: "middleName"
  parseJSON v          = typeMismatch "PersonName" v

-- |Image resource on Google+
data Image = Image { imageURL :: URL,
                     imageType :: Maybe Text, -- ^ Media type of the link
                     imageWidth :: Maybe Integer,
                     imageHeight :: Maybe Integer
                   } deriving (Show, Eq)

instance FromJSON Image where
  parseJSON (Object v) = Image <$> v .:  "url"
                               <*> v .:? "type"
                               <*> v .:? "width"
                               <*> v .:? "height"
  parseJSON v          = typeMismatch "Image" v

-- |Email address belonging to the User
data Email = Email { emailPrimary      :: Bool,      -- ^ Whether or not the Email is the Person's primary Email
                     emailType         :: EmailType, -- ^ Type/context of the Email address
                     emailAddressValue :: Text       -- ^ The actual text address of the Email
                   } deriving (Show, Eq)

instance FromJSON Email where
  parseJSON (Object v) = Email <$> v .: "primary"
                               <*> v .: "type"
                               <*> v .: "value"
  parseJSON v          = typeMismatch "Email" v

-- |Context/types of Emails that a Person can have
data EmailType = HomeEmail |
                 WorkEmail |
                 OtherEmail deriving (Show, Eq)

instance FromJSON EmailType where
  parseJSON (String "home")  = pure HomeEmail
  parseJSON (String "work")  = pure WorkEmail
  parseJSON (String "other") = pure OtherEmail
  parseJSON v                = typeMismatch "EmailType" v

-- |External URLS that the Person has published
data PersonURL = PersonURL { personUrlPrimary :: Bool,                -- ^ Whether or not the URL is the Person's primary URl
                             personUrlType    :: Maybe PersonURLType, -- ^ Type of URL
                             personURLValue   :: URL                  -- ^ Actual text URl for the Person
                             } deriving (Show, Eq)

instance FromJSON PersonURL where
  parseJSON (Object v) = PersonURL <$> v .:| ("primary", False)
                                   <*> v .:? "type"
                                   <*> v .:  "value"
  parseJSON v          = typeMismatch "PersonURL" v

-- |Context/types of URLS that a Person can have
data PersonURLType = HomeURL |
                     WorkURL |
                     BlogURL |
                     ProfileURL |
                     JsonURL | -- ^ This is not a documented value, yet I've encountered it in the wild. I have no idea what it means.
                     OtherURL
                     deriving (Show, Eq)

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


-- |Organization to which a Persion currently or previously may belong
data Organization = Organization { organizationDepartment  :: Maybe Text,      -- ^ Department of an Organization in which the Person resided
                                   organizationDescription :: Maybe Text,      -- ^ General description of the Organization
                                   organizationEndDate     :: Maybe Text,      -- ^ Date on which the user stopped at the organization in an unspecified text format
                                   organizationLocation    :: Maybe Text,      -- ^ Location of the Organization
                                   organizationName        :: Text,            -- ^ Name of the Organization
                                   organizationPrimary     :: Bool,            -- ^ Whether or not this Organization was the Person's primary one
                                   organizationstartDate   :: Maybe Text,      -- ^ Date on which the user started at the organization in an unspecified text format
                                   organizationTitle       :: Text,            -- ^ The Person's role at the Organization
                                   organizationType        :: OrganizationType -- ^ The type of Organization
                                 } deriving (Show, Eq)

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

-- |The capacity in which the Perosn participated in an Organization
data OrganizationType = Work |
                        School
                        deriving (Show, Eq)

instance FromJSON OrganizationType where
  parseJSON (String "work")   = pure Work -- spec says job. fail.
  parseJSON (String "school") = pure School
  parseJSON v                = typeMismatch "OrganizationType" v

-- |A physical location where a Person resides/resided
data Place = Place { placePrimary :: Bool, -- ^ Whether or not this is/was the Person's primary residence
                     placeValue   :: Text  -- ^ Text description of the Place
                   } deriving (Show, Eq)

instance FromJSON Place where
  parseJSON (Object v) = Place <$> v .:| ("primary", False)
                               <*> v .: "value"
  parseJSON v          = typeMismatch "Place" v

type Language = Text

-- |Relationship status of a Person
data RelationshipStatus = Single |
                          InARelationship |
                          Engaged |
                          Married |
                          ItsComplicated |
                          OpenRelationship |
                          Widowed |
                          InDomesticPartnership |
                          InCivilUnion
                          deriving (Show, Eq)

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
