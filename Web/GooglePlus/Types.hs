{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus.Types (Person(..), PersonID(..)) where

import           Data.Aeson.Types (Parser, typeMismatch)
import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson
import qualified Data.Map as M
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..))
import           Data.Time.RFC3339 (readRFC3339)
import           Data.Text (Text, unpack, append)
import           Network.URL

data Activity = Activity { activityPlaceholder :: Bool,
                           activityTitle :: Text,
                           activityPublished :: ZonedTime,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           activityTitle :: Text,
                           
  }

data Person = Person { personId                 :: Text,
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

data Image = Image { imageURL :: URL } deriving (Show, Eq)

instance FromJSON Image where
  parseJSON (Object v) = Image <$> v .: "url"
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
  parseJSON (String v) = maybe (fail $ "Failed to parse URL " ++ unpack v) pure parsed
    where parsed = importURL . unpack $ v
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
