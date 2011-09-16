{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus.Types (Person(..)) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Aeson
import Data.Time.Calendar (Day(..))
import Data.Text (Text, unpack)
import Network.URL

data Person = Person { personId                 :: Text,
                       personDisplayName        :: Text,
                       personName               :: PersonName,
                       personNickName           :: Text,
                       personTagline            :: Text,
                       personBirthday           :: Day,
                       personGender             :: Gender,
                       personAboutMe            :: Text,
                       personCurrentLocation    :: Text,
                       personRelationshipStatus :: RelationshipStatus,
                       personURL                :: URL,
                       personImage              :: Image,
                       personEmails             :: [Email],
                       personURLs               :: [PersonURL],
                       personOrganizations      :: [Organization],
                       placesLived              :: [Place],
                       languagesSpoken          :: [Language],
                       hasApp                   :: Bool }

-- Maybe there will be more types in the future?
data Gender = Male | Female | OtherGender

instance FromJSON Gender where
  parseJSON (String "male")   = pure Male
  parseJSON (String "female") = pure Female
  parseJSON (String "other")  = pure OtherGender
  parseJSON _                 = mzero


data PersonID = PersonID Text | Me

data PersonName = PersonName { familyName      :: Text,
                               formatted       :: Text,
                               givenName       :: Text,
                               honorificPrefix :: Text,
                               honorificSuffix :: Text,
                               middleName      :: Text }

instance FromJSON PersonName where
  parseJSON (Object v) = PersonName <$> v .: "familyName"
                                    <*> v .: "formatted"
                                    <*> v .: "givenName"
                                    <*> v .: "honorificPrefix"
                                    <*> v .: "honorificSuffix"
                                    <*> v .: "middleName"
  parseJSON _          = mzero

data Image = Image { imageURL :: URL }

instance FromJSON Image where
  parseJSON = undefined --TODO

data Email = Email { emailPrimary      :: Bool,
                     emailType         :: EmailType,
                     emailAddressValue :: Text }

instance FromJSON Email where
  parseJSON (Object v) = Email <$> v .: "primary"
                               <*> v .: "type"
                               <*> v .: "value"
  parseJSON _          = mzero

data EmailType = HomeEmail | WorkEmail | OtherEmail

instance FromJSON EmailType where
  parseJSON (String "home")  = pure HomeEmail
  parseJSON (String "work")  = pure WorkEmail
  parseJSON (String "other") = pure OtherEmail
  parseJSON _                = mzero

data PersonURL = PersonURL { personUrlPrimary :: Bool,
                             personUrlType    :: PersonURLType,
                             personURLValue   :: URL }

instance FromJSON PersonURL where
  parseJSON (Object v) = PersonURL <$> v .: "primary"
                                   <*> v .: "type"
                                   <*> v .: "value"
  parseJSON _          = mzero

data PersonURLType = HomeURL | WorkURL | BlogURL | ProfileURL | OtherURL

instance FromJSON PersonURLType where
  parseJSON (String "home")    = pure HomeURL
  parseJSON (String "work")    = pure WorkURL
  parseJSON (String "blog")    = pure BlogURL
  parseJSON (String "profile") = pure ProfileURL
  parseJSON (String "other")   = pure OtherURL
  parseJSON _                  = mzero

instance FromJSON URL where
  parseJSON (String v) = maybe mzero pure parsed
    where parsed = importURL . unpack $ v
  parseJSON _          = mzero


data Organization = Organization { organizationDepartment  :: Text,
                                   organizationDescription :: Text,
                                   organizationEndDate     :: Text, -- What format is this going to be in?
                                   organizationLocation    :: Text,
                                   organizationName        :: Text,
                                   organizationPrimary     :: Bool,
                                   organizationstartDate   :: Text, -- same here
                                   organizationTitle       :: Text,
                                   organizationType        :: OrganizationType }

instance FromJSON Organization where
  parseJSON (Object v) = Organization <$> v .: "department"
                                      <*> v .: "description"
                                      <*> v .: "endDate"
                                      <*> v .: "location"
                                      <*> v .: "name"
                                      <*> v .: "primary"
                                      <*> v .: "startDate"
                                      <*> v .: "title"
                                      <*> v .: "type"
  parseJSON _          = mzero

data OrganizationType = Job | School

instance FromJSON OrganizationType where
  parseJSON (String "job")     = pure Job
  parseJSON (String "school")  = pure School
  parseJSON _                = mzero

data Place = Place {}
data Language = Language {}

data RelationshipStatus = Single |
                          InARelationship |
                          Engaged |
                          Married |
                          ItsComplicated |
                          OpenRelationship |
                          Widowed |
                          InDomesticPartnership |
                          InCivilUnion

