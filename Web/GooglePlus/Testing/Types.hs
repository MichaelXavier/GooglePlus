{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Web.GooglePlus.Testing.Types (specs) where

import           Data.Aeson (json,
                             fromJSON,
                             Result(..))
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.String.QQ (s)
import           Network.URL
import           Test.Hspec (Specs, describe, descriptions, it)
import           Test.Hspec.HUnit
import           Test.HUnit.Base ((~?=))

import Web.GooglePlus.Types

specs :: Specs
specs = descriptions [describe_parsePerson]

describe_parsePerson :: Specs
describe_parsePerson = describe "parse Person" [
  it "parses a full Person" (parseStr fullPersonJSON ~?= Right fullPerson)]
  where fullPerson = Person {
          personId                 = "112582860413717209806",
          personDisplayName        = "Michael Xavier",
          personName               = Nothing,
          personNickName           = Nothing,
          personTagline            = Just "CS Graduate. Loves Programming, Linux, Muffins. Writes Ruby for work, Haskell for fun.",
          personBirthday           = Nothing,
          personGender             = Just Male,
          personAboutMe            = Just "I like creating software and reading about code. I also like watching movies.",
          personCurrentLocation    = Nothing,
          personRelationshipStatus = Nothing,
          personProfileURL         = purl,
          personImage              = pimg,
          personEmails             = [],
          personURLs               = [],
          personOrganizations      = orgs,
          personPlacesLived        = [],
          personLanguagesSpoken    = [],
          personHasApp             = Nothing
        }
        purl = URL { 
          url_type = Absolute $ Host { protocol = HTTP True,
                                       host     = "plus.google.com",
                                       port     = Nothing},
          url_path = "112582860413717209806",
          url_params = []
        }
        pimg = Image {
          imageURL    = pimgurl,
          imageType   = Nothing,
          imageWidth  = Nothing,
          imageHeight = Nothing
        }
        pimgurl = URL { 
          url_type = Absolute $ Host { protocol = HTTP True,
                                       host     = "lh6.googleusercontent.com",
                                       port     = Nothing},
          url_path = "-c5Ii5djFXlw/AAAAAAAAAAI/AAAAAAAAAAA/PGcHK21Ds-k/photo.jpg",
          url_params = []
        }

        orgs = [uwb]
        uwb = Organization {
          organizationDepartment  = Nothing,
          organizationDescription = Nothing,
          organizationEndDate     = Nothing,
          organizationLocation    = Nothing,
          organizationName        = "University of Washington, Bothell",
          organizationPrimary     = False,
          organizationstartDate   = Nothing,
          organizationTitle       = "Computing and Software Systems",
          organizationType        = School
        }
        fullPersonJSON = [s|
{
   "kind":"plus#person",
   "id":"112582860413717209806",
   "displayName":"Michael Xavier",
   "tagline":"CS Graduate. Loves Programming, Linux, Muffins. Writes Ruby for work, Haskell for fun.",
   "gender":"male",
   "aboutMe":"I like creating software and reading about code. I also like watching movies.",
   "url":"https://plus.google.com/112582860413717209806",
   "image":{
      "url":"https://lh6.googleusercontent.com/-c5Ii5djFXlw/AAAAAAAAAAI/AAAAAAAAAAA/PGcHK21Ds-k/photo.jpg"
   },
   "urls":[],
   "organizations":[
      {
         "name":"University of Washington, Bothell",
         "title":"Computing and Software Systems",
         "type":"school"
      }
   ]
} |]
  

---- Helpers
parseStr str = fjson =<< parsed
  where fjson v = case fromJSON v of
                    Success a -> Right a
                    Error e   -> Left e
        parsed  = eitherResult $ parse json str
