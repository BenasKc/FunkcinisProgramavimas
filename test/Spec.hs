{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

import Control.Monad (liftM, liftM2)

genPatientId :: Gen Lib2.PatientId
genPatientId = Lib2.PatientId <$> arbitrary `suchThat` (> 0)

genName :: Gen Lib2.Name
genName = Lib2.Name <$> (Lib2.FirstName <$> genNonEmptyString)
                    <*> (Lib2.LastName <$> genNonEmptyString)

genGender :: Gen Lib2.Gender
genGender = elements [Lib2.Male, Lib2.Female, Lib2.Other]

genAddress :: Gen Lib2.Address
genAddress = do
  word1 <- genNonEmptyString
  word2 <- genNonEmptyString
  return $ Lib2.Address (word1 ++ " " ++ word2)

genContactInfo :: Gen Lib2.ContactInfo
genContactInfo = oneof [ Lib2.PhoneNumberContact <$> genPhoneNumber,
                         Lib2.EmailContact <$> genEmail
                       ]

genEmail :: Gen Lib2.Email
genEmail = do
  -- Generate a valid username (at least 4 characters)
  usern <- genValidUsername
  
  -- Generate a valid domain (with a valid suffix)
  domain <- genValidDomain
  
  return $ Lib2.Email usern domain

-- Generate a valid username with length > 3
genValidUsername :: Gen String
genValidUsername = do
  len <- chooseInt (4, 20)  -- Username must be at least 4 characters long
  vectorOf len (elements allowedUsernameChars)
  where
    allowedUsernameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._-" -- Adjust if needed

-- Generate a valid domain
genValidDomain :: Gen String
genValidDomain = do
  -- Generate a domain name (before the suffix)
  len <- chooseInt (3, 10)  -- Length of the domain name
  domainName <- vectorOf len (elements allowedDomainChars)
  
  -- Append a valid domain suffix
  suffix <- elements validDomains
  
  return $ domainName ++ suffix
  where
    allowedDomainChars = ['a'..'z'] ++ ['0'..'9'] ++ "-"  -- Common domain name characters
    validDomains = [".com", ".org", ".net", ".edu", ".med"]

genPhoneNumber :: Gen Lib2.PhoneNumber
genPhoneNumber = do
  countryCode <- genCountryCode
  
  localNumber <- genLocalNumber
  
  return $ Lib2.PhoneNumber countryCode localNumber

genLocalNumber :: Gen String
genLocalNumber = do
  len <- chooseInt (8, 13) 
  vectorOf len (choose ('0', '9'))



genCountryCode :: Gen Lib2.CountryCode
genCountryCode = genCountryCodeWithLimit 2

genCountryCodeWithLimit :: Int -> Gen Lib2.CountryCode
genCountryCodeWithLimit 0 = Lib2.SingleDigitCC <$> genDigit
genCountryCodeWithLimit n = oneof
  [ Lib2.SingleDigitCC <$> genDigit 
  , Lib2.ManyDigitCC <$> genDigit <*> genCountryCodeWithLimit (n - 1) 
  ]

genDigit :: Gen Lib2.Digit
genDigit = Lib2.Digit <$> elements ['0'..'9']

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ elements ['a'..'z']

genDate :: Gen Lib2.Date
genDate = Lib2.Date <$> choose (1, 28) <*> choose (1, 12) <*> choose (1900, 2100)

genTime :: Gen Lib2.Time
genTime = Lib2.Time <$> choose (0, 23) <*> choose (0, 59)

genDepartment :: Gen Lib2.Department
genDepartment = elements [Lib2.Cardiology, Lib2.Neurology, Lib2.Orthopedics, Lib2.Pediatrics,
                          Lib2.Critical_Care, Lib2.General_Surgery, Lib2.Infectious_Diseases, Lib2.Oncology]

genRegisterQuery :: Gen Lib2.Query
genRegisterQuery = Lib2.RegisterQuery <$> genPatientInfo

genBookAppointmentQuery :: Gen Lib2.Query
genBookAppointmentQuery = Lib2.BookAppointmentQuery <$> genAppointment

genQuery :: Gen Lib2.Query
genQuery = oneof [genRegisterQuery, genBookAppointmentQuery]

genPatientInfo :: Gen Lib2.PatientInfo
genPatientInfo = Lib2.PatientInfo <$> genPatientId
                                  <*> genName
                                  <*> (Lib2.Age <$> choose (0, 120))
                                  <*> genGender
                                  <*> genContactInfo
                                  <*> genAddress

genAppointment :: Gen Lib2.Appointment
genAppointment = Lib2.Appointment <$> genPatientId
                                   <*> genDoctorName
                                   <*> genDepartment
                                   <*> genDate
                                   <*> genTime

genDoctorName :: Gen Lib2.DoctorName
genDoctorName = Lib2.DoctorName <$> elements [Lib2.Dr, Lib2.Prof]
                                <*> genNonEmptyString
                                <*> genNonEmptyString

genStatements :: Gen Lib3.Statements
genStatements = do
  queries <- listOf1 genQuery  
  case queries of
    [q] -> return $ Lib3.Single q        
    qs  -> return $ Lib3.Batch qs  

propertyRenderParse :: TestTree
propertyRenderParse = QC.testProperty "renderStatements . parseStatements == id" $
  forAll genStatements $ \statements ->
    let rendered = Lib3.renderStatements statements
        parsed = Lib3.parseStatements rendered
    in parsed == Right (statements, "")


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Some error message"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]