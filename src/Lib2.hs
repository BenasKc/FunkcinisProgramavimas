{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    PatientInfo(..),
    Name(..),
    PatientId(..),
    FirstName(..),
    LastName(..),
    Age(..),
    Gender(..),
    Email(..),
    ContactInfo(..),
    Address(..),
    Appointment(..),
    DoctorName(..),
    Title(..),
    LastName(..),
    Age(..),
    Gender(..),
    Email(..),
    ContactInfo(..),
    Address(..),
    Appointment(..),
    DoctorName(..),
    Title(..),
    Department(..),
    Date(..),
    Time(..) -- these exports are required for proper testing
    ) where

import Data.Char (ord)
import Data.List (find)
import Text.Read (readEither)
import Data.Char (toLower)

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = RegisterQuery PatientInfo
           | BookAppointmentQuery Appointment
           | UpdateDetailsQuery PatientId UpdateInfo
           | SearchQuery SearchCriteria
           | ViewAllAppointmentsQuery
           deriving (Show, Eq) -- here comes the instances instead

-- | The instances are needed basically for tests
--instance Eq Query where
  --(==) _ _= False

--instance Show Query where
  --show _ = ""

-- | Parses user's input.
-- The function must have tests.

-- <command> ::= <register_patient> 
--             | <book_appointment> 
--             | <update_patient_details> 
--             | <search_patient>
--             | <view_all_appointments>
parseQuery :: String -> Either String Query
parseQuery input = case parseCommand input of
  Right (RegisterPatient pInfo) -> Right (RegisterQuery pInfo)
  Right (BookAppointment app) -> Right (BookAppointmentQuery app)
  Right (UpdatePatientDetails pid updateInfo) -> Right (UpdateDetailsQuery pid updateInfo)
  Right (SearchPatient criteria) -> Right (SearchQuery criteria)
  Right (ViewAllAppointments) -> Right (ViewAllAppointmentsQuery)
  Left errMsg -> Left errMsg

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
type State = HospitalState

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = initialHospitalState

-- | Updates a state according to a query.
-- This allows program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition hs (RegisterQuery pInfo) =
  let updatedState = addPatientInfo hs pInfo
  in Right (Just "Patient registered successfully.", updatedState)

stateTransition hs (BookAppointmentQuery app) =
  let updatedState = addAppointment hs app
  in Right (Just "Appointment booked successfully.", updatedState)

stateTransition hs (UpdateDetailsQuery pid updateInfo) = 
  case updatePatientDetails hs pid updateInfo of
    Right updatedState -> Right (Just "Patient details updated successfully.", updatedState)
    Left errMsg -> Left errMsg

stateTransition hs (SearchQuery criteria) =
  case searchPatient hs criteria of
    Right result -> Right (Just $ "Search result: " ++ show result, hs)
    Left errMsg -> Left errMsg

stateTransition hs ViewAllAppointmentsQuery =
  Right (Just $ "All appointments:\n" ++ showAppointments (getAllAppointments hs), hs)

------------------------------------------------------------------------------------------------

newtype EmptyChar = EmptyChar Char
  deriving (Show, Eq)

data CountryCode = SingleDigitCC Digit | ManyDigitCC Digit CountryCode
  deriving (Show, Eq)

data FieldName = NameField | AgeField | GenderField | ContactField | AddressField
  deriving (Show, Eq)

data Time = Time Int Int
  deriving (Show, Eq)

data Date = Date Int Int Int
  deriving (Show, Eq)

data Department
  = Cardiology
  | Neurology
  | Orthopedics
  | Pediatrics
  | Critical_Care
  | General_Surgery
  | Infectious_Diseases
  | Oncology
  deriving (Show, Eq)

data Title = Dr | Prof
  deriving (Show, Eq)

-- <doctor_name> ::= <title> " " <first_name> " " <last_name>
data DoctorName = DoctorName Title String String
  deriving (Show, Eq)

data Address = Address String
  deriving (Show, Eq)

data Domain = Domain String
  deriving (Show, Eq)

data Username = Username String
  deriving (Show, Eq)

data Email = Email String String
  deriving (Show, Eq)

data PhoneNumber = PhoneNumber CountryCode String
  deriving (Show, Eq)

data ContactInfo = PhoneNumberContact PhoneNumber | EmailContact Email | Contacts String
  deriving (Show, Eq)

data Gender = Male | Female | Other | Gender
  deriving (Show, Eq)

data Age = Age Int
  deriving (Show, Eq, Ord)

data LastName = LastName String
  deriving (Show, Eq)

data FirstName = FirstName String
  deriving (Show, Eq)

data Name = Name FirstName LastName
  deriving (Show, Eq)

data PatientId = PatientId Int
  deriving (Show, Eq)

data Appointment = Appointment
  { appointmentPatientId :: PatientId,
    appointmentDoctorName :: DoctorName,
    appointmentDepartment :: Department,
    appointmentDate :: Date,
    appointmentTime :: Time
  }
  deriving (Show, Eq)

-- <patient_info> ::= <patient_id> <name> <age> <gender> <address> <contact_info> 
data PatientInfo = PatientInfo
  { patientId :: PatientId,
    patientName :: Name,
    patientAge :: Age,
    patientGender :: Gender,
    patientContacts :: ContactInfo,
    patientAddress :: Address
  }
  deriving (Show, Eq)

data UpdateInfo = UpdateInfo FieldName String 
  deriving (Show, Eq)

data SearchCriteria = SearchCriteria FieldName String deriving (Show, Eq)

data Command
  = RegisterPatient PatientInfo
  | BookAppointment Appointment
  | UpdatePatientDetails PatientId UpdateInfo
  | SearchPatient SearchCriteria
  | ViewAllAppointments 
  deriving (Show, Eq)

data StringType
  = SingleLetterS Letter
  | SingleDigitS Digit
  | Space
  | Concat StringType StringType
  | Empty EmptyChar
  deriving (Show, Eq)

newtype Digit = Digit Char
  deriving (Show, Eq)

data System = System [Command]
  deriving (Show, Eq)

data HospitalState = HospitalState
  { appointments :: [Appointment],
    patientInfos :: [PatientInfo]
  }
  deriving (Show)

data Criteria
  = AgeAbove Int
  | AgeBelow Int
  | AgeInRange Int Int
  | GenderIs Gender
  deriving (Show, Eq)

parseNWords :: String -> Int -> ([String], String)
parseNWords str n = go str [] 0
  where
    go [] acc _ = (reverse acc, [])
    go s acc count
      | count >= n = (reverse acc, s) 
      | otherwise =
          let (word, rest) = span (/= ' ') s  -- Extract a word until a space
              trimmedRest = dropWhile (== ' ') rest  -- Skip spaces after the word
          in if null word
               then go trimmedRest acc count  -- Continue if no word found
               else go trimmedRest (word : acc) (count + 1)  -- Add word to accumulator

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

makedgt :: Char -> Either String Digit
makedgt c
  | c >= '0' && c <= '9' = Right (Digit c)
  | otherwise = Left "Not a digit"

dgt2char :: Digit -> Char
dgt2char (Digit c) = c

newtype Letter = Letter Char
  deriving (Show, Eq)

makeltr :: Char -> Either String Letter
makeltr c
  | isLetter c = Right (Letter c)
  | otherwise = Left "Not a letter"

ltr2chr :: Letter -> Char
ltr2chr (Letter c) = c

dgt2int :: Digit -> Int
dgt2int (Digit d) = ord d - ord '0'

-- We enforce restrictions on Digits and Letters using Smart Constructors

data Integer_
  = SingleDigit Digit
  | MoreDigits Digit Integer_
  deriving (Show, Eq)

-- SingleDigit and MoreDigits are constructors (essentially)

int_2int :: Integer_ -> Int
int_2int (SingleDigit d) = dgt2int d
int_2int (MoreDigits d rest) = (dgt2int d) * 10 ^ integer_Length rest + int_2int rest

integer_Length :: Integer_ -> Int
integer_Length (SingleDigit _) = 1
integer_Length (MoreDigits _ rest) = 1 + integer_Length rest

makeInteger_ :: [Char] -> Either String Integer_
makeInteger_ [] = Left "Empty input"
makeInteger_ [d] =
  case makedgt d of
    Right digit -> Right (SingleDigit digit)
    Left msg -> Left msg
makeInteger_ (d : ds) =
  case makedgt d of
    Right digit ->
      case makeInteger_ ds of
        Right rest -> Right (MoreDigits digit rest)
        Left msg -> Left msg
    Left msg -> Left msg

printInteger_ :: Either String Integer_ -> IO ()
printInteger_ (Right intVal) =
  let result = int_2int intVal
   in print result
printInteger_ (Left msg) = putStrLn msg

makeStringType :: String -> Either String StringType
makeStringType [] = Left "Empty input"
makeStringType (x : xs) =
  case (makeltr x, makedgt x) of
    (Right l, _) ->
      case makeStringType xs of
        Right rest -> Right (Concat (SingleLetterS l) rest)
        Left msg -> Right (SingleLetterS l)
    (_, Right d) ->
      case makeStringType xs of
        Right rest -> Right (Concat (SingleDigitS d) rest)
        Left msg -> Right (SingleDigitS d)
    (_, _) ->
      case makeStringType xs of
        Right rest -> Right (Concat Space rest)
        Left msg -> Right Space

stringType2str :: Either String StringType -> String
stringType2str (Left _) = ""
stringType2str (Right (SingleLetterS (Letter c))) = [c]
stringType2str (Right (SingleDigitS (Digit d))) = [d]
stringType2str (Right Space) = " "
stringType2str (Right (Concat s1 s2)) = stringType2str (Right s1) ++ stringType2str (Right s2)

isDigit :: Char -> Bool
isDigit c
  | c >= '0' && c <= '9' = True
  | otherwise = False

isAllNumOrPipe :: String -> Bool
isAllNumOrPipe [] = True
isAllNumOrPipe (x : xs) =
  (x == '|' || (x >= '0' && x <= '9')) && isAllNumOrPipe xs

isAllAlpha :: String -> Bool
isAllAlpha [] = True
isAllAlpha (x : xs) = ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')) && isAllAlpha xs

isAlphaNum :: Char -> Bool
isAlphaNum c
  | c >= 'A' && c <= 'Z' = True
  | c >= 'a' && c <= 'z' = True
  | c >= '0' && c <= '9' = True
  | otherwise = False

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf suffix str = isPrefixOf (reverse suffix) (reverse str)

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = (x == y) && (isPrefixOf xs ys)

isLetter :: Char -> Bool
isLetter c
  | c >= 'A' && c <= 'Z' = True
  | c >= 'a' && c <= 'z' = True
  | otherwise = False

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], []) 
partition p (x : xs) =
  let (yes, no) = partition p xs
   in if p x
        then (x : yes, no)
        else (yes, x : no)

createCountryCode :: String -> Either String CountryCode
createCountryCode [] = Left "No digits"
createCountryCode [d] =
  case makedgt d of
    Right digit -> Right (SingleDigitCC digit)
    Left errMsg -> Left errMsg
createCountryCode (d : ds) =
  case makedgt d of
    Right digit ->
      -- Handle valid digit case
      case createCountryCode ds of
        Left errMsg -> Left errMsg
        Right rest -> Right (ManyDigitCC digit rest)
    Left _ -> Left "Invalid digit"

cc2str :: Either String CountryCode -> Either String String
cc2str (Left errMsg) = Left errMsg 
cc2str (Right (SingleDigitCC c)) = Right [dgt2char c]
cc2str (Right (ManyDigitCC c rest)) =
  case cc2str (Right rest) of
    Right restStr -> Right ([dgt2char c] ++ restStr) 
    Left errMsg -> Left errMsg 

printCountryCode :: Either String CountryCode -> IO ()
printCountryCode cc =
  case cc2str cc of
    Right str -> putStrLn $ "Country Code: " ++ str
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

str2field :: String -> Either String FieldName
str2field "Name" = Right NameField
str2field "Age" = Right AgeField
str2field "Gender" = Right GenderField
str2field "Contact" = Right ContactField
str2field "Address" = Right AddressField
str2field unknown = Left $ "Invalid field: " ++ unknown

printFieldName :: Either String FieldName -> IO ()
printFieldName (Right fieldName) = putStrLn $ "Field: " ++ show fieldName
printFieldName (Left errMsg) = putStrLn $ "Error: " ++ errMsg


parseFieldName :: String -> Either String (FieldName, String)
parseFieldName input =
  let (fieldStr, remaining) = parseNWords input 1
  in case str2field (fieldStr !! 0) of
       Right fieldName -> Right (fieldName, remaining)
       Left errMsg -> Left errMsg
       
-- <time> ::= <Int> <Int>
parseTime :: String -> Either String (Time, String)
parseTime str =
  let trimmedStr = dropWhile (== ' ') str  -- Trim leading spaces
      (timePart, remaining) = parseNWords trimmedStr 1  -- Extract the first word (time)
  in if length (timePart !! 0) == 5
        && (timePart !! 0) !! 2 == ':'
        && all isDigit (take 2 (timePart !! 0))
        && all isDigit (drop 3 (timePart !! 0))
     then
        let hours = read (take 2 (timePart !! 0)) :: Int
            minutes = read (drop 3 (timePart !! 0)) :: Int
        in if isValidTime hours minutes
              then Right (Time hours minutes, remaining)  -- Return Time and remaining string
              else Left "Invalid time: hours must be 0-23 and minutes must be 0-59"
     else Left "Invalid format: expected HH:MM"

isValidTime :: Int -> Int -> Bool
isValidTime h m = h >= 0 && h <= 23 && m >= 0 && m < 60

formatTime :: Int -> Int -> String
formatTime hours minutes =
  let formattedHours = if hours < 10 then '0' : show hours else show hours
      formattedMinutes = if minutes < 10 then '0' : show minutes else show minutes
   in formattedHours ++ ":" ++ formattedMinutes

printTime :: Either String Time -> IO ()
printTime (Right (Time hours minutes)) =
  putStrLn $ "Time: " ++ formatTime hours minutes
printTime (Left errMsg) =
  putStrLn $ "Error: " ++ errMsg

isValidDate :: Int -> Int -> Int -> Bool
isValidDate day month year =
  day >= 1 && day <= daysInMonth month year && month >= 1 && month <= 12

daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 && isLeapYear year = 29
  | month == 2 = 28
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31

isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- <date> ::= <Int> <Int> <Int>
parseDate :: String -> Either String (Date, String)
parseDate str =
  let trimmedStr = dropWhile (== ' ') str  -- Trim leading spaces
      (datePart, remaining) = parseNWords trimmedStr 1  -- Extract the first word (date)
  in if length (datePart !! 0) == 10
        && (datePart !! 0) !! 2 == '-'
        && (datePart !! 0) !! 5 == '-'
        && all isDigit (take 2 (datePart !! 0))
        && all isDigit (take 2 (drop 3 (datePart !! 0)))
        && all isDigit (drop 6 (datePart !! 0))
     then
        let day = read (take 2 (datePart !! 0)) :: Int
            month = read (take 2 (drop 3 (datePart !! 0))) :: Int
            year = read (drop 6 (datePart !! 0)) :: Int
        in if isValidDate day month year
              then Right (Date day month year, remaining)  -- Return Date and remaining string
              else Left "Invalid date: day, month, or year out of range"
     else Left "Invalid format: expected DD-MM-YYYY"

printDate :: Either String Date -> IO ()
printDate (Right (Date day month year)) =
  putStrLn $ "Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year
printDate (Left errMsg) =
  putStrLn $ "Error: " ++ errMsg

deptToStr :: Department -> String
deptToStr Cardiology = "Cardiology"
deptToStr Neurology = "Neurology"
deptToStr Orthopedics = "Orthopedics"
deptToStr Pediatrics = "Pediatrics"
deptToStr Critical_Care = "Critical Care"
deptToStr General_Surgery = "General Surgery"
deptToStr Infectious_Diseases = "Infectious Diseases"
deptToStr Oncology = "Oncology"

strToDept :: String -> Either String (Department, String)
strToDept input =
  let trimmedInput = dropWhile (== ' ') input  -- Trim leading spaces
      (deptPart, remaining) = parseNWords trimmedInput 1  -- Extract the first word (department)
  in case (deptPart !! 0) of
       "Cardiology"          -> Right (Cardiology, remaining)
       "Neurology"           -> Right (Neurology, remaining)
       "Orthopedics"        -> Right (Orthopedics, remaining)
       "Pediatrics"         -> Right (Pediatrics, remaining)
       "Critical Care"      -> Right (Critical_Care, remaining)
       "General Surgery"     -> Right (General_Surgery, remaining)
       "Infectious Diseases" -> Right (Infectious_Diseases, remaining)
       "Oncology"           -> Right (Oncology, remaining)
       _                     -> Left $ "Invalid department: " ++ (deptPart !! 0)

printDepartment :: Either String Department -> IO ()
printDepartment (Right dept) = putStrLn $ "Department: " ++ show dept
printDepartment (Left errMsg) = putStrLn $ "Error: " ++ errMsg

title2str :: Title -> String
title2str Dr = "Dr"
title2str Prof = "Prof"

str2title :: String -> Either String Title
str2title "Dr" = Right Dr
str2title "Prof" = Right Prof
str2title unknown = Left $ "Invalid title: " ++ unknown

parseTitle :: String -> Either String (Title, String)
parseTitle input =
  let trimmedInput = dropWhile (== ' ') input
      (titleStr, remaining) = span (/= ' ') trimmedInput
  in case str2title titleStr of
       Right title -> Right (title, dropWhile (== ' ') remaining)
       Left errMsg -> Left errMsg 

printTitle :: Either String Title -> IO ()
printTitle (Right title) = putStrLn $ "Title: " ++ title2str title
printTitle (Left errMsg) = putStrLn $ "Error: " ++ errMsg

makeDoctorName :: Either String Title -> String -> String -> Either String DoctorName
makeDoctorName (Right title) name surname
  | not (null name) && not (null surname) = Right (DoctorName title name surname)
  | null name = Left "Name cannot be empty."
  | null surname = Left "Surname cannot be empty."
makeDoctorName (Left err) _ _ = Left err

doctorName2str :: Either String DoctorName -> String
doctorName2str (Right (DoctorName title name surname)) =
  title2str title ++ " " ++ name ++ " " ++ surname
doctorName2str (Left errMsg) = "Error: " ++ errMsg

printDoctorName :: Either String DoctorName -> IO ()
printDoctorName result = putStrLn (doctorName2str result)


makeAddress :: String -> Either String (Address, String)
makeAddress addr = 
  let (addressWords, remaining) = parseNWords addr 2
  in if length addressWords == 2
       then Right (Address (unwords addressWords), remaining)
       else Left "Invalid address: must contain exactly 2 words"

addr2str :: Address -> String
addr2str (Address str) = str

validDomains :: [String]
validDomains = [".com", ".org", ".net", ".edu", ".med"]

isValidDomain :: String -> [String] -> Bool
isValidDomain str (v : valids) =
  case break (== '.') str of
    (_, domain) ->
      if v == domain then True else isValidDomain domain valids
isValidDomain _ _ = False

str2domain :: String -> Either String Domain
str2domain domain =
  if isValidDomain domain validDomains
    then Right (Domain domain)
    else Left "Invalid domain"

domain2str :: Either String Domain -> String
domain2str (Right (Domain domain)) = "Domain: " ++ domain
domain2str (Left errMsg) = "Error: " ++ errMsg

printDomain :: Either String Domain -> IO ()
printDomain result = putStrLn (domain2str result)

makeUsername :: String -> Either String Username
makeUsername usern
  | length usern > 3 = Right (Username usern)
  | otherwise = Left "Username must be longer than 3 characters"

username2str :: Either String Username -> String
username2str (Right (Username usern)) = usern
username2str (Left errMsg) = "Error: " ++ errMsg

printUsername :: Either String Username -> IO ()
printUsername result = putStrLn (username2str result)

makeEmail :: String -> String -> Either String Email
makeEmail usern domain
  | isValidDomain domain validDomains && length usern > 3 = Right (Email usern domain)
  | otherwise = Left "Invalid email format"

email2str :: Either String Email -> String
email2str (Right (Email usern domain)) = usern ++ "@" ++ domain
email2str (Left errMsg) = "Error: " ++ errMsg

printEmail :: Either String Email -> IO ()
printEmail result = putStrLn (email2str result)

makePhoneNumber :: Either String CountryCode -> String -> Either String PhoneNumber
makePhoneNumber (Right cc) num
  | length num > 7 && length num < 14 && isAllNumOrPipe num = Right (PhoneNumber cc num)
  | otherwise = Left "Invalid phone number format"
makePhoneNumber (Left errMsg) _ = Left errMsg

pn2str :: Either String PhoneNumber -> String
pn2str (Right (PhoneNumber cc num)) =
  case cc2str (Right cc) of
    Right ccStr -> "+" ++ ccStr ++ num 
    Left errMsg -> "Error: " ++ errMsg 
pn2str (Left errMsg) = "Error: " ++ errMsg 

printPhoneNumber :: Either String PhoneNumber -> IO ()
printPhoneNumber result = putStrLn (pn2str result)

ci2str :: Either String ContactInfo -> String
ci2str (Right (PhoneNumberContact pn)) = pn2str (Right pn)
ci2str (Right (EmailContact e)) = email2str (Right e)
ci2str (Left errMsg) = "Error: " ++ errMsg

makeContactInfo :: String -> Either String ContactInfo
makeContactInfo str =
  case parseEmail str of
    Right (usern, domain) ->
      case makeEmail usern domain of
        Right email -> Right (EmailContact email)
        Left errMsg -> Left errMsg
    Left _ ->
      case parsePhoneNumber str of
        Right pn -> Right (PhoneNumberContact pn)
        Left errMsg -> Left errMsg

-- <email_address> ::= <username> "@" <domain>
parseEmail :: String -> Either String (String, String)
parseEmail str =
  case break (== '@') str of
    (username, '@' : domain)
      | not (null username) && not (null domain) -> Right (username, domain)
      | otherwise -> Left "Invalid email format: Username or domain is empty" 
    _ -> Left "Invalid email format: Missing '@' symbol"

-- <phone_number> ::= <country_code> <string>
parsePhoneNumber :: String -> Either String PhoneNumber
parsePhoneNumber str =
  case break (== '|') str of
    (cc, '|':ln)  -- Split into country code and local number
      | all isDigit cc && all isDigit ln && length cc <= 3 && length ln >= 7 && length ln <= 14 ->
          case createCountryCode cc of
            Right countryCode -> Right (PhoneNumber countryCode ln)
            Left errMsg -> Left errMsg
      | not (all isDigit cc) -> Left "Invalid country code: must be numeric"
      | not (all isDigit ln) -> Left "Invalid local number: must be numeric"
    _ -> Left "Invalid phone number format: expected format is '<country code>|<local number>', local number min 7 max 14"

printContactInfo :: Either String ContactInfo -> IO ()
printContactInfo contactInfo =
  putStrLn $ ci2str contactInfo

makeGender :: String -> Either String (Gender, String)
makeGender input =
  let trimmedInput = trim input
      (genderStr, remaining) = parseNWords trimmedInput 1 
  in case map toLower (genderStr !! 0) of
       "male"   -> Right (Male, remaining)
       "female" -> Right (Female, remaining)
       "other"  -> Right (Other, remaining)
       _        -> Left "Invalid gender"

gender2str :: Either String Gender -> String
gender2str (Right Male) = "Male"
gender2str (Right Female) = "Female"
gender2str (Right Other) = "Other"
gender2str (Left errMsg) = "Error: " ++ errMsg

printGender :: Either String Gender -> IO ()
printGender gender = putStrLn $ gender2str gender

makeAge :: Int -> Either String Age
makeAge age
  | age >= 0 = Right (Age age)
  | otherwise = Left "Invalid age"

parseAge :: String -> Either String (Age, String)
parseAge input =
  let trimmedInput = trim input
      (ageStr, remaining) = parseNWords trimmedInput 1 
  in if all isDigit (ageStr !! 0)  
      then 
          let age = read (ageStr !! 0) :: Int 
          in case makeAge age of 
              Right validAge -> Right (validAge, remaining) 
              Left errMsg -> Left errMsg
      else Left "Invalid age: must be numeric"

age2int :: Either String Age -> Int
age2int (Right (Age age)) = age
age2int (Left _) = -1 

printAge :: Either String Age -> IO ()
printAge age =
  case age of
    Right (Age a) -> putStrLn $ "Age: " ++ show a
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

makeLastName :: String -> Either String LastName
makeLastName lname
  | isAllAlpha lname && length lname > 0 = Right (LastName lname)
  | otherwise = Left "Invalid last name: must be non-empty and contain only alphabetic characters"

lname2str :: Either String LastName -> String
lname2str (Right (LastName lname)) = lname
lname2str (Left errMsg) = "Error: " ++ errMsg

makeFirstName :: String -> Either String FirstName
makeFirstName fname
  | isAllAlpha fname && length fname > 0 = Right (FirstName fname)
  | otherwise = Left "Invalid first name: must be non-empty and contain only alphabetic characters"

fname2str :: Either String FirstName -> String
fname2str (Right (FirstName fname)) = fname
fname2str (Left errMsg) = "Error: " ++ errMsg

makeName :: String -> Either String (Name, String)
makeName input =
  let (parts, remaining) = parseNWords input 2  -- Extract the first two words and the remaining string
  in case parts of
       [firstName, lastName] ->  -- Check if we got exactly two parts
         case makeFirstName firstName of
           Right fnm ->
             case makeLastName lastName of
               Right lnm -> Right (Name fnm lnm, remaining)  -- Return Name object and remaining string
               Left errMsg -> Left errMsg  -- Handle error from makeLastName
           Left errMsg -> Left errMsg  -- Handle error from makeFirstName
       _ -> Left "Only first name and last name must be provided"

name2str :: Either String Name -> String
name2str (Right (Name f l)) = (fname2str (Right f)) ++ " " ++ (lname2str (Right l))
name2str (Left errMsg) = "Error: " ++ errMsg

printName :: Either String Name -> IO ()
printName name =
  case name of
    Right n -> putStrLn $ "Name: " ++ name2str (Right n)
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

makePatientId :: String -> Either String (PatientId, String)
makePatientId input =
  let trimmedInput = trim input
      (idStr, remaining) = parseNWords trimmedInput 1 
  in if all isDigit (idStr !! 0)  
      then
          let id = read (idStr !! 0) :: Int
          in if id >= 0 
             then Right (PatientId id, remaining)  
             else Left "Invalid Patient ID: must be a non-negative integer"
      else Left "Invalid Patient ID: must be numeric"

-- <contact_info> ::= <phone_number> | <email_address>

parseContactInfo :: String -> Either String (ContactInfo, String)
parseContactInfo contactInfo = 
  if null contactInfo
  then Left "Invalid Contact Info"
  else
    let (contactWords, remaining) = parseNWords contactInfo 1
    in case makeContactInfo (unwords contactWords) of
         Right validContact -> Right (validContact, remaining)
         Left errMsg -> Left errMsg


-- <patient_id> ::= <integer>
parsePatientId :: String -> Either String PatientId
parsePatientId str =
  case reads str of
    [(x, "")] ->
      case makePatientId x of
        Right patientId -> Right (fst patientId)
        Left errMsg -> Left "Invalid ID"
    _ -> Left "Incorrect patient ID"

-- <appointment_info> ::= <patient_id> <doctor_name> <department> <date> <time>
parseAppointment :: String -> Either String Appointment
parseAppointment str =
  case verifyBookAppointment str of
    Right remainingStr ->
      case makePatientId remainingStr of
        Right (pid, afterPid) ->
          case parseTitle afterPid of
            Right (title, afterTitle) ->
              case parseNWords afterTitle 2 of
                ([firstName, lastName], afterName) ->
                  case strToDept afterName of
                    Right (dept, afterDept) ->
                      case parseDate afterDept of
                        Right (date, afterDate) ->
                          case parseTime afterDate of
                            Right (time, _) ->
                              case makeDoctorName (Right title) firstName lastName of
                                Right doctorName -> Right (Appointment pid doctorName dept date time)
                                Left errMsg -> Left ("Error creating DoctorName: " ++ errMsg)
                            Left errMsg -> Left ("Error parsing Time: " ++ errMsg)
                        Left errMsg -> Left ("Error parsing Date: " ++ errMsg)
                    Left errMsg -> Left ("Error parsing Department: " ++ errMsg)
            Left errMsg -> Left ("Error parsing Doctor title: " ++ errMsg)
        Left errMsg -> Left ("Error parsing Patient ID: " ++ errMsg)
    Left errMsg -> Left ("Invalid command: " ++ errMsg)

-- <search_criteria> ::= <field_name> "=" <value>
parseSearchCriteria :: String -> Either String SearchCriteria
parseSearchCriteria str =
  case verifySearchPatient str of
    Right remainingStr -> 
      case parseFieldName remainingStr of
        Right (fieldName, restAfterField) ->
          let (equalsSign, restAfterEquals) = parseNWords restAfterField 1
          in if (equalsSign !! 0) == "="
             then Right (SearchCriteria fieldName (trim restAfterEquals))
             else Left "Incorrect syntax: expected '=' after field name"
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg

-- <update_patient_details> ::= "UPDATE PATIENT" <patient_id> <update_info>
parseUpdatePatientDetails :: String -> Either String (PatientId, UpdateInfo)
parseUpdatePatientDetails str =
  case verifyUpdatePatient str of
    Right remainingStr -> 
      case makePatientId remainingStr of
        Right (patientId, restAfterId) ->
          case parseFieldName restAfterId of
            Right (fieldName, restAfterField) ->
              let fieldValueStr = trim restAfterField  -- Remaining string is the new field value
              in Right (patientId, UpdateInfo fieldName fieldValueStr)
            Left errMsg -> Left ("Invalid field name: " ++ errMsg)
        Left errMsg -> Left ("Error parsing patient ID: " ++ errMsg)
    Left errMsg -> Left errMsg

-- <age> ::= <integer>



-- <register_patient> ::= "REGISTER PATIENT" <patient_info>
parseRegisterPatient :: String -> Either String PatientInfo
parseRegisterPatient str =
  case verifyRegisterPatient str of
    Right remainingStr -> 
      case makePatientId remainingStr of
        Right (pid, restAfterId) ->
          case makeName restAfterId of
            Right (name, restAfterName) ->  
              case parseAge restAfterName of
                Right (age, restAfterAge) ->  
                  case makeGender restAfterAge of
                    Right (gender, restAfterGender) -> 
                      case makeAddress restAfterGender of
                        Right (addr, restAfterAddr) -> 
                          case parseContactInfo restAfterAddr of
                            Right (contInfo, _) -> 
                              Right (PatientInfo pid name age gender contInfo addr)
                            Left errMsg -> Left ("Invalid contact info: " ++ errMsg)
                        Left errMsg -> Left ("Invalid address info: " ++ errMsg)
                    Left errMsg -> Left ("Invalid gender: " ++ errMsg)
                Left errMsg -> Left ("Invalid age: " ++ errMsg)
            Left errMsg -> Left ("Error in name: " ++ errMsg)
        Left errMsg -> Left ("Error reading the ID: " ++ errMsg)
    Left errMsg -> Left ("Unknown command:asd " ++ errMsg)

-- <command> ::= <register_patient> 
--             | <book_appointment> 
--             | <update_patient_details> 
--             | <search_patient>
--             | <view_all_appointments>
parseCommand :: String -> Either String Command
parseCommand str =
  case parseRegisterPatient str of
    Right regDetails -> Right (RegisterPatient regDetails)
    Left regErr -> case parseAppointment str of
      Right appDetails -> Right (BookAppointment appDetails)
      Left appErr -> case parseUpdatePatientDetails str of
        Right (id, details) -> Right (UpdatePatientDetails id details)
        Left updateErr -> case parseSearchCriteria str of
          Right searchCriteria -> Right (SearchPatient searchCriteria)
          Left searchErr -> case str of
            "VIEW ALL APPOINTMENTS" -> Right ViewAllAppointments
            _ -> Left $ "Unknown command. Errors encountered: "
                     ++ "\n- Register: " ++ regErr
                     ++ "\n- Appointment: " ++ appErr
                     ++ "\n- Update: " ++ updateErr
                     ++ "\n- Search: " ++ searchErr

-- <system> ::= <command> | <command> <system>
parseSystem :: String -> Either String System
parseSystem str =
  let commandStrings = lines str
   in fmap System (mapM parseCommand commandStrings)


initialHospitalState :: HospitalState
initialHospitalState = HospitalState [] []

getPatientInfo :: HospitalState -> PatientId -> Either String PatientInfo
getPatientInfo hs pid =
  case find (\pInfo -> patientId pInfo == pid) (patientInfos hs) of
    Just pInfo -> Right pInfo
    Nothing -> Left $ "Patient with ID " ++ show pid ++ " not found."

addAppointment :: HospitalState -> Appointment -> HospitalState
addAppointment hs app = hs { appointments = app : appointments hs }

addPatientInfo :: HospitalState -> PatientInfo -> HospitalState
addPatientInfo hs pInfo = hs { patientInfos = pInfo : patientInfos hs }

updatePatientDetails :: HospitalState -> PatientId -> UpdateInfo -> Either String HospitalState
updatePatientDetails hs pid updateInfo = 
  case getPatientInfo hs pid of
    Right pInfo -> 
      case applyUpdate pInfo updateInfo of
        Right updatedPInfo -> Right ( hs { patientInfos = updateList (\p -> patientId p == pid) (const updatedPInfo) (patientInfos hs) })
        Left err -> Left err
    Left err -> Left err

searchPatient :: HospitalState -> SearchCriteria -> Either String [PatientInfo]
searchPatient hs (SearchCriteria fieldName value) =
  Right $ filter (matchesCriteria fieldName value) (patientInfos hs)

applyUpdate :: PatientInfo -> UpdateInfo -> Either String PatientInfo
applyUpdate pInfo (UpdateInfo fieldName value) =
  case fieldName of
    NameField ->
      case makeName value of
        Right name -> Right $ pInfo { patientName = fst name }
        Left errMsg -> Left ("Invalid name: " ++ errMsg)
    AgeField ->
      case makeAge (read value) of
        Right age -> Right $ pInfo { patientAge = age }
        Left errMsg -> Left ("Invalid age, " ++ errMsg)
    GenderField ->
      case makeGender value of
        Right gender -> Right $ pInfo { patientGender = fst gender }
        Left errMsg -> Left ("Invalid gender " ++ errMsg)
    ContactField ->
      case parseContactInfo value of
        Right contactInfo -> Right $ pInfo { patientContacts = fst contactInfo }
        Left errMsg -> Left errMsg
    AddressField ->
      case makeAddress value of
        Right addr -> Right $ pInfo { patientAddress = fst addr }
        Left errMsg -> Left errMsg


updateList :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateList _ _ [] = []
updateList predicate updateFunc (x:xs)
  | predicate x = updateFunc x : xs 
  | otherwise = x : updateList predicate updateFunc xs


instance Read Age where
  readsPrec _ str = case reads str of
    [(age, rest)] -> [(Age age, rest)]
    _ -> []

matchesCriteria :: FieldName -> String -> PatientInfo -> Bool
matchesCriteria field value patient =
  case field of
    AgeField ->
      case reads value of
        [(age, "")] -> patientAge patient == age
        _ -> False

    GenderField ->
      case makeGender value of
        Right gender -> patientGender patient == fst gender
        Left _ -> False
    
    NameField ->
      case makeName value of
        Right nm -> patientName patient == fst nm
        Left _ -> False

    AddressField ->
      case makeAddress value of
        Right addr -> patientAddress patient == fst addr
        Left _ -> False

    ContactField ->
      case parseContactInfo value of
        Right contactInfo -> patientContacts patient == fst contactInfo
        Left _ -> False

    _ -> False
    
getAllAppointments :: HospitalState -> [Appointment]
getAllAppointments hs = appointments hs

showAppointments :: [Appointment] -> String
showAppointments apps = unlines (map show apps)

verifyUpdatePatient :: String -> Either String String
verifyUpdatePatient input =
  let (prefix, remaining) = parseNWords input 2
  in case prefix of
       ["UPDATE", "PATIENT"] -> Right remaining
       _ -> Left "Incorrect syntax. Must start with 'UPDATE PATIENT'."

verifySearchPatient :: String -> Either String String
verifySearchPatient input =
  let (prefix, remaining) = parseNWords input 2
  in case prefix of
       ["SEARCH", "PATIENT"] -> Right remaining
       _ -> Left "Incorrect syntax. Must start with 'SEARCH PATIENT'."

verifyBookAppointment :: String -> Either String String
verifyBookAppointment input =
  let (prefix, remaining) = parseNWords input 2
  in case prefix of
       ["BOOK", "APPOINTMENT"] -> Right remaining
       _ -> Left "Incorrect syntax. Must start with 'BOOK APPOINTMENT'."

verifyRegisterPatient :: String -> Either String String
verifyRegisterPatient input =
  let trimmedInput = trim input
      (firstTwoWords, remaining) = parseNWords trimmedInput 2  -- Extract the first two words
  in if (unwords firstTwoWords) == "REGISTER PATIENT"
      then Right remaining  -- Return the remaining string if valid
      else Left "Invalid command: expected 'REGISTER PATIENT'"