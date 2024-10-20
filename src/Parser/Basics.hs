module Basics where

import Data.Char (ord)
import Data.List (find)
import Text.Read (readEither)
import Data.Either (isRight, fromRight, fromLeft, isLeft)

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
partition _ [] = ([], []) -- Base case: empty list
partition p (x : xs) =
  let (yes, no) = partition p xs -- Recursive call
   in if p x
        then (x : yes, no) -- If the predicate is true, include in the first list
        else (yes, x : no) -- If false, include in the second list

createCountryCode :: String -> Either String CountryCode
createCountryCode [] = Left "No digits" -- Handle empty input
createCountryCode [d] =
  case makedgt d of
    Right digit -> Right (SingleDigitCC digit) -- If there's only one digit, construct a SingleDigitCC
    Left errMsg -> Left errMsg -- Handle invalid digit case
createCountryCode (d : ds) =
  case makedgt d of
    Right digit ->
      -- Handle valid digit case
      case createCountryCode ds of
        Left errMsg -> Left errMsg -- Propagate error if any
        Right rest -> Right (ManyDigitCC digit rest) -- Construct CountryCode
    Left _ -> Left "Invalid digit" -- Handle invalid digit case

cc2str :: Either String CountryCode -> Either String String
cc2str (Left errMsg) = Left errMsg -- Propagate any error
cc2str (Right (SingleDigitCC c)) = Right [dgt2char c]
cc2str (Right (ManyDigitCC c rest)) =
  case cc2str (Right rest) of
    Right restStr -> Right ([dgt2char c] ++ restStr) -- Concatenate the current digit with the rest
    Left errMsg -> Left errMsg -- Propagate any errors from recursive call

printCountryCode :: Either String CountryCode -> IO ()
printCountryCode cc =
  case cc2str cc of
    Right str -> putStrLn $ "Country Code: " ++ str
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

-- Updated str2field function using Either
str2field :: String -> Either String FieldName
str2field "Name" = Right NameField
str2field "Age" = Right AgeField
str2field "Gender" = Right GenderField
str2field "Contact" = Right ContactField
str2field "Address" = Right AddressField
str2field unknown = Left $ "Invalid field: " ++ unknown

-- Function to print FieldName or an error message
printFieldName :: Either String FieldName -> IO ()
printFieldName (Right fieldName) = putStrLn $ "Field: " ++ show fieldName
printFieldName (Left errMsg) = putStrLn $ "Error: " ++ errMsg

parseTime :: String -> Either String Time
parseTime str
  | length str == 5
      && str !! 2 == ':'
      && all isDigit (take 2 str)
      && all isDigit (drop 3 str) =
      let hours = read (take 2 str) :: Int
          minutes = read (drop 3 str) :: Int
       in if isValidTime hours minutes
            then Right (Time hours minutes)
            else Left "Invalid time: hours must be 0-23 and minutes must be 0-59"
  | otherwise = Left "Invalid format: expected HH:MM"

isValidTime :: Int -> Int -> Bool
isValidTime h m = h >= 0 && h <= 23 && m >= 0 && m < 60

-- Function to convert hours and minutes to a formatted string without printf
formatTime :: Int -> Int -> String
formatTime hours minutes =
  let formattedHours = if hours < 10 then '0' : show hours else show hours
      formattedMinutes = if minutes < 10 then '0' : show minutes else show minutes
   in formattedHours ++ ":" ++ formattedMinutes

-- Function to print Time or an error message
printTime :: Either String Time -> IO ()
printTime (Right (Time hours minutes)) =
  putStrLn $ "Time: " ++ formatTime hours minutes
printTime (Left errMsg) =
  putStrLn $ "Error: " ++ errMsg

-- Function to check if the date is valid
isValidDate :: Int -> Int -> Int -> Bool
isValidDate day month year =
  day >= 1 && day <= daysInMonth month year && month >= 1 && month <= 12

-- Helper function to calculate the number of days in a given month
daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 && isLeapYear year = 29
  | month == 2 = 28
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31

-- Check if a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- Updated parseDate function using Either
parseDate :: String -> Either String Date
parseDate str
  | length str == 10
      && str !! 2 == '-'
      && str !! 5 == '-'
      && all isDigit (take 2 str)
      && all isDigit (take 2 (drop 3 str))
      && all isDigit (drop 6 str) =
      let day = read (take 2 str) :: Int
          month = read (take 2 (drop 3 str)) :: Int
          year = read (drop 6 str) :: Int
       in if isValidDate day month year
            then Right (Date day month year)
            else Left "Invalid date: day, month, or year out of range"
  | otherwise = Left "Invalid format: expected DD-MM-YYYY"

-- Function to print Date or an error message
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

-- Updated strToDept function using Either
strToDept :: String -> Either String Department
strToDept "Cardiology" = Right Cardiology
strToDept "Neurology" = Right Neurology
strToDept "Orthopedics" = Right Orthopedics
strToDept "Pediatrics" = Right Pediatrics
strToDept "Critical Care" = Right Critical_Care
strToDept "General Surgery" = Right General_Surgery
strToDept "Infectious Diseases" = Right Infectious_Diseases
strToDept "Oncology" = Right Oncology
strToDept unknown = Left $ "Invalid department: " ++ unknown

-- Function to print Department or an error message
printDepartment :: Either String Department -> IO ()
printDepartment (Right dept) = putStrLn $ "Department: " ++ show dept
printDepartment (Left errMsg) = putStrLn $ "Error: " ++ errMsg

-- Function to convert a Title to its String representation
title2str :: Title -> String
title2str Dr = "Dr"
title2str Prof = "Prof"

-- Function to convert a String to a Title, using Either for error handling
str2title :: String -> Either String Title
str2title "Dr" = Right Dr
str2title "Prof" = Right Prof
str2title unknown = Left $ "Invalid title: " ++ unknown

-- Function to print Title or an error message
printTitle :: Either String Title -> IO ()
printTitle (Right title) = putStrLn $ "Title: " ++ title2str title
printTitle (Left errMsg) = putStrLn $ "Error: " ++ errMsg

makeDoctorName :: Either String Title -> String -> String -> Either String DoctorName
makeDoctorName (Right title) name surname
  | not (null name) && not (null surname) = Right (DoctorName title name surname)
  | null name = Left "Name cannot be empty."
  | null surname = Left "Surname cannot be empty."
makeDoctorName (Left err) _ _ = Left err -- Propagate the error from the title

-- Function to convert a DoctorName to a String, now using Either
doctorName2str :: Either String DoctorName -> String
doctorName2str (Right (DoctorName title name surname)) =
  title2str title ++ " " ++ name ++ " " ++ surname
doctorName2str (Left errMsg) = "Error: " ++ errMsg

-- Print function for demonstrating results
printDoctorName :: Either String DoctorName -> IO ()
printDoctorName result = putStrLn (doctorName2str result)

-- Parse a string to create a DoctorName, using Either for error handling
parseDoctorName :: String -> Either String DoctorName
parseDoctorName str =
  case words str of
    (titleStr : firstName : lastName : []) ->
      case str2title titleStr of
        Right title -> Right (DoctorName title firstName lastName)
        Left errMsg -> Left errMsg -- Propagate the title error
    (titleStr : firstName : []) ->
      case str2title titleStr of
        Right title -> Right (DoctorName title firstName "") -- No last name provided
        Left errMsg -> Left errMsg
    _ -> Left "Invalid format" -- Handle invalid input format

makeAddress :: String -> Either String Address
makeAddress addr
  | length addr > 5 = Right (Address addr) -- Valid address if longer than 5 characters
  | otherwise = Left "Invalid address: must be longer than 5 characters"

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

-- Convert a string to a Domain using Either for error handling
str2domain :: String -> Either String Domain
str2domain domain =
  if isValidDomain domain validDomains
    then Right (Domain domain)
    else Left "Invalid domain"

-- Convert Domain to String using Either
domain2str :: Either String Domain -> String
domain2str (Right (Domain domain)) = "Domain: " ++ domain
domain2str (Left errMsg) = "Error: " ++ errMsg

-- Print function for Domain
printDomain :: Either String Domain -> IO ()
printDomain result = putStrLn (domain2str result)

makeUsername :: String -> Either String Username
makeUsername usern
  | length usern > 3 = Right (Username usern)
  | otherwise = Left "Username must be longer than 3 characters"

-- Convert Username to String using Either
username2str :: Either String Username -> String
username2str (Right (Username usern)) = usern
username2str (Left errMsg) = "Error: " ++ errMsg

-- Print function for Username
printUsername :: Either String Username -> IO ()
printUsername result = putStrLn (username2str result)

makeEmail :: String -> String -> Either String Email
makeEmail usern domain
  | isValidDomain domain validDomains && length usern > 3 = Right (Email usern domain)
  | otherwise = Left "Invalid email format"

-- Convert Email to String using Either
email2str :: Either String Email -> String
email2str (Right (Email usern domain)) = usern ++ "@" ++ domain
email2str (Left errMsg) = "Error: " ++ errMsg

-- Print function for Email
printEmail :: Either String Email -> IO ()
printEmail result = putStrLn (email2str result)

-- Convert a string to a PhoneNumber using Either for error handling
makePhoneNumber :: Either String CountryCode -> String -> Either String PhoneNumber
makePhoneNumber (Right cc) num
  | length num > 7 && length num < 14 && isAllNumOrPipe num = Right (PhoneNumber cc num)
  | otherwise = Left "Invalid phone number format"
makePhoneNumber (Left errMsg) _ = Left errMsg

-- Convert PhoneNumber to String using Either
pn2str :: Either String PhoneNumber -> String
pn2str (Right (PhoneNumber cc num)) =
  case cc2str (Right cc) of
    Right ccStr -> "+" ++ ccStr ++ num -- Successfully convert CountryCode to string
    Left errMsg -> "Error: " ++ errMsg -- Handle error from cc2str
pn2str (Left errMsg) = "Error: " ++ errMsg -- Handle error directly from Either

-- Print function for PhoneNumber
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
        Left errMsg -> Left errMsg -- Propagate email error
    Left _ ->
      -- If parseEmail fails
      case parsePhoneNumber str of
        Right pn -> Right (PhoneNumberContact pn)
        Left errMsg -> Left errMsg -- Propagate phone number error

parseEmail :: String -> Either String (String, String)
parseEmail str =
  case break (== '@') str of
    (username, '@' : domain)
      | not (null username) && not (null domain) -> Right (username, domain) -- Successful parse
      | otherwise -> Left "Invalid email format: Username or domain is empty" -- Handle empty parts
    _ -> Left "Invalid email format: Missing '@' symbol" -- Handle missing '@'

parsePhoneNumber :: String -> Either String PhoneNumber
parsePhoneNumber str =
  let parts = words str -- Split the string by whitespace
   in case parts of
        [cc, ln]
          | all isDigit cc && all isDigit ln && length cc <= 3 && length ln >= 7 && length ln <= 14 ->
              case createCountryCode cc of -- Create the CountryCode here
                Right countryCode -> Right (PhoneNumber countryCode ln) -- Valid country code and local number
                Left errMsg -> Left errMsg -- Propagate error from createCountryCode
          | not (all isDigit cc) -> Left "Invalid country code: must be numeric"
          | not (all isDigit ln) -> Left "Invalid local number: must be numeric"
        _ -> Left "Invalid phone number format: expected format is '<country code> <local number>', local number min 7 max 14"

printContactInfo :: Either String ContactInfo -> IO ()
printContactInfo contactInfo =
  putStrLn $ ci2str contactInfo

makeGender :: String -> Either String Gender
makeGender "Male" = Right Male
makeGender "Female" = Right Female
makeGender "Other" = Right Other
makeGender _ = Left "Invalid gender"

-- Function to convert Gender to String using Either
gender2str :: Either String Gender -> String
gender2str (Right Male) = "Male"
gender2str (Right Female) = "Female"
gender2str (Right Other) = "Other"
gender2str (Left errMsg) = "Error: " ++ errMsg

-- Function to print the gender information
printGender :: Either String Gender -> IO ()
printGender gender = putStrLn $ gender2str gender

makeAge :: Int -> Either String Age
makeAge age
  | age >= 0 = Right (Age age)
  | otherwise = Left "Invalid age"

-- Function to convert Age to Int using Either
age2int :: Either String Age -> Int
age2int (Right (Age age)) = age
age2int (Left _) = -1 -- Return -1 for errors

-- Function to print the age information
printAge :: Either String Age -> IO ()
printAge age =
  case age of
    Right (Age a) -> putStrLn $ "Age: " ++ show a
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

makeLastName :: String -> Either String LastName
makeLastName lname
  | isAllAlpha lname && length lname > 0 = Right (LastName lname)
  | otherwise = Left "Invalid last name: must be non-empty and contain only alphabetic characters"

-- Function to convert LastName to String using Either
lname2str :: Either String LastName -> String
lname2str (Right (LastName lname)) = lname
lname2str (Left errMsg) = "Error: " ++ errMsg

-- Function to create FirstName using Either
makeFirstName :: String -> Either String FirstName
makeFirstName fname
  | isAllAlpha fname && length fname > 0 = Right (FirstName fname)
  | otherwise = Left "Invalid first name: must be non-empty and contain only alphabetic characters"

-- Function to convert FirstName to String using Either
fname2str :: Either String FirstName -> String
fname2str (Right (FirstName fname)) = fname
fname2str (Left errMsg) = "Error: " ++ errMsg

-- Function to create Name using Either
makeName :: String -> Either String Name
makeName name =
  let parts = words name
   in case makeFirstName (parts !! 0) of
        Right fnme ->
          if length parts == 2
            then case makeLastName (parts !! 1) of
              Right lnme -> Right (Name fnme lnme)
              Left errMsg -> Left errMsg
            else Left "Only first name and last name must be provided"
        Left errMsg -> Left errMsg

-- Function to convert Name to String using Either
name2str :: Either String Name -> String
name2str (Right (Name f l)) = (fname2str (Right f)) ++ " " ++ (lname2str (Right l))
name2str (Left errMsg) = "Error: " ++ errMsg

-- Function to print Name using Either
printName :: Either String Name -> IO ()
printName name =
  case name of
    Right n -> putStrLn $ "Name: " ++ name2str (Right n)
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

makePatientId :: Int -> Either String PatientId
makePatientId id
  | id > -1 = Right (PatientId id)
  | otherwise = Left "Invalid Patient ID: must be a non-negative integer"

-- Function to convert PatientId to Int using Either
pi2int :: Either String PatientId -> Int
pi2int (Right (PatientId id)) = id
pi2int (Left _) = -1 -- Return -1 if there is an error

-- Function to print PatientId using Either
printPatientId :: Either String PatientId -> IO ()
printPatientId pid =
  case pid of
    Right p -> putStrLn $ "Patient ID: " ++ show (pi2int (Right p))
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

printPatientInfo :: Either String (PatientId, Name, Age, Gender, ContactInfo, Address) -> IO ()
printPatientInfo result =
  case result of
    Right (pid, name, age, gender, contactInfo, address) ->
      putStrLn $
        "Patient Info:\n"
          ++ "ID: "
          ++ show pid
          ++ "\n"
          ++ "Name: "
          ++ show name
          ++ "\n"
          ++ "Age: "
          ++ show age
          ++ "\n"
          ++ "Gender: "
          ++ show gender
          ++ "\n"
          ++ "Contact Info: "
          ++ show contactInfo
          ++ "\n"
          ++ "Address: "
          ++ show address
    Left errMsg -> putStrLn $ "Error: " ++ errMsg

parseContactInfo :: [String] -> Either String ContactInfo
parseContactInfo contactInfo
  | not (null contactInfo) =
      case makeContactInfo (unwords contactInfo) of
        Right validContact -> Right validContact -- If makeContactInfo succeeds
        Left errMsg -> Left errMsg -- If makeContactInfo fails
  | otherwise = Left "Invalid Contact Info" -- Handle empty contact info

parsePatientInfo :: String -> Either String (PatientId, Name, Age, Gender, ContactInfo, Address)
parsePatientInfo str =
  let parts = words str
   in if length parts >= 6
        then
          let idStr = parts !! 0 -- Patient ID
              fullName = (parts !! 1) ++ " " ++ (parts !! 2)
              ageStr = parts !! 3 -- Age
              genderStr = parts !! 4 -- Gender
              addressStr = unwords (drop 5 parts) -- Everything after the first five as address
              (addrParts, contactParts) = splitAt 2 (words addressStr) -- Assuming address has two words and the rest is contact info
           in case readEither idStr of -- Safely parsing Patient ID
                Right pid ->
                  case makePatientId pid of -- Converting to PatientId
                    Right patientId ->
                      case makeName fullName of -- Validating and creating Name
                        Right name ->
                          case readEither ageStr of -- Safely parsing age
                            Right age ->
                              case makeAge age of -- Validating and creating Age
                                Right validAge ->
                                  case makeGender genderStr of -- Validating and creating Gender
                                    Right gender ->
                                      case parseContactInfo contactParts of -- Parsing contact info
                                        Right contactInfo ->
                                          case makeAddress (unwords addrParts) of -- Creating Address
                                            Right address ->
                                              Right (patientId, name, validAge, gender, contactInfo, address)
                                            Left err -> Left err -- Invalid address
                                        Left err -> Left err -- Invalid contact info
                                    Left err -> Left err -- Invalid gender
                                Left err -> Left err -- Invalid age
                            Left _ -> Left "Invalid age: not a valid integer" -- Invalid age string
                        Left err -> Left err -- Invalid name
                    Left err -> Left err -- Invalid Patient ID
                Left _ -> Left "Invalid Patient ID: not a valid integer" -- Invalid Patient ID string
        else Left "Invalid input: Not enough parts"

parsePatientId :: String -> Either String PatientId
parsePatientId str =
  case reads str of
    [(x, "")] ->
      case makePatientId x of
        Right patientId -> Right patientId
        Left errMsg -> Left "Invalid ID"
    _ -> Left "Incorrect patient ID"

parseAppointment :: String -> Either String Appointment
parseAppointment str =
  let parts = words str
   in if length parts >= 9 && head parts == "BOOK" && parts !! 1 == "APPOINTMENT"
        then
          let patientId = parsePatientId (parts !! 2) -- Parse Patient ID
              doctorTitleEither = str2title (parts !! 3) -- Extract the title as Either
              doctorFirstName = parts !! 4 -- First name
              doctorLastName = parts !! 5 -- Last name
              department = strToDept (parts !! 6) -- Department
              date = parseDate (parts !! 7) -- Date
              time = parseTime (parts !! 8) -- Time
           in case (patientId, doctorTitleEither, department, date, time) of
                (Right pid, Right title, Right dept, Right d, Right t) ->
                  case makeDoctorName (Right title) doctorFirstName doctorLastName of
                    Right doctorName -> Right (Appointment pid doctorName dept d t)
                    Left errMsg -> Left ("Error creating DoctorName: " ++ errMsg)
                (Left errMsg, _, _, _, _) -> Left ("Error parsing Patient ID: " ++ errMsg)
                (_, Left errMsg, _, _, _) -> Left ("Error parsing Doctor title: " ++ errMsg)
                (_, _, Left errMsg, _, _) -> Left "Error parsing Department"
                (_, _, _, Left errMsg, _) -> Left "Error parsing Date"
                (_, _, _, _, Left errMsg) -> Left "Error parsing Time"
        else Left "Invalid appointment format: Expected 'BOOK APPOINTMENT <patientId> <doctorTitle> <firstName> <lastName> <department> <date> <time>'"

parseSearchCriteria :: String -> Either String SearchCriteria
parseSearchCriteria str =
  let parts = words str
   in if parts !! 0 == "SEARCH" && parts !! 1 == "PATIENT" && parts !! 3 == "="
        then
          let fieldName = str2field (parts !! 2)
              value = (drop 4 parts)
           in case fieldName of
                Right fName -> Right (SearchCriteria fName (unwords value))
                Left errMsg -> Left errMsg
        else Left "Incorret syntax. Must be as follows: SEARCH PATIENT <field_name> = <value>"

parseUpdatePatientDetails :: String -> Either String (PatientId, UpdateInfo)
parseUpdatePatientDetails str =
  let parts = words str
   in if length parts >= 4 && parts !! 0 == "UPDATE" && parts !! 1 == "PATIENT"
        then
          let patientIdStr = parts !! 2
              fieldNameStr = parts !! 3
              fieldValueStr = unwords (drop 4 parts) -- The rest of the string after the field name
           in case (parsePatientId patientIdStr, str2field fieldNameStr) of
                (Right patientId, Right fieldName) -> Right (patientId, UpdateInfo fieldName fieldValueStr)
                (Left errMsg, _) -> Left errMsg
                (_, Left errMsg) -> Left errMsg
        else Left "Incorrect syntax. Must be as follows: UPDATE PATIENT <patientId> <fieldName> <new_value>"

parseAge :: String -> Either String Age
parseAge str =
  case reads str of
    [(x, "")] ->
      case makeAge x of
        Right age -> Right age
        Left errMsg -> Left "Invalid age"
    _ -> Left "Invalid age"

parseRegisterPatient :: String -> Either String PatientInfo
parseRegisterPatient str =
  let parts = words str
   in if length parts >= 10 && head parts == "REGISTER" && parts !! 1 == "PATIENT"
        then
          let pidStr = parts !! 2
              firstName = parts !! 3
              lastName = parts !! 4
              ageStr = parts !! 5
              genderStr = parts !! 6
              addressPart1 = parts !! 7
              addressPart2 = parts !! 8
              contactInfoStr = unwords (drop 9 parts) -- Take everything after part 9 as contact info
           in case (readEither pidStr :: Either String Int) of
                Right pidInt ->
                  case makePatientId pidInt of
                    Right pid ->
                      case (readEither ageStr :: Either String Int, makeGender genderStr) of
                        (Right ageInt, Right gender) ->
                          case makeAge ageInt of
                            Right age -> 
                              case makeName (firstName ++ " " ++ lastName) of
                                Right name ->
                                  case parseContactInfo (words contactInfoStr) of
                                    Right contactInfo ->
                                      case makeAddress (addressPart1 ++ " " ++ addressPart2) of
                                        Right addr ->
                                          Right (PatientInfo pid name age gender contactInfo addr)
                                        Left errMsg -> Left ("Invalid address info: " ++ errMsg)
                                    Left errMsg -> Left ("Invalid contact info: " ++ errMsg)
                                Left errMsg -> Left ("Invalid name: " ++ errMsg)
                            Left errMsg -> Left ("Error parsing age: " ++ errMsg)
                          
                        (Left errMsg, _) -> Left ("Invalid age: " ++ errMsg)
                        (_, Left errMsg) -> Left ("Invalid gender: " ++ errMsg)
                    Left errMsg -> Left ("Error reading the Id:" ++ errMsg)
                Left errMsg -> Left ("Error reading Patient id: " ++ errMsg)
        else Left "Format: <patient_id> <name> <age> <gender> <address1> <address2> <contact_info>"

parseCommand :: String -> Either String Command
parseCommand str =
  let parts = words str
   in case head parts of
        "REGISTER" -> fmap RegisterPatient (parseRegisterPatient str)
        "BOOK" -> fmap BookAppointment (parseAppointment str)
        "UPDATE" -> fmap (uncurry UpdatePatientDetails) (parseUpdatePatientDetails str)
        "SEARCH" -> fmap SearchPatient (parseSearchCriteria str)
        "VIEW" -> 
          if unwords (take 3 parts) == "VIEW ALL APPOINTMENTS"
            then Right ViewAllAppointments
            else Left "Invalid VIEW command"
        _ -> Left "Unknown command"

parseSystem :: String -> Either String System
parseSystem str =
  let commandStrings = lines str -- Split the input into separate lines (commands)
   in fmap System (mapM parseCommand commandStrings) -- Parse each command


initialHospitalState :: HospitalState
initialHospitalState = HospitalState [] []

executeCommand :: HospitalState -> Command -> Either String (HospitalState, String)

executeCommand hs ViewAllAppointments =
  Right (hs, "All appointments:\n" ++ showAppointments (getAllAppointments hs))

executeCommand hs (BookAppointment app) =
  let updatedState = addAppointment hs app
  in Right (updatedState, "Appointment booked successfully. Updated state: " ++ show updatedState)

executeCommand hs (RegisterPatient pInfo) =
  let updatedState = addPatientInfo hs pInfo
  in Right (updatedState, "Patient registered successfully. Updated state: " ++ show updatedState)

executeCommand hs (UpdatePatientDetails pid updateInfo) = 
  case updatePatientDetails hs pid updateInfo of
    Right updatedState -> Right (updatedState, "Patient details updated successfully. Updated state: " ++ show updatedState)
    Left errMsg -> Left errMsg

executeCommand hs (SearchPatient criteria) =
  case searchPatient hs criteria of
    Right result -> Right (hs, "Search result: " ++ show result)
    Left errMsg -> Left errMsg


-- Helper function to get patient information by ID
getPatientInfo :: HospitalState -> PatientId -> Either String PatientInfo
getPatientInfo hs pid =
  case find (\pInfo -> patientId pInfo == pid) (patientInfos hs) of
    Just pInfo -> Right pInfo
    Nothing -> Left $ "Patient with ID " ++ show pid ++ " not found."

-- Function to add an appointment to hospital state
addAppointment :: HospitalState -> Appointment -> HospitalState
addAppointment hs app = hs { appointments = app : appointments hs }

-- Function to add patient information to hospital state
addPatientInfo :: HospitalState -> PatientInfo -> HospitalState
addPatientInfo hs pInfo = hs { patientInfos = pInfo : patientInfos hs }

-- Function to update patient details in hospital state
updatePatientDetails :: HospitalState -> PatientId -> UpdateInfo -> Either String HospitalState
updatePatientDetails hs pid updateInfo = do
  pInfo <- getPatientInfo hs pid
  updatedPInfo <- applyUpdate pInfo updateInfo
  return $ hs { patientInfos = updateList (\p -> patientId p == pid) (const updatedPInfo) (patientInfos hs) }

-- Function to search for patient information based on criteria
searchPatient :: HospitalState -> SearchCriteria -> Either String [PatientInfo]
searchPatient hs (SearchCriteria fieldName value) =
  Right $ filter (matchesCriteria fieldName value) (patientInfos hs)

-- Utility function to apply an update to PatientInfo
applyUpdate :: PatientInfo -> UpdateInfo -> Either String PatientInfo
applyUpdate pInfo (UpdateInfo fieldName value) =
  case fieldName of
    NameField ->
      case makeName value of
        Right name -> Right $ pInfo { patientName = name }
        Left errMsg -> Left ("Invalid name: " ++ errMsg)
    AgeField ->
      case makeAge (read value) of
        Right age -> Right $ pInfo { patientAge = age }
        Left errMsg -> Left ("Invalid age, " ++ errMsg)
    GenderField ->
      case makeGender value of
        Right gender -> Right $ pInfo { patientGender = gender }
        Left errMsg -> Left ("Invalid gender " ++ errMsg)
    ContactField ->
      case parseContactInfo [value] of
        Right contactInfo -> Right $ pInfo { patientContacts = contactInfo }
        Left errMsg -> Left errMsg
    AddressField ->
      case makeAddress value of
        Right addr -> Right $ pInfo { patientAddress = addr }
        Left errMsg -> Left errMsg

-- Function to update a list based on a predicate and update function
updateList :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateList _ _ [] = []
updateList predicate updateFunc (x:xs)
  | predicate x = updateFunc x : xs  -- Update the first matching element and leave the rest unchanged
  | otherwise = x : updateList predicate updateFunc xs


instance Read Age where
  readsPrec _ str = case reads str of
    [(age, rest)] -> [(Age age, rest)]
    _ -> []
-- Function to check if a patient matches a list of criteria
matchesCriteria :: FieldName -> String -> PatientInfo -> Bool
matchesCriteria field value patient =
  case field of
    AgeField ->
      case reads value of
        [(age, "")] -> patientAge patient == age
        _ -> False

    -- Compare Gender
    GenderField ->
      case makeGender value of
        Right gender -> patientGender patient == gender
        Left _ -> False
    
    NameField ->
      case makeName value of
        Right nm -> patientName patient == nm
        Left _ -> False

    -- Compare Address
    AddressField ->
      case makeAddress value of
        Right addr -> patientAddress patient == addr  -- Assuming `Address` is a simple string
        Left _ -> False

    -- Compare Contact Info (assuming `patientContacts` is a string or can be parsed)
    ContactField ->
      case parseContactInfo [value] of
        Right contactInfo -> patientContacts patient == contactInfo
        Left _ -> False

    -- If the field name doesn't match any known fields
    _ -> False
    


-- Function to retrieve and display all appointments
getAllAppointments :: HospitalState -> [Appointment]
getAllAppointments hs = appointments hs

showAppointments :: [Appointment] -> String
showAppointments apps = unlines (map show apps)
