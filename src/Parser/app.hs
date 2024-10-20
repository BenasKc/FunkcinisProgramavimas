

parseAppointmentInfo :: String -> Maybe Appointment
parseAppointmentInfo str =
    case words str of
        (idStr:doctorNameStr:deptStr:dateStr:timeStr:[]) -> do
            patientId <- makePatientId (read idStr)
            doctorName <- str2DoctorName doctorNameStr  -- Assuming str2DoctorName is defined
            department <- strToDept deptStr
            date <- parseDate dateStr
            time <- parseTime timeStr
            return $ Appointment patientId doctorName department date time
        _ -> Nothing

parseUpdateInfo :: String -> Maybe UpdateInfo
parseUpdateInfo str =
    case break (== '=') str of
        (fieldStr, '=':valueStr) -> do
            fieldName <- str2field fieldStr
            return $ UpdateInfo fieldName valueStr
        _ -> Nothing

parseSearchCriteria :: String -> Maybe SearchCriteria
parseSearchCriteria str =
    case break (== '=') str of
        (fieldStr, '=':valueStr) -> do
            fieldName <- str2field fieldStr
            return $ SearchCriteria fieldName valueStr
        _ -> Nothing

data Appointment = Appointment {
    appointmentPatientId :: PatientId,
    appointmentDoctorName :: DoctorName,
    appointmentDepartment :: Department,
    appointmentDate :: Date,
    appointmentTime :: Time
} deriving (Show, Eq)

parseAppointment :: String -> Maybe Appointment
parseAppointment str = 
    let parts = words str
    in if length parts == 6 && head parts == "BOOK" && parts !! 1 == "APPOINTMENT"
       then 
           let patientId = parsePatientId (parts !! 2)
               doctorName = parseDoctorName (parts !! 3 ++ " " ++ parts !! 4) -- Assuming name is in "First Last" format
               department = strToDept (parts !! 5)
               date = parseDate (parts !! 6)
               time = parseTime (parts !! 7)
           in case (patientId, doctorName, department, date, time) of
               (Just pid, Just dName, Just dept, Just d, Just t) -> 
                   Just (Appointment pid dName dept d t)
               _ -> Nothing
       else Nothing

data UpdateInfo = UpdateInfo FieldName String deriving (Show, Eq)

parseAppointment :: String -> Maybe Appointment
parseAppointment str = 
    let parts = words str
    in if length parts == 6 && head parts == "BOOK" && parts !! 1 == "APPOINTMENT"
       then 
           let patientId = parsePatientId (parts !! 2)
               doctorName = parseDoctorName (parts !! 3 ++ " " ++ parts !! 4) -- Assuming name is in "First Last" format
               department = strToDept (parts !! 5)
               date = parseDate (parts !! 6)
               time = parseTime (parts !! 7)
           in case (patientId, doctorName, department, date, time) of
               (Just pid, Just dName, Just dept, Just d, Just t) -> 
                   Just (Appointment pid dName dept d t)
               _ -> Nothing
       else Nothing


data SearchCriteria = SearchCriteria FieldName String deriving (Show, Eq)

parseSearchCriteria :: String -> Maybe SearchCriteria
parseSearchCriteria str =
    let parts = words str
    in if length parts == 4 && parts !! 0 == "SEARCH" && parts !! 1 == "PATIENT" && parts !! 2 == "="
       then
           let fieldName = str2field (parts !! 2)
               value = parts !! 3
           in case fieldName of
               Just fName -> Just (SearchCriteria fName value)
               Nothing -> Nothing
       else Nothing
