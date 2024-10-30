{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified
import Lib2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
      -- Some simple tests to ensure the program is working
    testCase "Register Patient Parsing" $
      Lib2.parseQuery "REGISTER PATIENT 5 John Doe 18 Male Washington DC example@example.com" @?=
      Right (RegisterQuery (PatientInfo
                             { patientId = PatientId 5,
                               patientName = Name (FirstName "John") (LastName "Doe"),
                               patientAge = Age 18,
                               patientGender = Male,
                               patientContacts = EmailContact (Email "example" "example.com"),
                               patientAddress = Address "Washington DC"
                             })),
    testCase "Book appointment" $
      Lib2.parseQuery "BOOK APPOINTMENT 5 Dr Matt Newman Neurology 12-08-2024 12:35" @?= Right (BookAppointmentQuery (Appointment {appointmentPatientId = PatientId 5, appointmentDoctorName = DoctorName Dr "Matt" "Newman", appointmentDepartment = Neurology, appointmentDate = Date 12 8 2024, appointmentTime = Time 12 35})),

    testCase "Invalid Query Parsing" $
      Lib2.parseQuery "asd" @?= Left "Unknown command. Errors encountered: \n- Register: Unknown command:asd Invalid command: expected 'REGISTER PATIENT'\n- Appointment: Invalid command: Incorrect syntax. Must start with 'BOOK APPOINTMENT'.\n- Update: Incorrect syntax. Must start with 'UPDATE PATIENT'.\n- Search: Incorrect syntax. Must start with 'SEARCH PATIENT'."
  ]