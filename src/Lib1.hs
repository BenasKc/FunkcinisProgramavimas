module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.

-- Hospital reception registration
completions :: [String]
completions = [
    "Cardiology", "Neurology",
    "Orthopedics", "Pediatrics",
    "PATIENT", "BOOK",
    "APPOINTMENT", "REGISTER",
    "UPDATE", "SEARCH",
    "FEMALE", "MALE", 
    "OTHER"
    ]
