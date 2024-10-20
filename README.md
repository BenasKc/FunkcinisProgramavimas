# fp-2024

## Introduction

This project will be about a hospital registration; Objects will include but will not be limited to: pacients, doctors, time slots.
Pacients will have a variety of symptoms, whereas doctors will specialize in some types of diseases. A reception mechanism will enforce efficient and robust service.

# Usage example:
UserContacts [Just (Email "lols" "example.com")] [Just (PhoneNumber (ManyDigitCC (Digit '6') (ManyDigitCC (Digit '8') (SingleDigitCC (Digit '7')))) " 4350685")]

# Notes
makeContactInfo accepts either email or phone in format "<country_code> <local_number>"
Address is two words

# Changes to BNF

`<new_value> ::= <string>`

`<value> ::= <string>`

Were not implemented, deemed unnecessary and assignment was straightforward



`<date> ::= <digit> <digit> "-" <digit> <digit> "-" <digit> <digit> <digit> <digit>`
`<time> ::= <digit> <digit> ":" <digit> <digit>`

Was converted to

`<date> ::= <Int> <Int> <Int>`
`<time> ::= <Int> <Int>`
to simplify things


`<domain> ::= <letter> | <letter> <domain>`
to
`<domain> ::= <string>`

`<username> ::= <letter> | <letter> <username>`
Was removed, as no authentication mechanism was in place, deemed unnecessary


`<phone_number> ::= "+" <country_code> <digit> <digit> <digit> <digit> <digit> <digit> <digit> <digit> <digit> <digit>`
to
`<phone_number> ::= <country_code> <string>`
to simplify things

`<first_name> ::= <letter> | <letter> <first_name>`

`<last_name> ::= <letter> | <letter> <last_name>`
Also were converted to strings

To commands, a new function was added - view all appointments. Implemented to allow viewing state of program more in depth

## Author
Benas Kučinskas, 2024 PS, 2 year third semester student