# fp-2024

## Introduction

This project will be about a hospital registration; Objects will include but will not be limited to: pacients, doctors, time slots.
Pacients will have a variety of symptoms, whereas doctors will specialize in some types of diseases. A reception mechanism will enforce efficient and robust service.

# Usage example:
UserContacts [Just (Email "lols" "example.com")] [Just (PhoneNumber (ManyDigitCC (Digit '6') (ManyDigitCC (Digit '8') (SingleDigitCC (Digit '7')))) " 4350685")]

# Notes
makeContactInfo accepts either email or phone in format "<country_code> <local_number>"
Address is two words

## Author
Benas Kučinskas, 2024 PS, 2 year third semester student