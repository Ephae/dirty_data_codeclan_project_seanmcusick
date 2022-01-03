# This document should include:

## A brief introduction to the dataset
seabird_data -
sheet: Ship data by record ID
12310 observations of 27 Variables
27 variables:
  * Record (1 - 12310) original record order
  * Record ID (1083001 - 88007036) - record identifier to link ship and bird data
  * Date (1969-07-31 -- 1990-12-21) 
  * Time (- time has a date - 1900-01-01 00:00:00 to 1899-12-31 01:00:00) needs to be removed // local time _at start of count_
  * Latitude (-69 to -19) Where will that place it? decimal
  * Longitude (50 to 179.9833) Where will that place it?
  * EW - East or West hemisphere
  * SACT - (1 - 9) - Ship Activity (type of ship)
  * SPEED (0 - 22) - ship speed - knots
  * SDIR - (0 - 358) ship direction in continuous degrees
  * CLD (0 - 2) Cloud Cover
  * PREC (0 - 11) Precipations
  * WSPEED - wind speed, classifications (0 - 12) 
  * WDIR - wind direction? (0 - 350) (359 would be max)
  * ATMP - TRUE or NA  - air temperature ??? supposedly continuous degrees
  * APRS (991 - 1040) - Atmospheric sea-level pressure - millibars
  * SSTE (1 - 6) - Sea State - 
  * STMP (-1.0 -- 27.0 ) - sea surface temperature
  * SAL (TRUE or NA) only 1 true - sea surface salinitry - parts per thousan?
  * DEPTH (60 - 2200) - sea floor depth
  * OBS - observer? - Bain to Thomas - 27 obs
  * CSMETH (F or P) - ??? BIRD COUNT - Census Method - Full 10 mins or Part/casual
  * MONTH (1-12) no NAs // derived
  * SEASN  (spring/summer/autumn/winter) (southern hemisphere)
  * LONG360 (50.00000
 - 184.4167) 
  * LATCELL (-69.5 -- -19.5) one degree latitude call centre
  * LONGECELL (50.5 -- 184.5)one degree longitude call centre
  
sheet: Bird data by record ID
  * RECORd
  * RECORD ID
  * Species common name (taxon [AGE / SEX / PLUMAGE PHASE])
  * Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])
  * Species abbreviation
  * AGE
  * WANPLUM -Wandering albatross plumage phase (John Jenkins counts)
  * PLPHASE - Plumage phase
  * SEX
  * COUNT
  * NFEED - Feeding numbers (unspecified whether around ship or naturally)
  * OCFEED - Feeding occurrence (unspecified whether around ship or naturally)
  * NSOW - Sitting on water numbers
  * OCSOW - Sitting on water occurrence
  * NSOICE - Sitting on ice numb
  * OCSOICE - Sitting on ice occurrence 
  * OCSOSHP - Sitting on ship occurrence
  * OCINHD - In hand occurrence
  * NFLYP - Flying past numbers
  * OCFLYP - Flying past occurrence
  * NACC - Accompanying numbers
  * OCACC - Accompanying occurrence
  * NFOLL - Ship following in wake numbers
  * OCFOL - Ship following in wake occurrence
  * OCMOULT - Occurrence of moult
  * OCNATFED - Naturally feeding occurrence (not feeding around or behind ship)
  * 
  * 
  
  
  
  
sheet: Ship data codes - Table
  * Label //	Definition //	Type of variable, unit and coding

  * 
  
sheet: Bird data codes
  * Label //	Definition //	Type of variable, unit and coding
  * 
    

## A list of any assumptions you have made


## The steps you took to clean the data (you don’t need to write out in detail every step, but a combination of commented code chunks with surrounding overall explanations would be great).

* Do we need all the variables for this data?
* You’ll need to join the ship data to the bird record data


## The answers to the questions presented in the task brief
* Which bird had the most individual sightings?
* Which bird had the highest total count?
* Which bird had the highest total count above a latitude of -30?
* How many different types of birds were only ever seen in groups of 1?
* How many penguins were seen? (Hint: there are many types of penguin)

## Any other interesting analyses or conclusions you come across.



# Expectations
The clean data should be in tidy format as covered in week 3.
Follow the style guide covered in week 1 for both code style, and naming data and variables.
Look over the reproducibility lesson from week 1 again to help you think about how to organise your project. Following the steps there to ensure reproducibility and clarity for review, and recap the hints and tips for writing documentation.
While the datasets in all of these projects are pretty small (and so processing power won’t be an issue), do think about efficient ways of answering the questions. By this we don’t necessarily mean that you should write code that runs as fast as possible (as this sometimes can make code more difficult to read), but rather look to avoid overcomplicating the code, and any ‘hard coding’.