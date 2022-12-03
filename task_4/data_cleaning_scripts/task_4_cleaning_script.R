# First, let's load in any packages we think we'll need for cleaning, and the raw data.

library(tidyverse)
library(janitor)
candy2015 <- readxl::read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
candy2016 <- readxl::read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
candy2017 <- readxl::read_xlsx("raw_data/boing-boing-candy-2017.xlsx")



# candy2015 Notes:

# 1. 'How old are you?' should be age, 'Are you going trick or treating yourself' should be simplified. Could be 'trick_or_treating'. Could potentially be a Boolean.

# 2. snake_case, and consistent titles. 'Anonmyous brown globs...' needs to be something less silly, that makes more sense.


# candy2016 Notes:

#1. Much the same as the last time, except we have more columns. "Your gender" should be changed  to gender and so on.
#2. There is an 'old enough to know better' entry in age! All non-numeric values should be NA. We can convert the whole column to numeric.


# General Issues:

#1. No MEH option in candy2015. 
#2. Also, no country, gender or province in candy2015
#3. In candy2017, there is no timestamp. There is, however, an internal ID column NOT present in the other 2 sheets. Could timestamp data be gleamed from internal ID? If not, we will simply have to deal with a huge amout of NAs.
#4. Loads of NAs in every column! Will have to decide how to deal with this, particularly when performing analysis.
#5. Country and state entries are very inconsistent. I predict a whole lot of regex!


# Gameplan:
#1. Fix the column names. Silly comments in them trying to be funny should be removed. They all need to be the same in each sheet so we can join the 3 tables later on. Lots of renaming, then clean_names will be ran. 

#2. pivot_longer() the candy entries into 'type_of_candy' and 'candy_preference'.

#3. Join the tables. We'll probably have to create an id column to join by

#4. Convert age to numeric. Also, have it rounded to 0 decimal places to avoid decimals.

#5. Fix the country and state entries so they're consistent.


# Process
#1. First we clean up the column names. 
candy2015 <- candy2015 %>% 
  clean_names() %>% 
  rename(age = how_old_are_you) %>% 
  rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself)



candy2016 <- candy2016 %>% 
  clean_names() %>% 
  rename(age = how_old_are_you) %>% 
  rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>% 
  rename(gender = your_gender) %>% 
  rename(country = which_country_do_you_live_in) %>% 
  rename(province = which_state_province_county_do_you_live_in)
  


candy2017 <- candy2017 %>% 
  clean_names() %>% 
  rename(age = q3_age) %>% 
  rename(trick_or_treating = q1_going_out) %>% 
  rename(country = q4_country) %>% 
  rename(province = q5_state_province_county_etc) %>% 
  rename(gender = q2_gender)

colnames(candy2017 ) <- gsub("q6_","",colnames(candy2017))

  



#1.1. Now, we need to fix the sweets. We rename so we have the same column names for each table.

candy2017 <- candy2017 %>% 
  rename(x100_grand_bar = `100_grand_bar`) %>% 
  rename(anonymous_brown_globs_that_come_in_black_and_orange_wrappers = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes) %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate)


candy2015 <- candy2015 %>% 
  rename(bonkers_the_candy = bonkers) %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate) %>% 
  rename(hersheys_kissables = hershey_s_kissables)

candy2016 <- candy2016 %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate)
  
  
# Now we have consistent names across the tables, we should be able to join them.

candy_full <- full_join(candy2016, candy2015)


candy_full <- full_join(candy_full, candy2017)


# Now we need to clean the age column.

candy_full <- candy_full %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(age = na_if(age, age > 120 | age < 0))



# Now we will begin the dreaded regex. We need to have standardised information for both country and province. We will begin with countries.



# We'll start with removing all the silly entries, these should all be NA.

candy_full <- candy_full %>% 
  mutate(country = str_to_upper(country)) %>% 
  mutate(country = str_replace_all(country, "NOT THE USA OR CANADA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "GOD'S COUNTRY", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "ONE OF THE BEST ONES", NA_character_)) %>%
  mutate(country = str_replace_all(country, "THERE ISN'T ONE FOR OLD MEN", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "THIS ONE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "NARNIA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "I DON'T KNOW ANYMORE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "FEAR AND LOATHING", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "SUBSCRIBE TO DM4UZ3 ON YOUTUBE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "INSANITY LATELY", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "THE REPUBLIC OF CASCADIA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "SEE ABOVE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "CASCADIA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "A TROPICAL ISLAND SOUTH OF THE EQUATOR", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "N. AMERICA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "[[:digit:]]", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "SOMEWHERE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "DENIAL", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "ATLANTIS", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "EARTH", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "EUROPE", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "EUA", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "NEVERLAND", NA_character_)) %>% 
  mutate(country = str_replace_all(country, "^A$", NA_character_))
                                     
                                     
                              
# Now we standardise all of the country names                                             

candy_full <- candy_full %>% 
  mutate(country = str_replace_all(country, "NORTH CAROLINA", "USA")) %>% 
  mutate(country = str_replace_all(country, "ALASKA", "USA")) %>% 
  mutate(country = str_replace_all(country, "PITTSBURGH", "USA")) %>% 
  mutate(country = str_replace_all(country, "TRUMPISTAN", "USA")) %>% 
  mutate(country = str_replace_all(country, "NEW JERSEY", "USA")) %>% 
  mutate(country = str_replace_all(country, "CALIFORNIA", "USA")) %>% 
  mutate(country = str_replace_all(country, "THE YOO ESS OF AAAYYYYYY", "USA")) %>% 
  mutate(country = str_replace_all(country, "NEW YORK", "USA")) %>% 
  mutate(country = str_replace_all(country, "^UD$", "USA")) %>% 
  mutate(country = str_replace_all(country, "USSA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAUSAUSA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAS", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA? HARD TO TELL ANYMORE..", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA!!!!!!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA! USA! USA!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA! USA!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA USA USA!!!!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA USA USA USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA USA USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAA (I THINK BUT IT'S AN ELECTION YEAR SO WHO CAN REALLY TELL)", "USA")) %>% 
  mutate(country = str_replace_all(country, "US OF A", "USA")) %>% 
  mutate(country = str_replace_all(country, "US", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITS STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITES STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STSTES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STETES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATSS", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATES OF AMERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATED", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATEA", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STATE", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED STAES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED SATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITED  STATES OF AMERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNITE STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNIED STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "UNHINGED STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "U.S.A.", "USA")) %>% 
  mutate(country = str_replace_all(country, "U.S.", "USA")) %>% 
  mutate(country = str_replace_all(country, "U S A", "USA")) %>% 
  mutate(country = str_replace_all(country, "U S", "USA")) %>% 
  mutate(country = str_replace_all(country, "THE UNITED STATES OF AMERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "THE UNITED STATES", "USA")) %>% 
  mutate(country = str_replace_all(country, "THE BEST ONE - USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "SUB-CANADIAN NORTH AMERICA... 'MERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "MURRIKA", "USA")) %>% 
  mutate(country = str_replace_all(country, "MURICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "MERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "I PRETEND TO BE FROM CANADA, BUT I AM REALLY FROM THE USA.", "USA")) %>% 
  mutate(country = str_replace_all(country, "AMERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "AHEM....AMERCA", "USA")) %>% 
  mutate(country = str_replace_all(country, "'MERICA", "USA")) %>% 
  mutate(country = str_replace_all(country, "'USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAA? HARD TO TELL ANYMORE..", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAA!!!", "USA")) %>% 
  mutate(country = str_replace_all(country, "USAA", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA\\? HARD TO TELL ANYMORE..", "USA")) %>% 
  mutate(country = str_replace_all(country, "USA \\(I THINK BUT IT'S AN ELECTION YEAR SO WHO CAN REALLY TELL\\)", "USA")) %>% 
  mutate(country = str_replace_all(country, "THE USA", "USA")) %>% 
  mutate(country = str_replace_all(country, "U.K.", "UK")) %>% 
  mutate(country = str_replace_all(country, "UNITED KINGDOM", "UK")) %>% 
  mutate(country = str_replace_all(country, "UNITED KINDOM", "UK")) %>% 
  mutate(country = str_replace_all(country, "SCOTLAND", "UK")) %>% 
  mutate(country = str_replace_all(country, "ENGLAND", "UK")) %>% 
  mutate(country = str_replace_all(country, "ENDLAND", "UK")) %>% 
  mutate(country = str_replace_all(country, "THE NETHERLANDS", "NETHERLANDS")) %>% 
  mutate(country = str_replace_all(country, "ESPAÑA", "SPAIN")) %>% 
  mutate(country = str_replace_all(country, "BRASIL", "BRAZIL")) %>% 
  mutate(country = str_replace_all(country, "SOVIET CANUKSTAN", "CANADA")) %>% 
  mutate(country = str_replace_all(country, "AUSATRIA", "AUSTRIA")) %>% 
  mutate(country = str_replace_all(country, "AUSATRALIA", "AUSTRALIA")) %>% 
  mutate(country = str_replace_all(country, "AUSA", "AUSTRALIA")) %>% 
  mutate(country = str_replace_all(country, "CANADA`", "CANADA")) %>% 
  mutate(country = str_replace_all(country, "CANAE", "CANADA")) %>% 
  mutate(country = str_replace_all(country, "^CAN$", "CANADA")) %>% 
  mutate(country = str_replace_all(country, "^KOREA$", "SOUTH KOREA"))
  
  
# Note: I have assumed all Koreans are South Korean. 


# Next; we must standardise the provinces.

# First, some entries are invalid. These will be converted to NAs.

candy_full <- candy_full %>% 
  mutate(province = str_to_upper(province)) %>% 
  mutate(province = str_replace_all(province, "[[:digit:]]", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^AMERICA$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^A$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^AGAIN$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^A$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^AB$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^CANADA$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "CASCADIA", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "EMERALD CITY, PETROLIA", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^CHAOIS$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "IN THE MIDDLE", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "NOT APPLICABLE", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "NUNYA, BIZNESS, USA", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "OBLIVION IF THINGS KEEP GOING THIS WAY", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "P€##$¥|>@#,@", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "PSYCHOTIC", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^NA$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^NOPE$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "SUB EARTH", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^UNITED STATES$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^USA$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^UR MOM$", NA_character_)) %>% 
  mutate(province = str_replace_all(province, "^WHERE\\?$", NA_character_))


# Now, the first 6 letters of the alphabet; A-F
  
candy_full <- candy_full %>%
  mutate(province = str_replace_all(province, "^AR$", "ARKANSAS")) %>% 
  mutate(province = str_replace_all(province, "^AZ$", "ARIZONA")) %>% 
  mutate(province = str_replace_all(province, "^AL$", "ALABAMA")) %>% 
  mutate(province = str_replace_all(province, "ALBEQRRTA", "ALBERTA")) %>% 
  mutate(province = str_replace_all(province, "ALASKA, MATANUSKA-SUSITNA BOROUGH", "ALASKA")) %>% 
  mutate(province = str_replace_all(province, "ALAMEDA COUNTY, CALIFORNIA", "CALIFORNIA")) %>% 
  mutate(province = str_replace_all(province, "\\|NC", "NORTH CAROLINA")) %>% 
  mutate(province = str_replace_all(province, "^AK$", "ALASKA")) %>% 
  mutate(province = str_replace_all(province, "ARLINGTON, VA", "VIRGINIA")) %>% 
  mutate(province = str_replace_all(province, "ASTORIA NY", "NEW YORK")) %>% 
  mutate(province = str_replace_all(province, "ATLANTA, GA", "GEORGIA")) %>%
  # Now onto B
  mutate(province = str_replace_all(province, "BADEN WÜRTTEMBERG", "BADEN-WURTTEMBERG")) %>%
  mutate(province = str_replace_all(province, "^BC$", "BRITISH COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "^BENTON$", "NEWCASTLE")) %>%
  mutate(province = str_replace_all(province, "BERKELEY, CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "BERLIN", "BRANDENBURG")) %>%
  mutate(province = str_replace_all(province, "BLOOMINGTON, IN", "INDIANA")) %>%
  mutate(province = str_replace_all(province, "BOO JERSEY", "NEW JERSEY")) %>%
  mutate(province = str_replace_all(province, "BRISTOL COUNTY, RHODE ISLAND", "RHODE ISLAND")) %>%
  mutate(province = str_replace_all(province, "BRITISH COLUMBIA, CANADA", "BRITISH COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "^BROOKLYN$", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "BROOKLYN, KINGS COUNTY NY", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "BUNCOMBE COUNTY, NORTH CAROLINA", "NORTH CAROLINA")) %>%
  # Now the Cs
  mutate(province = str_replace_all(province, "^CA$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CA, ALAMEDA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CA, MADERA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CA, SAN FRANCISCO", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^CALI$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALI BABY!", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^CALIF$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^CALIFOR$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORMIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CAILIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, ALAMEDA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, BUTTE COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, CONTRA COSTA COUNTY, PLEASANT HILL", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, LA COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, MENDOCINO COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, SAN DIEGO", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, SAN DIEGO COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, SAN FRANCISCO", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, SOLANO COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, USA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA, VENTURA COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CALIFORNIA COUNTY", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "CARVER COUNTY, MN", "MINNESOTA")) %>%
  mutate(province = str_replace_all(province, "CASCACHUAN", "SASKATCHEWAN")) %>%
  mutate(province = str_replace_all(province, "CASTRO MARIM", "ALGARVE")) %>%
  mutate(province = str_replace_all(province, "CDMX", "MEXICO CITY")) %>%
  mutate(province = str_replace_all(province, "CHICAGO ILLINOIS", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "CHICAGO, IL", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "CHICAGO, ILLINOIS", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "^CO$", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "CO, LARIMER COUNTY", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "^COLO$", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "COLORADO \\(CO\\)", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "COLORADO LARIMER COUNTY", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "COLORADO, BOULDER", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "COLORADO, JEFFERSON COUNTY", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "CONNECTIUCT", "CONNECTICUT")) %>%
  mutate(province = str_replace_all(province, "^CT$", "CONNECTICUT")) %>%
  mutate(province = str_replace_all(province, "CUYAHOGA COUNTY, OHIO", "OHIO")) %>%
  # Now the Ds
  mutate(province = str_replace_all(province, "^DC$", "DISTRICT OF COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "DENVER, COLORADO", "COLORADO")) %>%
  mutate(province = str_replace_all(province, "DONA ANA COUNTY, NEW MEXICO", "NEW MEXICO")) %>%
  mutate(province = str_replace_all(province, "DOUGLASS COMMONWEALTH", "DISTRICT OF COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "DURHAM COUNTY, NORTH CAROLINA", "NORTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "DURHAM NC", "NORTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "DUTCHESS COUNTY NY", "EW YORK")) %>%
  # Now the Es
  mutate(province = str_replace_all(province, "EASTON, PA", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "EXTON PA", "PENNSYLVANIA")) %>%
  # Now the Fs
  mutate(province = str_replace_all(province, "FIFE, SCOTLAND", "FIFE")) %>%
  mutate(province = str_replace_all(province, "^FL$", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "FL, ORANGE COUNTY", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "FLORIDA,  BAY COUNTY", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "FROM FLORIDA", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "FREE STATE OF TEXAS", "TEXAS")) %>% 
  mutate(province = str_replace_all(province, "CHICAGO", "ILLINOIS")) %>%  
  mutate(province = str_replace_all(province, "^EW YORK$", "NEW YORK"))



# Now the next 10 letters of the alphabet; G-P
candy_full <- candy_full %>%
  # G
  mutate(province = str_replace_all(province, "^GA$", "GEORGIA")) %>%
  mutate(province = str_replace_all(province, "GA - GEORGIA", "GEORGIA")) %>%
  mutate(province = str_replace_all(province, "GAWJA", "GEORGIA")) %>%
  mutate(province = str_replace_all(province, "GENESEE COUNTY, MI", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "GLENDORA, LOS ANGELES, CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "GREATER LONDON", "LONDON")) %>%
  mutate(province = str_replace_all(province, "GREATER MANCHESTER", "MANCHESTER")) %>%
  mutate(province = str_replace_all(province, "GREIFSWALD", "MECKLENBURG-VORPOMMERN")) %>%
  # H
  mutate(province = str_replace_all(province, "HARRIS COUNTY, TEXAS", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "HAWAII, HONOLULU", "HAWAII")) %>%
  mutate(province = str_replace_all(province, "HENRICO COUNTY, VA", "VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "HENRICO, VA", "VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "^HESSE$", "HESSEN")) %>%
  mutate(province = str_replace_all(province, "^HI$", "HAWAII")) %>%
  # I
  mutate(province = str_replace_all(province, "^IA$", "IOWA")) %>%
  mutate(province = str_replace_all(province, "IDAHO ADA", "IDAHO")) %>%
  mutate(province = str_replace_all(province, "^IL$", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "ILLINOIS \\(BIRTH STATE OF HILARY CLINTON WHICH MAY OR MAY NOT BE A GOOD THING\\)", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "ILLINOIS, COOK", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "^COOK$", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "ILLINOISLINOIS", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "ILLNOIS", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "^ILL$", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "^IN$", "INDIANA")) %>%
  mutate(province = str_replace_all(province, "INCHEON", "SEOUL CAPITAL")) %>%
  mutate(province = str_replace_all(province, "ISSAQUAH, WA, KING COUNTY", "WASHINGTON")) %>%
  # K
  mutate(province = str_replace_all(province, "KALAMAZOO COUNTY, MI", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "KANAGAWA", "KANTO")) %>%
  mutate(province = str_replace_all(province, "KANSAS, DOUGLAS COUNTY", "KANSAS")) %>%
  mutate(province = str_replace_all(province, "^KING COUNTY, WASHINGTON$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "KING COUNTY, WASHINGTON.", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^KINGS COUNTY NY$", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "KINGS COUNTY, NEW YORK", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "KINGS, NEW YORK", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "KNOX COUNTY, TENNESSEE", "TENNESSEE")) %>%
  mutate(province = str_replace_all(province, "^KS$", "KANSAS")) %>%
  mutate(province = str_replace_all(province, "^KY$", "KENTUCKY")) %>%
  mutate(province = str_replace_all(province, "^KYOTO$", "KANSAI")) %>%
  # L
  mutate(province = str_replace_all(province, "^LA$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "LA,CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "LANCASIRE", "LANCASHIRE")) %>%
  mutate(province = str_replace_all(province, "^LONDOM$", "LONDON")) %>%
  mutate(province = str_replace_all(province, "^LOS ANGELES$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^LOS ANGELES, CA$", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "LOS ANGELES, CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "LOZANGELEEEZZ, CA", "CALIFORNIA")) %>%
  # M
  mutate(province = str_replace_all(province, "^MA$", "MASSACHUSETTS")) %>%
  mutate(province = str_replace_all(province, "MANNHEIM, BADEN-WÜRTTEMBERG", "BADEN-WURTTEMBERG")) %>%
  mutate(province = str_replace_all(province, "MARIETTA, GA", "GEORGIA")) %>%
  mutate(province = str_replace_all(province, "MARYLANDS\\/AFK", "MARYLAND")) %>%
  mutate(province = str_replace_all(province, "^MASS$", "")) %>%
  mutate(province = str_replace_all(province, "^MASSACHUSETS$", "")) %>%
  mutate(province = str_replace_all(province, "MASSACHUSETTS, PLYMOUTH COUNTY", "MASSACHUSETTS")) %>%
  mutate(province = str_replace_all(province, "MASSACHUSSETS", "MASSACHUSETTS")) %>%
  mutate(province = str_replace_all(province, "^MD$", "MARYLAND")) %>%
  mutate(province = str_replace_all(province, "^ME$", "MAINE")) %>%
  mutate(province = str_replace_all(province, "MECKLENBURG COUNTY, NC", "NORTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "^MI$", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "^MICH$", "")) %>%
  mutate(province = str_replace_all(province, "MICHIGAN, SHIAWASSEE COUNTY", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "MICHIGIN", "MICHIGAN")) %>% 
  mutate(province = str_replace_all(province, "MISSOURE", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "MISSOURI BALLWIN USA", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "MISSOURI, MILLER COUNTY", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "^MN$", "MINNESOTA")) %>%
  mutate(province = str_replace_all(province, "^MO$", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "MONROE COUNTU NY", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "^MONROE COUNTY$", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "MONTANA, JUDITH BASIN", "MONTANA")) %>%
  mutate(province = str_replace_all(province, "^MS$", "MISSISSIPPI")) %>%
  mutate(province = str_replace_all(province, "^MT$", "MONTANA")) %>%
  mutate(province = str_replace_all(province, "MULTNOMAH COUNTY, OREGON", "OREGON")) %>%
  # N
  mutate(province = str_replace_all(province, "N-BRABANT", "NORTH BRABANT")) %>%
  mutate(province = str_replace_all(province, "^NB$", "NEW BRUNSWICK")) %>%
  mutate(province = str_replace_all(province, "^NC$", "NORTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "^ND$", "NORTH DAKOTA")) %>%
  mutate(province = str_replace_all(province, "^NE$", "NEBRASKA")) %>%
  mutate(province = str_replace_all(province, "NEW MEXICO, DONA ANA COUNTY", "NEW MEXICO")) %>%
  mutate(province = str_replace_all(province, "NEW MEXICO, USA", "NEW MEXICO")) %>%
  mutate(province = str_replace_all(province, "NEW ORLEANS LA", "LOUISIANA")) %>%
  mutate(province = str_replace_all(province, "NEW YORK  M", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "NEW YORK COUNTY, NEW YORK CITY, NEW YORK STATE", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "NEW YORK, NEW YORK", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "NEW Y0RK", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "NEW YORK, NY", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "NEWCASTLE", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "NEWFOUNDLAND AND LABRADOR", "NEWFOUNDLAND")) %>%
  mutate(province = str_replace_all(province, "^NH$", "NEW HAMPSHIRE")) %>%
  mutate(province = str_replace_all(province, "^NJ$", "NEW JERSEY")) %>%
  mutate(province = str_replace_all(province, "NJ, GLOUCESTER", "NEW JERSEY")) %>%
  mutate(province = str_replace_all(province, "NJ; ESSEX", "NEW JERSEY")) %>%
  mutate(province = str_replace_all(province, "^NL$", "NEWFOUNDLAND")) %>%
  mutate(province = str_replace_all(province, "^NM$", "NEW MEXICO")) %>%
  mutate(province = str_replace_all(province, "NOORD-BRABANT", "NORTH BRABANT")) %>%
  mutate(province = str_replace_all(province, "NOORD-HOLLAND", "NORTH HOLLAND")) %>%
  mutate(province = str_replace_all(province, "NORTHAMPTON COUNTY, PA", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "NORTHBROOK, IL", "ILLINOIS")) %>%
  mutate(province = str_replace_all(province, "^NOYB$", "NEW BRUNSWICK")) %>%
  mutate(province = str_replace_all(province, "^NS$", "NOVA SCOTIA")) %>%
  mutate(province = str_replace_all(province, "^NSW$", "NEW SOUTH WALES")) %>%
  mutate(province = str_replace_all(province, "^NV$", "NEVADA")) %>%
  mutate(province = str_replace_all(province, "^NY$", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "^NY, KINGS COUNTY \\(BROOKLYN\\)$", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "^NY, PUTNAM$", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "NY, TOMPKINS COUNTY", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "NY, ULSTER COUNTY", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "NY,ONEIDA", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "NY\\/NYC", "NEW Y0RK")) %>%
  mutate(province = str_replace_all(province, "^NYC$", "NEW Y0RK")) %>%
  # O
  mutate(province = str_replace_all(province, "OAKLAND COUNTY, MICHIGAN", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "OAKLAND, CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "OAKLAND,CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^OH$", "OHIO")) %>%
  mutate(province = str_replace_all(province, "OHIO, ATHENS", "OHIO")) %>%
  mutate(province = str_replace_all(province, "^OK$", "OKLAHOMA")) %>%
  mutate(province = str_replace_all(province, "OKLAHOMA, ROGERS COUNTY", "OKLAHOMA")) %>%
  mutate(province = str_replace_all(province, "^ON$", "ONTARIO")) %>%
  mutate(province = str_replace_all(province, "^ONT$", "ONTARIO")) %>%
  mutate(province = str_replace_all(province, "ONTARIO,  CANADA", "ONTARIO")) %>%
  mutate(province = str_replace_all(province, "^OR$", "OREGON")) %>%
  mutate(province = str_replace_all(province, "ORANGE COUNTY, CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "OREGON - BUT WILL BE IN WASHINGTON FOR HALLOWEEN", "OREGON")) %>%
  mutate(province = str_replace_all(province, "OREGON, MULTNOMAH COUNTY", "OREGON")) %>%
  mutate(province = str_replace_all(province, "^ORLANDO, FL$", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "ORYGUN", "OREGON")) %>%
  mutate(province = str_replace_all(province, "OSHAWA, ONTARIO, CANADA", "ONTARIO")) %>%
  mutate(province = str_replace_all(province, "OXFORD, OXFORDSHIRE, ENGLAND", "OXFORDSHIRE")) %>%
  # P
  mutate(province = str_replace_all(province, "^PA$", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PA.   NORTHAMPTON COUNTY", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PENNSYLVANIA, ALLEGHENY", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PENNSYLVANIA, MONTGOMERY COUNTY", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PENNSYLVANIA, NORTHAMPTON COUNY, EASTON", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PEOPLES REPUBLIC OF KALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^PHILADELPHIA$", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PHILADELPHIA PA", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PHILADELPHIA, PA", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PHOENIX, MARICOPA COUNTY, ARIZONA", "ARIZONA")) %>%
  mutate(province = str_replace_all(province, "PITTSBURGH PA", "PENNSYLVANIA")) %>%
  mutate(province = str_replace_all(province, "PORTLAND, MAINE", "MAINE")) %>%
  mutate(province = str_replace_all(province, "^PR$", "PUERTO RICO"))
  

# Now for the next 4 letters, Q-T

candy_full <- candy_full %>%
  # Q
  mutate(province = str_replace_all(province, "^QC$", "QUEBEC")) %>%
  mutate(province = str_replace_all(province, "QUÉBEC", "QUEBEC")) %>%
  mutate(province = str_replace_all(province, "QUEBEC, CANADA", "QUEBEC")) %>%
  mutate(province = str_replace_all(province, "QUENEC", "QUEBEC")) %>%
  mutate(province = str_replace_all(province, "^QLD$", "QUEENSLAND")) %>%
  mutate(province = str_replace_all(province, "^QUEENS$", "NEW YORK")) %>%
  # R
  mutate(province = str_replace_all(province, "RAPPAHANNOCK COUNTY, VA", "VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "RENTON,WA", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^RI$", "RHODE ISLAND")) %>%
  # S
  mutate(province = str_replace_all(province, "SAN BERNADINO COUNTY, CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "SAN DIEGO, CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "SAN FRANCISCO, CA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "SANTA BARBARA CO. CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "SANTA CLARA COUNTY, CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "SANTA CRUZ COUNTY, CALIFORNIA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "^SC$", "SOUTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "^SEOUL$", "SEOUL CAPITAL")) %>%
  mutate(province = str_replace_all(province, "^SD$", "SOUTH DAKOTA")) %>%
  mutate(province = str_replace_all(province, "^SK$", "SASKATCHEWAN")) %>%
  mutate(province = str_replace_all(province, "ST LOUIS MO", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "ST. AUGUSTINE FLORIDA", "FLORIDA")) %>%
  mutate(province = str_replace_all(province, "ST. JOHN'S, NL", "NEWFOUNDLAND")) %>%
  mutate(province = str_replace_all(province, "ST. LOUIS COUNTY, MISSOURI", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "ST. LOUIS, MO", "MISSOURI")) %>%
  mutate(province = str_replace_all(province, "SUFFOLK COUNTY NY", "NEW YORK")) %>%
  # T
  mutate(province = str_replace_all(province, "TARRANT COUNTY, TEXAS", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "TAXACHUSETTS", "MASSACHUSETTS")) %>%
  mutate(province = str_replace_all(province, "TEHAMA", "CALIFORNIA")) %>%
  mutate(province = str_replace_all(province, "TEXAS, FORT BEND", "")) %>%
  mutate(province = str_replace_all(province, "TEXAS, HARRIS COUNTY", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "TEXAS, TRAVIS COUNTY", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "THE DEMOCRATIC PEOPLE'S REPUBLIC OF NORTH CAROLINA", "NORTH CAROLINA")) %>%
  mutate(province = str_replace_all(province, "THE MITTEN", "MICHIGAN")) %>%
  mutate(province = str_replace_all(province, "THE SHORE", "NEW JERSEY")) %>%
  mutate(province = str_replace_all(province, "^TN$", "TENNESSEE")) %>%
  mutate(province = str_replace_all(province, "TENNESSEE, KNOX COUNTY", "TENNESSEE")) %>%
  mutate(province = str_replace_all(province, "TORONTO, ONTARIO", "ONTARIO")) %>%
  mutate(province = str_replace_all(province, "TREE TOWN, USA", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "^TX$", "TEXAS")) %>%
  mutate(province = str_replace_all(province, "^TX.$", "TEXAS"))


# Final 6 letters, U-Z
candy_full <- candy_full %>% 
  # U
  mutate(province = str_replace_all(province, "ULSTER COUNTY, NY", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "ULSTER, NY", "NEW YORK")) %>%
  mutate(province = str_replace_all(province, "^UT$", "UTAH")) %>%
  mutate(province = str_replace_all(province, "UT, SALT LAKE", "UTAH")) %>%
  # V
  mutate(province = str_replace_all(province, "^VA$", "VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "VANCOUVER, WA", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "VANCOUVER, BC", "BRITISH COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "VIRGINIA, ARLINGTON", "VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "^VT$", "VERMONT")) %>%
  mutate(province = str_replace_all(province, "VT, CHITTENDEN", "VERMONT")) %>%
  # W
  mutate(province = str_replace_all(province, "^WA$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WA, SPOKANE", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTO$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON DC", "DISTRICT OF COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON STATE$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON STATE, KING COUNTY$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON STATE, KING COUNTY, CITY OF SEATTLE", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON STATE, KITSAP COUNTY", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON, CLARK", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON, DC", "DISTRICT OF COLUMBIA")) %>%
  mutate(province = str_replace_all(province, "WASHINGTON, ISLAND COUNTY", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON, KING$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON, KING COUNTY$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON, KITSAP COUNTY$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WASHINGTON, SEATTLE, KING$", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "WHATCOM COUNTY, WA", "WASHINGTON")) %>%
  mutate(province = str_replace_all(province, "^WI$", "WISCONSIN")) %>%
  mutate(province = str_replace_all(province, "WISCONISIN", "WISCONSIN")) %>%
  mutate(province = str_replace_all(province, "^WV$", "WEST VIRGINIA")) %>%
  mutate(province = str_replace_all(province, "WV, KANAWHA COUNTY", "WEST VIRGINIA")) %>% 
  mutate(province = str_replace_all(province, "ZUID HOLLAND", "SOUTH HOLLAND")) %>% 
  mutate(province = str_replace_all(province, "ZÜRICH", "ZURICH")) %>% 
  mutate(province = str_replace_all(province, "SÃO PAULO", "SAO PAULO"))
  




candy_full %>%
  distinct(province) %>%
  arrange(province) %>%
  view()

view(candy_full)
