# We start with calling in all the packages we need, and then reading in the raw data.
library(tidyverse)
library(janitor)
decathlon <- read_rds("raw_data/decathlon.rds")

# Row names are moved to a column
decathlon <- tibble::rownames_to_column(decathlon, "names")


# First all the characters in 'names' are converted to upper case for consistency, then clean_names from janitor converts our column names to snake_case, then our javeline column has it's spelling fixed
decathlon <- decathlon %>%
  mutate(names = toupper(names))

decathlon <- decathlon %>%
  clean_names()
  
decathlon <- decathlon %>% 
  rename(javelin = javeline)
  

  
 
# We write our cleaned data to a new csv file in our clean_data folder
write_csv(x = decathlon, file = "clean_data/decathlon_cleaned.csv")
