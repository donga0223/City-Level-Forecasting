library(dplyr)
library(tidyr)
library(readxl)

zip_to_zcta <- read_xlsx("/Users/dk29776/Dropbox/UTAustin/Forecasting/auxiliary-data/ZIP Code to ZCTA Crosswalk.xlsx")
tx_population1 <- read.csv("/Users/dk29776/Dropbox/UTAustin/Forecasting/auxiliary-data/TX_zip_pop20_allcation.csv")

zip_to_zcta_TX <- zip_to_zcta %>%
  filter(STATE == "TX")

tx_population2 <- tx_population1 %>%
  slice(-c(1,2,3)) %>%
  mutate(ZIPName = str_remove(ZIPName, "\\(PO boxes\\)")) %>%  # Removes only "(PO boxes)" text
  mutate(ZIPName = str_trim(ZIPName)) %>%  # Removes extra spaces
  # Separate City and State
  separate(ZIPName, into = c("City", "State"), sep = ", ", fill = "right", extra = "merge") %>% 
  select(zcta, City, State, pop20, afact)  


tx_population3 <- tx_population2 %>%
  group_by(zcta, City, State) %>%
  summarise(population = sum(as.numeric(pop20)*as.numeric(afact)),
            afact = sum(as.numeric(afact)))


tx_population4 <- tx_population3 %>%
  filter(State == "TX") %>%
  select(zcta, City, population) 


tx_population <- tx_population4 %>%
  left_join(zip_to_zcta_TX, by = "zcta") %>%
  select(-ZIP_TYPE, -zip_join_type)



write.csv(tx_population, "/Users/dk29776/Dropbox/UTAustin/Forecasting/auxiliary-data/TX_zip_zcta_pop_city.csv", row.names = FALSE)

