
# library -----------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, haven)


# load data ---------------------------------------------------------------

df <- read_dta("data/raw/artist_reputation_main.dta")


# clean data --------------------------------------------------------------

df_cleaned <- df %>% 
  filter(sample == 1)


# save data ---------------------------------------------------------------

saveRDS(df_cleaned, "data/cleaned/df_cleaned.rds")

