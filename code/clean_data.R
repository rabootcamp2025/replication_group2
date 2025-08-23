# ライブラリ
library(pacman)
pacman::p_load(tidyverse, haven)

library(ggplot2)

#データ
df <- read_dta("data/raw/artist_reputation_main.dta")


# データの整形
# sample = 1のみ
df_cleaned <- df %>% 
  filter(sample == 1)

#データの保存
saveRDS(df_cleaned, "data/cleaned/df_cleaned.rds")
