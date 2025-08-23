# ライブラリ
library(pacman)
pacman::p_load(tidyverse, haven)

library(ggplot2)

#データ
df <- read_dta("data/raw/artist_reputation_main.dta")


summary(df)
print(unique(df$occupation))

# other_artistの確認
df %>%
  filter(
    other_artist == 1)%>%
  select(
    occupation
  )%>%
  distinct()
# 感想：チャップリンが入っている。

names(df)

#アーテイストを見る

df %>%
  filter(str_detect(name, "Vincent")) %>%
  View()


# 
df_cleaned 