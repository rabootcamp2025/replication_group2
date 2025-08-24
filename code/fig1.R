# ライブラリ -------------------------------------------------------------------
library(pacman) 
pacman::p_load(tidyverse, haven, fixest, broom)


# データの読み込み ----------------------------------------------------------------

df_cleaned <- readRDS("data/cleaned/df_cleaned.rds")



# 固定効果モデル -----------------------------------------------------------------

df_cleaned_artist <- df_cleaned %>% 
  filter(artist == 1) 


formula_vars <- names(df_cleaned)[str_detect(names(df_cleaned), "year_after_death_")]
formula_vars <- formula_vars[c(2:11, 51:56)]

formula <- as.formula(
  paste("reputation ~ ",
        paste(formula_vars, collapse = " + ") ,"|id_final"
    )
  )

model <- feols(
  formula,
  data = df_cleaned_artist,
  cluster = "id_final"
)
summary




# プロット --------------------------------------------------------------------

base_year <- tibble(
  term = "year_after_death_0",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 0,
  year = 0
)

# tidyを使ってプロットする
model %>% 
  tidy() %>% 
  mutate(
    year = case_when(
      str_detect(term, "year_after_death_m") ~ paste0("-", str_extract(term, "\\d+")),
      TRUE ~ str_extract(term, "\\d+")
    )
  ) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year, y = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line()

test <- model %>% 
  tidy() %>% 
  mutate(
    year = case_when(
      str_detect(term, "year_after_death_m") ~ paste0("-", str_extract(term, "\\d+")),
      TRUE ~ str_extract(term, "\\d+")
    )
  ) %>% 
  mutate(year = as.numeric(year)) %>% 
  rbind(base_year) %>% 
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

test %>% 
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_line()+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) + 
  theme_classic() +
  labs(
    title = "All Artists",
    x = "Years after Death",
    y = "Estimated Reputation"
  ) +
  scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 2))

# yearというカラムを作成する
# casewhenでyear_after_death_から始まる場合はそのままの数をyearとする。
# year_after_death_mから始まる場合は、-1をかけた数をyearとする


# 死亡時




