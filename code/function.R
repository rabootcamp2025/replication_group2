gc()
gc()

# ライブラリの読み込み --------------------------------------------------------------
library(pacman) 
pacman::p_load(tidyverse, haven, fixest, broom)


# load data ---------------------------------------------------------------

#データ
df <- read_dta("data/raw/artist_reputation_main.dta")

df_cleaned4 <- df %>% 
  mutate(sample4 = case_when(
    year_after_death > -40 & year_after_death < 220 & artist == 1 & pct_dead < 1 & pct_dead > 0 ~ 1,
    name == "The Rev" | name == "Dr. Seuss" | (name == "Michael Jackson" & occupation == "WRITER") ~ 0,
    TRUE ~ 0
  )) %>% 
  filter(sample4 == 1)


df_cleaned <- readRDS("data/cleaned/df_cleaned.rds")


formula_vars <- names(df_cleaned)[str_detect(names(df_cleaned), "year_after_death_")]
formula_vars <- formula_vars[c(2:21, 51:56)]

formula <- as.formula(
  paste("reputation ~ ",
        paste(formula_vars, collapse = " + ") ,"|id_final"
  )
)

base_year <- tibble(
  term = "year_after_death_0",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 0,
  year = 0
)

# function1 ---------------------------------------------------------------

function1 <- function(df, group, title_text){
  
  df_function1 <- df %>% filter({{group}} == 1)
  
  model <- feols(
    formula,
    data = df_function1,
    cluster = "id_final"
  )
  
  model %>% 
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
    ) %>% 
    ggplot(aes(x = year, y = estimate)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) + 
    theme_classic() +
    labs(
      title = title_text,
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 2))
}

function1(df_cleaned, artist, "All Artists")



# function2 ---------------------------------------------------------------

function2 <- function(df){
  
  df_function2_1 <- df %>% filter(reputation_initial < -0.32)
  
  model1 <- feols(
    formula,
    data = df_function2_1,
    cluster = "id_final"
  ) %>% 
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
      estimate = estimate - 0.85, 
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error,
      group = "Lower Initial Reputation"
    ) 
  
  df_function2_2 <- df %>% filter( reputation_initial> 1.1)
  
  model2 <- feols(
    formula,
    data = df_function2_2,
    cluster = "id_final"
  ) %>% 
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
      estimate = estimate + 1.58, 
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error,
      group = "Higher Initial Reputation"
    ) 
  
  model3<-rbind(model1,model2)
  
  model3 %>%
    ggplot(aes(x = year, y = estimate, group = group, color = group)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) +
    theme_classic() +
    labs(
      title = "Sorted by Initial Reputation",
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
    scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 6))
}


# function3 ---------------------------------------------------------------
function3_a <- function(df){
  
  df_function3_1 <- df %>% filter(age_death < 41)
  
  model1 <- feols(
    formula,
    data = df_function3_1,
    cluster = "id_final"
  ) %>% 
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
      upper = estimate + 1.96 * std.error,
      group = "Age of Death < 40"
    ) 
  
  df_function3_2 <- df %>% filter(age_death > 70 & !is.na(age_death))
  
  model2 <- feols(
    formula,
    data = df_function3_2,
    cluster = "id_final"
  ) %>% 
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
      upper = estimate + 1.96 * std.error,
      group = "Age of Death > 70"
    ) 
  
  model3<-rbind(model1,model2)
  
  model3 %>%
    ggplot(aes(x = year, y = estimate, group = group, color = group)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) +
    theme_classic() +
    labs(
      title = "Sorted by Age of Death",
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 2))
}

function3_b <- function(df){
  
  df_function3_1 <- df %>% filter(expected_death == 4)
  
  model1 <- feols(
    formula,
    data = df_function3_1,
    cluster = "id_final"
  ) %>% 
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
      upper = estimate + 1.96 * std.error,
      group = "Unexpected Death"
    ) 
  
  df_function3_2 <- df %>% filter(expected_death == 2)
  
  model2 <- feols(
    formula,
    data = df_function3_2,
    cluster = "id_final"
  ) %>% 
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
      upper = estimate + 1.96 * std.error,
      group = "Natural Death"
    ) 
  
  model3<-rbind(model1,model2)
  
  model3 %>%
    ggplot(aes(x = year, y = estimate, group = group, color = group)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) +
    theme_classic() +
    labs(
      title = "Sorted by Nature of Death",
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
    scale_y_continuous(limits = c(-1.3, 1), breaks = seq(-1, 1, 2))
}


# function4 ---------------------------------------------------------------


delete_id <- df_cleaned4 %>% 
  group_by(id_final) %>% 
  summarise(sum = sum(valid, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(sum < 3) %>% 
  select(id_final) %>% 
  pull()

# drop famous_after_death
# gen famous_after_death=1 if pct_dead>0 & pct_dead<1 
# replace famous_after_death=0 if news_occurrence>4 & nondead==1
# gsort id_final famous_after_death
# by id_final: replace famous_after_death=famous_after_death[1]
# 
# bys id_final: egen delete=total(valid)
# replace famous_after_death=. if delete<3
# drop delete

df_cleaned4 <- df_cleaned4 %>%
  mutate(
    famous_after_death = case_when(
      pct_dead > 0 & pct_dead < 1 ~ 1,
      news_occurrence > 4 & nondead == 1 ~ 0,
      id_final %in% delete_id ~ NA,
      TRUE ~ NA
    )
  )


function4 <- function(df){
  
  df_function4 <- df %>% filter(famous_after_death == 1)
  
  model <- feols(
    formula,
    data = df_function4,
    cluster = "id_final"
  )
  
  model %>% 
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
    ) %>% 
    ggplot(aes(x = year, y = estimate)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) + 
    theme_classic() +
    labs(
      title = "Famous Only After Death",
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    scale_x_continuous(limits = c(-1, 201), breaks = seq(0, 200, 20)) +
    scale_y_continuous(limits = c(-1.5, 1), breaks = seq(-1, 1, 2))
}
  
# 確認用

df_function4 <- df_cleaned4 %>% filter(famous_after_death == 1)

model <- feols(
  formula,
  data = df_function4,
  cluster = "id_final"
)

model %>% 
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
  ) %>% 
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_line()+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) + 
  theme_classic() +
  labs(
    title = "Famous Only After Death",
    x = "Years after Death",
    y = "Estimated Reputation"
  ) +
  scale_x_continuous(limits = c(-1, 201), breaks = seq(0, 200, 20)) +
  scale_y_continuous(limits = c(-2, 1), breaks = seq(-1, 1, 2))


# function5 ---------------------------------------------------------------

# Figure 5(a)-5(b)
# drop valid_newspaper
# gen valid_newspaper=(newspaper_count~=.)
# replace valid_newspaper=0 if deathyear>2010
# gsort -valid_newspaper -artist -year2 -newspaper_count
# tab year2
# gen delete=0
# replace delete=1 if _n<61
# 
# gsort id_final -delete
# by id_final: replace delete=delete[1]



vec5_1 <- df_cleaned %>% 
  filter(!is.na(id_final)) %>% 
  mutate(valid_newspaper = ifelse(is.na(newspaper_count)|deathyear > 2010 , 0, 1)) %>% 
  arrange(desc(valid_newspaper), desc(artist), desc(year2), desc(newspaper_count)) %>% 
  head(50) %>% 
  select(id_final) %>% 
  pull()

df_cleaned5_1 <- df_cleaned %>% 
  filter(id_final %in% vec5_1)

vec5_test <- df %>% 
  mutate(valid_newspaper = ifelse(is.na(newspaper_count)|deathyear > 2010 , 0, 1)) %>% 
  arrange(desc(valid_newspaper), desc(artist), desc(year2), desc(newspaper_count)) %>% 
  head(60) %>% 
  filter(sample == 1) %>% 
  select(id_final) %>% 
  pull() 

df_cleaned5_test <- df_cleaned %>% 
  filter(id_final %in% vec5_test)

vec5_2 <- df_cleaned %>% 
  mutate(valid_newspaper = ifelse(is.na(newspaper_count) | deathyear > 2010, 0, 1)) %>%
  arrange(desc(valid_newspaper), desc(artist), desc(year2), desc(newspaper_count)) %>% 
  head(546) %>% 
  select(id_final) %>%
  pull()

df_cleaned5_2 <- df_cleaned %>% 
  filter(id_final %in% vec5_2)

function5 <- function(df,title_text){
  
  model <- feols(
    formula,
    data = df,
    cluster = "id_final"
  )
  
  model %>% 
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
    ) %>% 
    ggplot(aes(x = year, y = estimate)) +
    geom_point() +
    geom_line()+
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 2) + 
    theme_classic() +
    labs(
      title = title_text,
      x = "Years after Death",
      y = "Estimated Reputation"
    ) +
    scale_x_continuous(limits = c(-31, 101), breaks = seq(-30, 90, 20)) +
    scale_y_continuous(limits = c(-1, 1.5), breaks = seq(-1, 1, 0.5))
}
  


