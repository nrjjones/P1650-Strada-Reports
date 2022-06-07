library(haven)
library(tidyverse)
library(janitor)

df <- read_spss("./out/P1650 Harmonized 2022 Data Grad Responses.sav") %>% as_factor()

tabyl(df, q274, src) %>% adorn_percentages("col") %>% adorn_pct_formatting() %>% adorn_ns()
tmp <- tabyl(df, q272_cat, src) %>% adorn_percentages("col") %>% adorn_pct_formatting() %>% adorn_ns()

tmp1 <- tmp[c(1:5)]
tmp2 <- tmp[c(1, 6:10)]
tmp3 <- tmp[c(1, 11:16)]

tmp1
tmp2
tmp3

tmp <- tabyl(df, q274, src) %>% adorn_percentages("col") %>% adorn_pct_formatting() %>% adorn_ns()

tmp1 <- tmp[c(1:5)]
tmp2 <- tmp[c(1, 6:10)]
tmp3 <- tmp[c(1, 11:16)]

tmp1
tmp2
tmp3

gender_recode <- df %>% select(src, q273) %>% filter(q273 != "") %>% unique()

age_recode <- df %>% select(src, q272) %>% filter(q272 != "")%>%
  mutate(q272_rec = as.numeric(q272)) %>%
  filter(is.na(q272_rec)) %>% unique()

write_csv(gender_recode, "./out/P1650 Gender Recode.csv", na="")
write_csv(age_recode, "./out/P1650 Age Recode.csv", na="")



