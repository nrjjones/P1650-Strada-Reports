library(haven)
library(survey)
library(tidyverse)

df <- read_spss("./out/P1650 Harmonized 2022 Data Grad Responses.sav") %>% as_factor()
dict <- read_csv("./out/P1650 Harmonized Vars Clean w UT.csv") %>% arrange(order2)

dict <- dict %>% select(src, var = uvar, vars, order)

table(df$q7)
prt <- df %>% filter(q7 == "False")

table(prt$q5)

prt <- prt %>%
  mutate(csid = as.numeric(row_number())) %>%
  mutate_all(as.character) %>%
  select(csid, src, everything()) %>%
  pivot_longer(
    -c(csid, src),
    values_to = "val",
    names_to = "var"
  ) %>%
  group_by(csid) %>%
  filter(val != "") %>%
  filter(!is.na(val))

prt <- left_join(prt, dict)

prt <- prt %>%
  mutate(csid = as.numeric(csid)) %>%
  arrange(csid, order) %>%
  filter(vars != "school") %>%
  group_by(csid) %>%
  mutate(n=n()) %>%
  filter(n==row_number())

prt <- as.data.frame.matrix(table(prt$var, prt$src)) %>%
  rownames_to_column("last_q") %>%
  mutate(
    order = str_remove(last_q, "q"),
    order = as.numeric(order)
    ) %>%
  arrange(order)

dict <- read_csv("./out/P1650 Harmonized Vars Clean w UT.csv") %>% arrange(order2)

dict <- dict %>% select(last_q = uvar, label) %>% unique()

prt <- left_join(prt, dict) %>% select(last_q, label, everything())

write_csv(prt, "./out/P1650 Partials Break Off Question.csv", na="")
