library(haven)
library(survey)
library(tidyverse)

df <- read_spss("./out/P1650 Harmonized 2022 Data Grad Responses.sav") %>% as_factor()

fr <- read_csv("./raw/SOS 2022 School Email Send Schedule - IPEDS completion data (pulled 4-26-2022).csv")

fr <- fr %>%
  select(
    school = `Institution Name`,
    contains(c("men", "women", "Ages"))
    )

nms <- tibble(orig = names(fr)) %>%
  mutate(
    year = str_extract(orig, "[0-9]{4}"),
    var = str_extract(orig, "women|men|Ages 18-24|Ages 25-39|Ages 40 and above"),
    var = case_when(
      var == "women" ~ "Woman",
      var == "men" ~ "Man",
      var == "Ages 18-24" ~ "age18_24",
      var == "Ages 25-39" ~ "age25_39",
      var == "Ages 40 and above" ~ "age40p"
    ),
    new = paste0(year, "_", var)
  ) %>%
  filter(!is.na(var))

fr <- fr %>% select(school, nms$orig)
names(fr) <- c("school", nms$new)

fr <- fr %>%
  pivot_longer(
    -school,
    names_to = "var",
    values_to = "val"
  ) %>%
  mutate(
    year = str_extract(var, "[0-9]{4}"),
    var = str_remove(var, "[0-9]{4}_"),
    per = case_when(
      year >= 2012 & year < 2016 ~ "Early",
      year >= 2016 ~ "Late"
      )
    ) %>%
  filter(!is.na(per)) %>%
  group_by(school, per, var) %>%
  summarize(
    val = sum(val, na.rm=TRUE)
  )


fr$src <- case_when(
  fr$school == "Arizona State University Campus Immersion" ~ "AZ State",
  fr$school == "Arizona State University Digital Immersion" ~ "AZ State",
  fr$school == "New York University" ~ "NYU",
  fr$school == "Pace University" ~ "Pace U",
  fr$school == "Southern New Hampshire University" ~ "SNHU",
  fr$school == "The University of Montana" ~ "U Montana",
  fr$school == "The University of Texas at Arlington" ~ "UT Arlington",
  fr$school == "The University of Texas at Dallas" ~ "UT Dallas",
  fr$school == "The University of Texas at San Antonio" ~ "UT San Antonio",
  fr$school == "University of Arizona" ~ "U Arizona",
  fr$school == "University of Arizona-Sierra Vista" ~ "U Arizona",
  fr$school == "University of Colorado Denver/Anschutz Medical Campus" ~ "CU Denver",
  str_detect(fr$school, "Hawaii") ~ "U Hawaii",
  fr$school == "University of Minnesota-Rochester" ~ "UMN Rochester",
  fr$school == "University of Minnesota-Twin Cities" ~ "UMN Twin Cities",
  str_detect(fr$school, "Pittsburgh") ~ "U Pittsburgh",
  fr$school == "Western Governors University" ~ "WGU"
)

fr2 <- fr %>%
  filter(val!=0) %>%
  group_by(src, var) %>%
  #group_by(src, per, var) %>%
  summarize(val = sum(val))  %>%
  mutate(
    var2 = case_when(
      str_detect(var, "age") ~ "age",
      TRUE ~ "gender"
    )
  ) %>%
  select(-var2)


fr2_sum <- fr %>%
  filter(val!=0) %>%
  group_by(src, per, var) %>%
  summarize(val = sum(val))  %>%
  mutate(
    var2 = case_when(
      str_detect(var, "age") ~ "age",
      TRUE ~ "gender"
    )
  ) %>%
  group_by(src, per, var2) %>%
  mutate(
    perc = round(val/sum(val)*100, 2)
  ) %>%
   arrange(src, var)


fr3 <- fr2_sum %>%
  select(-val) %>%
  #select(-val, -var2) %>%
  pivot_wider(
    names_from = per,
    values_from = perc
  ) %>%
  mutate(
    diff = Late - Early
  ) %>%
  arrange(src, var2)

#write_csv(fr3, "./out/P1650 Age Gender Changes.csv")

fr2 <- fr2 %>% filter(!(src == "UMN Rochester" & var == "age40p"))
#fr2 <- fr2 %>% filter(!(src == "UMN Twin Cities" & var == "age18_24"))

df.w <- df %>%
  filter(src == "AAA National") %>%
  mutate(w2 = 1)

### UPDATE WITH NORC WEIGHTS ###

#df.w1 <- svydesign(
#  id= ~1,
#  weights = ~SAMPLINGWEIGHT,
#  data = tmp)
#
#q274.dist <- fr2 %>%
#  ungroup() %>%
#  filter(src == i) %>%
#  filter(var %in% c("Man", "Woman")) %>%
#  select(-src) %>%
#  select(q274 = var, Freq = val)
#
#q272.dist <- fr2 %>%
#  ungroup() %>%
#  filter(src == i) %>%
#  filter(str_detect(var, "age")) %>%
#  select(-src) %>%
#  select(q272_cat = var, Freq = val)
#
#df.w2 <- rake(
#  design = df.w1,
#  sample.margins = list(~q274, ~q272_cat),
#  population.margins = list(q274.dist, q272.dist)
#)
#
#tmp2$w2 <- weights(df.w2)



for(i in unique(df$src)[2:16]) {

  print(i)

  tmp <- df %>%
    filter(src == i) %>%
    filter(q274 %in% c("Man", "Woman")) %>%
    filter(!is.na(q272_cat)) %>%
    filter(q272_cat != "") %>%
    droplevels()

  df.w1 <- svydesign(
    id= ~1,
    #strata = per,
    #weights = ~SAMPLINGWEIGHT,
    data = tmp)

  q274.dist <- fr2 %>%
    ungroup() %>%
    filter(src == i) %>%
    filter(var %in% c("Man", "Woman")) %>%
    select(-src) %>%
    select(q274 = var, Freq = val)

  q272.dist <- fr2 %>%
    ungroup() %>%
    filter(src == i) %>%
    filter(str_detect(var, "age")) %>%
    select(-src) %>%
    select(q272_cat = var, Freq = val)

  df.w2 <- rake(
    design = df.w1,
    sample.margins = list(~q274, ~q272_cat),
    population.margins = list(q274.dist, q272.dist)
  )

  tmp$w2 <- weights(df.w2)

  df.w <- bind_rows(df.w, tmp)
}

nat <- df.w %>% filter(src == "AAA National")
sch1 <- df.w %>% filter(src == "AZ State")
sch2 <- df.w %>% filter(src == "CU Denver")
sch3 <- df.w %>% filter(src == "U Montana")
sch4 <- df.w %>% filter(src == "UMN Rochester")
sch5 <- df.w %>% filter(src == "UMN Twin Cities")
sch6 <- df.w %>% filter(src == "NYU")
sch7 <- df.w %>% filter(src == "Pace U")
sch8 <- df.w %>% filter(src == "U Pittsburgh")
sch9 <- df.w %>% filter(src == "SNHU")
sch10 <- df.w %>% filter(src == "U Arizona")
sch11 <- df.w %>% filter(src == "U Hawaii")
sch12 <- df.w %>% filter(src == "UT Arlington")
sch13 <- df.w %>% filter(src == "UT Dallas")
sch14 <- df.w %>% filter(src == "UT San Antonio")
sch15 <- df.w %>% filter(src == "WGU")

nat.w  <- svydesign(id= ~1,weights = ~w2, data = nat)
sch1.w <- svydesign(id= ~1,weights = ~w2, data = sch1)
sch2.w <- svydesign(id= ~1,weights = ~w2, data = sch2)
sch3.w <- svydesign(id= ~1,weights = ~w2, data = sch3)
sch4.w <- svydesign(id= ~1,weights = ~w2, data = sch4)
sch5.w <- svydesign(id= ~1,weights = ~w2, data = sch5)
sch6.w <- svydesign(id= ~1,weights = ~w2, data = sch6)
sch7.w <- svydesign(id= ~1,weights = ~w2, data = sch7)
sch8.w <- svydesign(id= ~1,weights = ~w2, data = sch8)
sch9.w <- svydesign(id= ~1,weights = ~w2, data = sch9)
sch10.w <-svydesign(id= ~1,weights = ~w2, data = sch10)
sch11.w <-svydesign(id= ~1,weights = ~w2, data = sch11)
sch12.w <-svydesign(id= ~1,weights = ~w2, data = sch12)
sch13.w <-svydesign(id= ~1,weights = ~w2, data = sch13)
sch14.w <-svydesign(id= ~1,weights = ~w2, data = sch14)
sch15.w <-svydesign(id= ~1,weights = ~w2, data = sch15)


#prop.table(xtabs(~q272_cat+q274, data=tmp),2)
#prop.table(svytable(~q272_cat+q274, df.w1),2)
#prop.table(svytable(~q272_cat+q274, df.w2),2)

#summary(tmp$w2)

#df.w_deid <- df.w %>% select(-c(q1:q17)) %>% select(-c(q259:q261))

#write_sav(df.w_deid, "./out/P1650 INTERIM Combined Weighted Responses.sav")
#write_csv(df.w_deid, "./out/P1650 INTERIM Combined Weighted Responses.csv", na="")
