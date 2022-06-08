library(tidyverse)

source("./load_data.R")

dict <- read_csv("./out/P1650 Harmonized Vars Clean w UT.csv") %>% arrange(order2)

nat_dict <- dict %>% filter(src=="National")
nat_l <- nat_dict$vars
nat <- nat %>% select(nat_l)
names(nat) <- nat_dict$uvar
nat <- as_factor(nat)
nat$src <- "AAA National"

azs_dict <- dict %>% filter(src=="AZ State") %>% arrange(order)
azs_l <- azs_dict$vars
azs <- azs %>% select(azs_l)
names(azs) <- azs_dict$uvar
azs <- as_factor(azs)
azs$src <- "AZ State"

cud_dict <- dict %>% filter(src=="CU Denver") %>% arrange(order)
cud_l <- cud_dict$vars
cud <- cud %>% select(cud_l)
names(cud) <- cud_dict$uvar
cud <- as_factor(cud)
cud$src <- "CU Denver"
mnt_dict <- dict %>% filter(src=="U Montana") %>% arrange(order)
mnt_l <- mnt_dict$vars
mnt <- mnt %>% select(mnt_l)
names(mnt) <- mnt_dict$uvar
mnt <- as_factor(mnt)
mnt$src <- "U Montana"

mro_dict <- dict %>% filter(src=="UMN Rochester") %>% arrange(order)
mro_l <- mro_dict$vars
mro <- mro %>% select(mro_l)
names(mro) <- mro_dict$uvar
mro <- as_factor(mro)
mro$src <- "UMN Rochester"

mro$q129 <- as.numeric(mro$q129)

mtc_dict <- dict %>% filter(src=="UMN Twin Cities") %>% arrange(order)
mtc_l <- mtc_dict$vars
mtc <- mtc %>% select(mtc_l)
names(mtc) <- mtc_dict$uvar
mtc <- as_factor(mtc)
mtc$src <- "UMN Twin Cities"

mtc$q129 <- as.numeric(mtc$q129)

nyu_dict <- dict %>% filter(src=="NYU") %>% arrange(order)
nyu_l <- nyu_dict$vars
nyu <- nyu %>% select(nyu_l)
names(nyu) <- nyu_dict$uvar
nyu <- as_factor(nyu)
nyu$src <- "NYU"

pac_dict <- dict %>% filter(src=="Pace") %>% arrange(order)
pac_l <- pac_dict$vars
pac <- pac %>% select(pac_l)
names(pac) <- pac_dict$uvar
pac <- as_factor(pac)
pac$src <- "Pace U"

pit_dict <- dict %>% filter(src=="U Pittsburgh") %>% arrange(order)
pit_l <- pit_dict$vars
pit <- pit %>% select(pit_l)
names(pit) <- pit_dict$uvar
pit <- as_factor(pit)
pit$src <- "U Pittsburgh"

snhu_dict <- dict %>% filter(src=="SNHU") %>% arrange(order)
snhu_l <- snhu_dict$vars
snhu <- snhu %>% select(snhu_l)
names(snhu) <- snhu_dict$uvar
snhu <- as_factor(snhu)
snhu$src <- "SNHU"

uaz_dict <- dict %>% filter(src=="U Arizona") %>% arrange(order)
uaz_l <- uaz_dict$vars
uaz <- uaz %>% select(uaz_l)
names(uaz) <- uaz_dict$uvar
uaz <- as_factor(uaz)
uaz$src <- "U Arizona"

uhi_dict <- dict %>% filter(src=="U Hawaii") %>% arrange(order)
uhi_l <- uhi_dict$vars
uhi <- uhi %>% select(uhi_l)
names(uhi) <- uhi_dict$uvar
uhi <- as_factor(uhi)
uhi$src <- "U Hawaii"

uta_dict <- dict %>% filter(src=="UT Arlington") %>% arrange(order)
uta_l <- uta_dict$vars
uta <- uta %>% select(uta_l)
names(uta) <- uta_dict$uvar
uta <- as_factor(uta)
uta$src <- "UT Arlington"

utd_dict <- dict %>% filter(src=="UT Dallas") %>% arrange(order)
utd_l <- utd_dict$vars
utd <- utd %>% select(utd_l)
names(utd) <- utd_dict$uvar
utd <- as_factor(utd)
utd$src <- "UT Dallas"

uts_dict <- dict %>% filter(src=="UT San Antonio") %>% arrange(order)
uts_l <- uts_dict$vars
uts <- uts %>% select(uts_l)
names(uts) <- uts_dict$uvar
uts <- as_factor(uts)
uts$src <- "UT San Antonio"

wgu_dict <- dict %>% filter(src=="WGU") %>% arrange(order)
wgu_l <- wgu_dict$vars
wgu <- wgu %>% select(wgu_l)
names(wgu) <- wgu_dict$uvar
wgu <- as_factor(wgu)
wgu$src <- "WGU"

strada <- bind_rows(nat, azs, cud, mnt, mro, mtc, nyu, pac, pit, snhu, uaz, uhi, uta, utd, uts, wgu)

levels(strada$q95) <- c("Arts, design, entertainment, sports, media", "Community and social services", "Legal", "Other")
levels(strada$q96) <- c("Healthcare practitioners and technical workers", "Healthcare support", "Other")
levels(strada$q97) <- c("Personal care and service worker", "Security and protective services", "Building and grounds cleaning", "Food preparation or service", "Other")
levels(strada$q99) <- c("Manufacturing or production worker", "Transportation and material moving worker", "Other")
levels(strada$q128)[7] <- "Some postgraduate, but no postgraduate degree"
levels(strada$q263) <- c("Yes, but no link", "Yes, and ok to link", "No")

# Add some others for gender and age

gender_recode <- read_csv("./out/P1650 Gender Recoded.csv")
age_recode <- read_csv("./out/P1650 Age Recoded.csv")

strada <- left_join(strada, age_recode)
strada <- left_join(strada, gender_recode)

strada <- strada %>%
  mutate(
    q273_rec = case_when(
      q273_rec == "M" ~ "Man",
      q273_rec == "F" ~ "Woman"
    ),
    q272_orig = q272,
    q272 = as.numeric(q272),
    q272_clean = coalesce(q272, q272_rec),
    q274_rec = case_when(
      q274 == "Man" ~ "Man",
      q274 == "Woman" ~ "Woman"
    ),
    q274_clean = coalesce(q274_rec, q273_rec)
  )

#tmp <- strada %>% select(q272_orig, q272, q272_rec, q272_clean, q272_cat) %>%  filter(q272_orig !="")
#tmp <- strada %>% select(q274, q274_rec, q273, q273_rec, q274_clean)

# Filter to grads only

strada <- strada %>%
  filter(
    q194 == "Yes" | src == "AAA National"
  )

# Extra vars

strada <- strada %>%
  mutate(
    q225_rec = case_when(
      q225 == "Prior to 2002" ~ 21,
      q225 == "2002" ~ 20,
      q225 == "2003" ~ 19,
      q225 == "2004" ~ 18,
      q225 == "2005" ~ 17,
      q225 == "2006" ~ 16,
      q225 == "2007" ~ 15,
      q225 == "2008" ~ 14,
      q225 == "2009" ~ 13,
      q225 == "2010" ~ 12,
      q225 == "2011" ~ 11,
      q225 == "2012" ~ 10,
      q225 == "2013" ~ 9,
      q225 == "2014" ~ 8,
      q225 == "2015" ~ 7,
      q225 == "2016" ~ 6,
      q225 == "2017" ~ 5,
      q225 == "2018" ~ 4,
      q225 == "2019" ~ 3,
      q225 == "2020" ~ 2,
      q225 == "2021" ~ 1
    ),
    q272_grad_age = as.numeric(q272_clean) - as.numeric(q225_rec),
    q272_cat = case_when(
      q272_grad_age < 25 ~ "age18_24",
      q272_grad_age >= 25 & q272_grad_age < 40 ~ "age25_39",
      q272_grad_age >= 40 ~ "age40p"),
    q131r = case_when(
      q131 < 50000 ~ "<$50k",
      q131 >= 50000 & q131 < 100000 ~ "$50k-$100k",
      q131 >= 100000 & q131 < 150000 ~ "$100k-$150k",
      q131 >= 150000 ~ "$150k+",
      is.na(q131) ~ "Decline to answer"
    ),
    q131r = factor(q131r,
                   levels = c("$150k+","$100k-$150k", "$50k-$100k", "<$50k", "Decline to answer")
                   ),
    q274 = droplevels(q274),
    q82r = if_else(!is.na(q82), 1, 0),
    q83r = if_else(!is.na(q83), 1, 0),
    q84r = if_else(!is.na(q84), 1, 0),
    q85r = if_else(!is.na(q85), 1, 0),
    q86r = if_else(!is.na(q86), 1, 0),
    q87r = if_else(!is.na(q87), 1, 0),

    q21r = if_else(!is.na(q21), 1, 0),
    q22r = if_else(!is.na(q22), 1, 0),
    q23r = if_else(!is.na(q23), 1, 0),
    q24r = if_else(!is.na(q24), 1, 0),
    q25r = if_else(!is.na(q25), 1, 0),
    q26r = if_else(!is.na(q26), 1, 0),
    q27r = if_else(!is.na(q27), 1, 0),
    q28r = if_else(!is.na(q28), 1, 0),
    q29r = if_else(!is.na(q29), 1, 0),
    soc = case_when(
      q191=="White" ~ "White",
      is.na(q191) ~ "Not White"),
    fgen = case_when(
      q128 %in% c(
        "Bachelor’s degree",
        "Some postgraduate, but no postgraduate degree",
        "Master's degree",
        "Advanced degree such as a Ph.D, a Law degree or a Medical degree"
        ) ~ "No",
      TRUE ~ "Yes"
    ),
      ntrad = case_when(
        q272_grad_age > 26 ~ "Yes",
        q272_grad_age <= 26 ~ "No"
      ),
    recg = case_when(
      q225_rec <= 10 ~ "Yes",
      q225_rec > 10 ~ "No"
    )

    )


#tmp <- strada %>% select(q272_orig, q272, q272_rec, q272_clean, q272_grad_age, q225_rec, q272_cat) %>%  filter(q272_orig !="")


strada <- strada %>% select(src, everything())

# Filter out old grads
strada <- strada %>% filter(q225 != "Prior to 2002" | src == "AAA National")

write_sav(strada, "./out/P1650 Harmonized 2022 Data Grad Responses.sav")
write_csv(strada, "./out/P1650 Harmonized 2022 Data Grad Responses.csv", na="")

strada_deid <- strada %>%
  select(-c(q1, q3:q5, q8:q17, q259:q261))

write_sav(strada_deid, "./out/P1650 Harmonized 2022 Data No Names.sav")
write_csv(strada_deid, "./out/P1650 Harmonized 2022 Data No Names.csv", na="")


#table(strada$q225_rec)
#tmp <- strada %>% select(src, q225, q225_rec, q272, q272_rec, q272_cat) %>% filter(!is.na(q225))
