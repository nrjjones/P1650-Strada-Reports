library(haven)

# National
nat <- read_spss("./raw/P1650 Strada National Core_June 6, 2022_14.28.sav")
nat <- nat %>% filter(row_number() > 20)

# SNHU
snhu <- read_spss("./raw/P1650 Strada S New Hampshire U_April 30, 2022_10.40.sav")

# Univ of Arizona
uaz <- read_spss("./raw/P1650 Strada U Arizona_April 30, 2022_10.44.sav")

#Arizona State
azs <- read_spss("./raw/P1650 Strada Arizona State_April 30, 2022_10.49.sav")
azs <- azs %>% select(-Q59)
#CU Denver
cud <- read_spss("./raw/P1650 Strada UC Denver_April 30, 2022_10.52.sav")

#UMN Twin Cities
mtc <- read_spss("./raw/P1650 Strada UMN Twin Cities_April 30, 2022_11.01.sav")

#UMN Rochester
mro <- read_spss("./raw/P1650 Strada UMN Rochester_April 30, 2022_11.32.sav")

#Montana
mnt <- read_spss("./raw/P1650 Strada U Montana_April 30, 2022_11.39.sav")

#Pace
pac <- read_spss("./raw/P1650 Strada Pace U_April 30, 2022_11.43.sav")

#WGU
wgu <- read_spss("./raw/P1650 Strada WGU_April 30, 2022_11.44.sav")
attr(wgu$Q20_1, "label") <- "How valuable was... - ...attending speakers, forums, cultural events and discussions?"

#Pitt
pit <- read_spss("./raw/P1650 Strada U Pittsburgh_April 30, 2022_11.34.sav")

#NYU
nyu <- read_spss("raw/P1650 Strada NYU_May 9, 2022_13.07.sav")

#UT Dallas
utd <- read_spss("./raw/P1650 Strada U Texas Dallas_May 9, 2022_12.19.sav")

#U Hawaii
uhi <- read_spss("./raw/P1650 Strada U Hawaii System_May 9, 2022_12.17.sav")

#UT Arlington
uta <- read_spss("./raw/P1650 Strada U Texas Arlington_May 18, 2022_04.15.sav")

#UT San Antonio
uts <- read_spss("./raw/P1650 Strada UT San Antonio_May 18, 2022_04.17.sav")

