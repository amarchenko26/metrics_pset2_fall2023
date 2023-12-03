################################# FRONT MATTER #################################

rm(list = ls())
gc()

library(haven) # used to read in SAS files
library(AER)
library(here)

################################# READ IN DATA #################################

# Requires the file "m_d_806.sas7bdat" from the replication files

# setwd()
data <- read_sas(here("data/AngEv98/m_d_806.sas7bdat"))
colnames(data) <- tolower(colnames(data))

################################# TIDY UP DATA #################################

# Code adapted from the replication file "angev80.log".
# Results validated by comparison to "angev.lst" 

# twins instrument
data$multi2nd <- data$ageq2nd == data$ageq3rd 
data$multi2nd[is.na(data$multi2nd)] <- 0

# illegit flag
data$qtrmar <- as.integer(data$qtrmar)
data$agemar <- as.integer(data$agemar)
data$qtrbthm <- as.integer(data$qtrbthm)
data$qtrbkid <- as.integer(data$qtrbkid)
data$illegit <- 0
data_0 <- subset(data, qtrmar == 0)
data <- subset(data, qtrmar != 0)
data$qtrmar <- data$qtrmar - 1
yom <- data$yobm + data$agemar + (data$qtrbthm > data$qtrmar)
dom_q <- yom + (data$qtrmar / 4)
do1b_q <- data$yobk + (data$qtrbkid / 4)
data$illegit <- dom_q > do1b_q
data <- rbind(data_0, data)
rm(data_0, yom, dom_q, do1b_q)

# sexes of the first two kids
data$boy1st <- data$sexk == "0"
data$boy2nd <- data$sex2nd == "0"
data$boys2 <- data$boy1st == 1 & data$boy2nd == 1
data$girls2 <- data$boy1st == 0 & data$boy2nd == 0
data$samesex <- data$boys2 == 1 | data$girls2 == 1
data$morekids <- data$kidcount > 2

# race indicators for mom and dad
data$blackm <- data$racem == "02"
data$hispm <- data$racem == "12"
data$whitem <- data$racem == "01"
data$othracem <- 1 - data$blackm - data$hispm - data$whitem

data$blackd <- data$raced == "02"
data$hispd <- data$raced == "12"
data$whited <- data$raced == "01"
data$othraced <- 1 - data$blackd - data$hispd - data$whited;

# education variables 
data$gradem <- as.numeric(sub("^0", "", data$gradem))
data$educm <- 
  pmax(0, data$gradem - 3 + (data$fingradm == "1" | data$fingradm == "2"))
data$hsgrad <- data$educm == 12
data$hsormore <- data$educm >= 12
data$moreths <- data$educm > 12

# ages of mom and dad, plus ages when 1st kid was born
data$agem1 <- as.integer(data$agem)
data$aged1 <- as.integer(data$aged)
data$yobd <- 79 - data$aged1 + (data$qtrbthd == "0")

data$ageqm <- 4 * (80 - data$yobm) - data$qtrbthm - 1
data$qtrbthd <- as.integer(data$qtrbthd)
data$ageqd <- 4 * (80 - data$yobd) - data$qtrbthd
data$agefstm <- floor((data$ageqm - data$ageqk) / 4)
data$agefstd <- floor((data$ageqd - data$ageqk) / 4)

# mom and dad labor supply variables;
data$weeksm1 <- as.numeric(sub("^0", "", data$weeksm))
data$weeksd1 <- as.numeric(sub("^0", "", data$weeksd))

data$workedm <- data$weeksm1 > 0
data$workedd <- data$weeksd1 > 0
data$workedd[is.na(data$workedd)] <- 0 # replication files seem to treat NA as 0

data$hourswm <- as.numeric(sub("^0", "", data$hoursm))
data$hourswd <- as.numeric(sub("^0", "", data$hoursd))

data$income1d <- as.numeric(data$income1d)
data$income2d <- as.numeric(data$income2d)
data$income1m <- as.numeric(data$income1m)
data$income2m <- as.numeric(data$income2m)
data$incomed <- data$income1d + pmax(0, data$income2d)
data$incomem <- data$income1m + pmax(0, data$income2m)

# deflate wages 
data$incomem <- data$incomem * 2.099173554
data$incomed <- data$incomed * 2.099173554

data$faminc1 <- as.numeric(data$faminc)
data$faminc1 <- data$faminc1 * 2.099173554
data$famincl <- log(pmax(data$faminc1 , 1))

data$nonmomi <- data$faminc1 - data$income1m * 2.099173554
data$nonmomil <- log(pmax(1, data$nonmomi));

# they construct a variable morea3 that does not seem to be used subsequently
# 450  *if morekids=0 then ageq3rdk=0;
# 451  
# 452  morea3=morekids*ageq3rdk;

data$qobm <- data$qtrbthm
# indicator foe whether the observation belongs in the msample
data$msample <- data$aged != "" & 
  data$timesmar == "1" & 
  data$marital == "0" & 
  data$illegit == 0 & 
  data$agefstd >= 15 & 
  data$agefstm >= 15 

data <- data.frame(kidcount = data$kidcount, 
                   morekids = data$morekids, 
                   boys2 = data$boys2, 
                   girls2 = data$girls2, 
                   boy1st = data$boy1st, 
                   samesex = data$samesex, 
                   multi2nd = data$multi2nd, 
                   agem = data$agem, 
                   agem1 = data$agem1, 
                   agefstm = data$agefstm, 
                   blackm = data$blackm, 
                   hispm = data$hispm, 
                   othracem = data$othracem, 
                   workedm = data$workedm, 
                   weeksm1 = data$weeksm1, 
                   hourswm = data$hourswm, 
                   incomem = data$incomem, 
                   faminc1 = data$faminc1, 
                   nonmomi = data$nonmomi, 
                   famincl = data$famincl, 
                   nonmomil = data$nonmomil, 
                   aged1 = data$aged1, 
                   agefstd = data$agefstd, 
                   moreths = data$moreths, 
                   boy2nd = data$boy2nd, 
                   msample = data$msample, 
                   educm = data$educm, 
                   hsormore = data$hsormore, 
                   hsgrad = data$hsgrad, 
                   asex = data$asex, 
                   aage = data$aage, 
                   aqtrbrth = data$aqtrbrth, 
                   asex2nd = data$asex2nd, 
                   aage2nd = data$aage2nd, 
                   blackd = data$blackd, 
                   hispd = data$hispd,
                   othraced = data$othraced, 
                   workedd = data$workedd, 
                   weeksd1 = data$weeksd1, 
                   hourswd = data$hourswd, 
                   incomed = data$incomed, 
                   marital = data$marital, 
                   ageq2nd = data$ageq2nd, 
                   ageqk = data$ageqk, 
                   ageq3rd = data$ageq3rd, 
                   ageqm = data$ageqm, 
                   yobm = data$yobm, 
                   qobm = data$qobm)

data_twoa <- subset(data, 
                    agem1 >= 21 & 
                      agem1 <= 35 & 
                      kidcount >= 2 & 
                      ageq2nd > 4 & 
                      agefstm >= 15 & 
                      asex == 0 & 
                      aage == 0 & 
                      aqtrbrth == 0 & 
                      asex2nd == 0 & 
                      aage2nd == 0)

# data_twoa should have 394840 observations. 
# Generate summary statistics to compre against replication files
summary_stats_twoa <- cbind(
  c(mean(data_twoa$kidcount), 
    mean(data_twoa$morekids), 
    mean(data_twoa$boy1st), 
    mean(data_twoa$boy2nd),
    mean(data_twoa$boys2), 
    mean(data_twoa$girls2), 
    mean(data_twoa$samesex), 
    mean(data_twoa$multi2nd, na.rm = T),
    mean(data_twoa$agem1), 
    mean(data_twoa$aged1, na.rm = T), 
    mean(data_twoa$agefstm), 
    mean(data_twoa$agefstd, na.rm = T), 
    mean(data_twoa$workedm), 
    mean(data_twoa$workedd, na.rm = T), 
    mean(data_twoa$weeksm1), 
    mean(data_twoa$weeksd1, na.rm = T), 
    mean(data_twoa$hourswm),
    mean(data_twoa$hourswd, na.rm = T), 
    mean(data_twoa$incomem), 
    mean(data_twoa$incomed, na.rm = T), 
    mean(data_twoa$faminc1), 
    mean(data_twoa$nonmomi)),
  c(sd(data_twoa$kidcount), 
    sd(data_twoa$morekids), 
    sd(data_twoa$boy1st), 
    sd(data_twoa$boy2nd),
    sd(data_twoa$boys2), 
    sd(data_twoa$girls2), 
    sd(data_twoa$samesex), 
    sd(data_twoa$multi2nd, na.rm = T),
    sd(data_twoa$agem1), 
    sd(data_twoa$aged1, na.rm = T), 
    sd(data_twoa$agefstm), 
    sd(data_twoa$agefstd, na.rm = T),
    sd(data_twoa$workedm), 
    sd(data_twoa$workedd, na.rm = T), 
    sd(data_twoa$weeksm1), 
    sd(data_twoa$weeksd1, na.rm = T),
    sd(data_twoa$hourswm), 
    sd(data_twoa$hourswd, na.rm = T), 
    sd(data_twoa$incomem), 
    sd(data_twoa$incomed, na.rm = T),
    sd(data_twoa$faminc1), 
    sd(data_twoa$nonmomi))
)
rownames(summary_stats_twoa) <- c("KIDCOUNT", 
                                  "morekids", 
                                  "boy1st", 
                                  "boy2nd", 
                                  "boys2", 
                                  "girls2", 
                                  "samesex", 
                                  "multi2nd", 
                                  "agem1", 
                                  "aged1", 
                                  "agefstm", 
                                  "agefstd", 
                                  "workedm", 
                                  "workedd",
                                  "weeksm1", 
                                  "weeksd1", 
                                  "hourswm", 
                                  "hourswd", 
                                  "incomem", 
                                  "incomed", 
                                  "faminc1", 
                                  "nonmomi")
                

data_twob <- subset(data_twoa, msample == 1) 

#Save 
# Cut down the data to only variables used
pums1980 <- data.frame(Y = data_twoa$famincl,
                       D = data_twoa$morekids,
                       Z = data_twoa$samesex, 
                       boy1st = data_twoa$boy1st,
                       boy2nd = data_twoa$boy2nd,
                       blackm = data_twoa$blackm,
                       hispm = data_twoa$hispm,
                       othracem = data_twoa$othracem,
                       agem1 = data_twoa$agem1,
                       agefstm = data_twoa$agefstm)

pums1980m <- data.frame(y = data_twob$famincl,
                        d = data_twob$morekids,
                        z = data_twob$samesex, 
                        boy1st = data_twob$boy1st,
                        boy2nd = data_twob$boy2nd,
                        blackm = data_twob$blackm,
                        hispm = data_twob$hispm,
                        othracem = data_twob$othracem,
                        agem1 = data_twob$agem1,
                        agefstm = data_twob$agefstm)


# save data to pass to other script
# path <- getwd()

#saveRDS(pums1980m, paste(path, "/pums1980m.rds", sep = ""))
write.csv(pums1980m, here("data/andres_cleaned.csv"))

