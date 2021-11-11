library(haven)
library(labelled)
library(janitor)
library(tidyverse)
library(here)

options(scipen = 999) #turn off scientific notation

# # 2009
# pisa_2009 <- read_dta('/Users/nathan/Data/PISA/2009/int_stu09_jan27.dta') %>%  
#   rename_all(toupper) %>%
#   select(COUNTRY, CNT,	COBN_S,	COBN_M,	COBN_F,
#          SCHOOLID,	STIDSTD, ST01Q01, ST04Q01,	IMMIG, W_FSTUWT,
#          PV1MATH,	PV1READ,	PV1SCIE,
#          PV2MATH,	PV2READ,	PV2SCIE,
#          PV3MATH,	PV3READ,	PV3SCIE,
#          PV4MATH,	PV4READ,	PV4SCIE,
#          PV5MATH,	PV5READ,	PV5SCIE)
# saveRDS(pisa_2009, '/Users/nathan/Google Drive/1 UCLA/PISA in Europe/Data/pisa_2009.rds')
# 
# filter(pisa_2009, country == 'United Kingdom') %>%
#   select(cobn_s)

# 2012 
pisa_2012 <- read_dta('/Users/nathan/Data/PISA/2012/stu.dta') %>%  
  rename_all(toupper) %>%
  select(NC, CNT,	COBN_S,	COBN_M,	COBN_F,
         SCHOOLID,	STIDSTD, ST01Q01, ST04Q01,	IMMIG, W_FSTUWT,
         ST04Q01, AGE, ST21Q01, HISCED, MISCED, FISCED, ST05Q01, CULTPOS, HEDRES,
         ST25Q01, LANGN,
         PV1MATH,	PV1READ,	PV1SCIE,
         PV2MATH,	PV2READ,	PV2SCIE,
         PV3MATH,	PV3READ,	PV3SCIE,
         PV4MATH,	PV4READ,	PV4SCIE,
         PV5MATH,	PV5READ,	PV5SCIE,
         STRATUM,	OCOD1, 	OCOD2, 	WEALTH, 	HOMEPOS, 	ICTHOME, 	PARED, 	HISEI, 	
         BMMJ1, 	BFMJ2, 	FAMSTRUC, 	ESCS, 		BELONG, 		ST86Q05, SENWGT_STU)

pisa_2012_school <- read_dta('/Users/nathan/Data/PISA/2012/sch.dta') %>%
  rename_all(toupper) %>%
  select(CNT, SCHOOLID, SC03Q01)
pisa_2012 <- left_join(pisa_2012, pisa_2012_school, by = c('CNT', 'SCHOOLID'))

saveRDS(pisa_2012, here('data', 'pisa_2012.rds'))




# 2015
options(haven.show_pillar_labels = FALSE)
pisa_2015 <- read_sav('/Users/nathan/Data/PISA/2015/CY6_MS_CMB_STU_QQQ.sav') %>%
  rename_all(toupper) %>%
  select(CNTRYID,	CNT,	COBN_S,	COBN_M,	COBN_F,
         CNTSCHID,	CNTSTUID,	ST004D01T,	IMMIG, W_FSTUWT,
         ST004D01T, AGE, ST021Q01TA, HISCED, MISCED, FISCED, DURECEC, ST125Q01NA, CULTPOSS, HEDRES,
         ST022Q01TA, LANGN,
         PV1MATH,	PV1READ,	PV1SCIE,
         PV2MATH,	PV2READ,	PV2SCIE,
         PV3MATH,	PV3READ,	PV3SCIE,
         PV4MATH,	PV4READ,	PV4SCIE,
         PV5MATH,	PV5READ,	PV5SCIE,
         PV6MATH,	PV6READ,	PV6SCIE,
         PV7MATH,	PV7READ,	PV7SCIE,
         PV8MATH,	PV8READ,	PV8SCIE,
         PV9MATH,	PV9READ,	PV9SCIE,
         PV10MATH,	PV10READ,	PV10SCIE,
         STRATUM, 	PA042Q01TA, 	OCOD1, 	OCOD2, 	WEALTH, 	HOMEPOS, 	ICTRES, 	
         PARED, 	HISEI, 	BMMJ1, 	BFMJ2, 		ESCS, 		BELONG, UNFAIRTEACHER, SENWT)

pisa_2015_school <- read_sav('/Users/nathan/Data/PISA/2015/CY6_MS_CMB_SCH_QQQ.sav') %>%
  select(CNTSCHID, SC001Q01TA)
pisa_2015 <- left_join(pisa_2015, pisa_2015_school, by = 'CNTSCHID')

saveRDS(pisa_2015, here('data', 'pisa_2015.rds'))


# Select variables of interest
pisa_2018 <- read_sav('/Users/nathan/Data/PISA/2018/CY07_MSU_STU_QQQ.sav') %>%
  select(CNTRYID,	CNT,	COBN_S,	COBN_M,	COBN_F,
         CNTSCHID,	CNTSTUID,	ST004D01T,	IMMIG, W_FSTUWT,
         ST004D01T, AGE, ST021Q01TA, HISCED, MISCED, FISCED, DURECEC, ST125Q01NA, CULTPOSS, HEDRES,
         LANGMOTHER, LANGFATHER, LANGN,
         PV1MATH,	PV1READ,	PV1SCIE,
         PV2MATH,	PV2READ,	PV2SCIE,
         PV3MATH,	PV3READ,	PV3SCIE,
         PV4MATH,	PV4READ,	PV4SCIE,
         PV5MATH,	PV5READ,	PV5SCIE,
         PV6MATH,	PV6READ,	PV6SCIE,
         PV7MATH,	PV7READ,	PV7SCIE,
         PV8MATH,	PV8READ,	PV8SCIE,
         PV9MATH,	PV9READ,	PV9SCIE,
         PV10MATH,	PV10READ,	PV10SCIE,
         STRATUM, 	PA042Q01TA, 	OCOD1, 	OCOD2, 	WEALTH, 	
         HOMEPOS, 	ICTRES, 	PARED, 	HISEI, 	BMMJ1, 	BFMJ2, 		ESCS, 
         BELONG, BEINGBULLIED, SENWT)

pisa_2018_school <- read_sav('/Users/nathan/Data/PISA/2018/CY07_MSU_SCH_QQQ.sav') %>%
  select(CNTSCHID, SC001Q01TA)
pisa_2018 <- left_join(pisa_2018, pisa_2018_school, by = 'CNTSCHID')

saveRDS(pisa_2018, here('data', 'pisa_2018.rds'))



# 
# var <- c('HISCED',	'ST021Q01TA', 	'MISCED', 	'FISCED',	'DURECEC', 	'JOYREADP', 	'EMOSUPS', 	'EMOSUPP',	'CULTPOSS', 	'CURSUPP', 	'PRESUPP', 	'GCAWAREP')
# bind_rows(pisa_2018 %>%
#   summarize(across(c(HISCED, ST021Q01TA, 	MISCED, 	FISCED,
#                    DURECEC, 	JOYREADP, 	EMOSUPS, 	EMOSUPP, 	
#                    CULTPOSS, 	CURSUPP, 	PRESUPP, 	GCAWAREP),
#                    ~ sum(is.na(.x)))),
#   pisa_2018 %>%
#     summarize(across(c(HISCED, ST021Q01TA, 	MISCED, 	FISCED,
#                        DURECEC, 	JOYREADP, 	EMOSUPS, 	EMOSUPP, 	
#                        CULTPOSS, 	CURSUPP, 	PRESUPP, 	GCAWAREP),
#                      ~ sum(!is.na(.x)))))










#### Combine subsetted data ####
standardize <- function(x){(x - mean(x, na.rm = T))/sd(x, na.rm = T)}


pisa_2018 <- readRDS(here('Data', 'pisa_2018.rds')) %>% 
  mutate(
    id = as.character(CNTSTUID),
    school_id = as.character(CNTSCHID),
    year = '2018',
    weight = W_FSTUWT,
    # Make countries character variables
    country = as_factor(CNTRYID),
    birth_country = as_factor(COBN_S),
    mom_country = as_factor(COBN_M),
    dad_country = as_factor(COBN_F),
    imm_category = as_factor(IMMIG),
    age_arrival = case_when(
      as.numeric(ST021Q01TA) <= 17 ~ (as.numeric(ST021Q01TA) - 1)), 
    age = AGE,
    gender = as_factor(ST004D01T),
    int_lang = !is.na(LANGFATHER) | !is.na(LANGMOTHER),
    lang = as_factor(LANGN),
    # Matching variables
    parent_ed = as.numeric(HISCED),
    parent_ed_fct = as_factor(HISCED),
    mom_ed = as.numeric(MISCED),
    mom_ed_fct = as_factor(MISCED),
    dad_ed = as.numeric(FISCED),
    dad_ed_fct = as_factor(FISCED),
    early_ed = case_when(
      ST125Q01NA == 7 ~ 'No',
      DURECEC == 0 ~ '0-1',
      DURECEC >= 1 ~ '1+'),
    cultural_pos = standardize(CULTPOSS),
    home_ed = standardize(HEDRES),
    math1 = PV1MATH, math2 = PV2MATH, math3 = PV3MATH, math4 = PV4MATH, math5 = PV5MATH,
    read1 = PV1READ, read2 = PV2READ, read3 = PV3READ, read4 = PV4READ, read5 = PV5READ,
    scie1 = PV1SCIE, scie2 = PV2SCIE, scie3 = PV3SCIE, scie4 = PV4SCIE, scie5 = PV5SCIE,
    # other variables
    stratum = STRATUM,
    hh_income = as_factor(PA042Q01TA),
    mom_occ = as_factor(OCOD1),
    dad_occ = as_factor(OCOD2),
    wealth = standardize(WEALTH),
    home_pos = standardize(HOMEPOS),
    ict_res = standardize(ICTRES),
    parent_ed_years = as.numeric(PARED),
    parent_isei = HISEI,
    mom_isei = BMMJ1,
    dad_isei = BFMJ2,
    escs = standardize(ESCS),
    school_belonging = standardize(BELONG),
    bullied = BEINGBULLIED,
    school_location = recode_factor(as.numeric(SC001Q01TA),
                             `1` = 'Village', `2` = 'Small Town',
                             `3` = 'Town', `4` = 'City', `5` = 'Large City'),
    senate_weight = SENWT)


pisa_2015 <- readRDS(here('Data', 'pisa_2015.rds')) %>%
  mutate(
  id = as.character(CNTSTUID),
  school_id = as.character(CNTSCHID),
  year = '2015',
  weight = W_FSTUWT,
  # Make countries character variables
  country = as_factor(CNTRYID),
  birth_country = as_factor(COBN_S),
  mom_country = as_factor(COBN_M),
  dad_country = as_factor(COBN_F),
  imm_category = as_factor(IMMIG),
  age_arrival = case_when(
    as.numeric(ST021Q01TA) <= 17 ~ (as.numeric(ST021Q01TA) - 1)), 
  age = AGE,
  gender = as_factor(ST004D01T),
  int_lang = ST022Q01TA == 2,
  lang = as_factor(LANGN),
  # Matching variables
  parent_ed = as.numeric(HISCED),
  parent_ed_fct = as_factor(HISCED),
  mom_ed = as.numeric(MISCED),
  dad_ed = as.numeric(FISCED),
  mom_ed_fct = as_factor(MISCED),
  dad_ed_fct = as_factor(FISCED),
  early_ed = case_when(
    ST125Q01NA == 7 ~ 'No',
    DURECEC == 0 ~ '0-1',
    DURECEC >= 1 ~ '1+'),
  cultural_pos = (CULTPOSS - mean(CULTPOSS, na.rm = T))/sd(CULTPOSS, na.rm = T),
  home_ed = (HEDRES - mean(HEDRES, na.rm = T))/sd(HEDRES, na.rm = T),
  math1 = PV1MATH, math2 = PV2MATH, math3 = PV3MATH, math4 = PV4MATH, math5 = PV5MATH,
  read1 = PV1READ, read2 = PV2READ, read3 = PV3READ, read4 = PV4READ, read5 = PV5READ,
  scie1 = PV1SCIE, scie2 = PV2SCIE, scie3 = PV3SCIE, scie4 = PV4SCIE, scie5 = PV5SCIE,
  # other variables
  stratum = STRATUM,
  hh_income = as_factor(PA042Q01TA),
  mom_occ = as_factor(OCOD1),
  dad_occ = as_factor(OCOD2),
  wealth = standardize(WEALTH),
  home_pos = standardize(HOMEPOS),
  ict_res = standardize(ICTRES),
  parent_ed_years = as.numeric(PARED),
  parent_isei = HISEI,
  mom_isei = BMMJ1,
  dad_isei = BFMJ2,
  escs = standardize(ESCS),
  school_belonging = standardize(BELONG),
  unfair_teacher = UNFAIRTEACHER,
  school_location = recode_factor(as.numeric(SC001Q01TA),
                           `1` = 'Village', `2` = 'Small Town',
                           `3` = 'Town', `4` = 'City', `5` = 'Large City'),
  senate_weight = SENWT)



# Need to load 2012 after since I'm using 2018 labels
pisa_2012 <- readRDS(here('Data', 'pisa_2012.rds')) %>%
  mutate(LANGN = as.numeric(LANGN))
# Add labels to country variables
for(v in c('COBN_S', 'COBN_F', 'COBN_M', 'NC')){
  val_labels(pisa_2012[[v]]) <- val_labels(pisa_2018[['COBN_S']])
}
for(v in c('OCOD1', 'OCOD2', 'MISCED', 'FISCED', 'HISCED')){
  val_labels(pisa_2012[[v]]) <- val_labels(pisa_2018[[v]])
}
val_labels(pisa_2012[['LANGN']]) <- val_labels(pisa_2015$LANGN)

pisa_2012 <- pisa_2012 %>%  
    mutate(
    id = as.character(STIDSTD),
    school_id = as.character(SCHOOLID),
    year = '2012',
    weight = as.numeric(W_FSTUWT),
    # Make countries character variables
    country = ifelse(CNT == 'GBR', 'United Kingdom', as.character(as_factor(NC))),
    birth_country = as.character(as_factor(COBN_S)),
    birth_country = ifelse(str_starts(birth_country, '9'), 
                           'Another country', birth_country),
    mom_country = as_factor(COBN_M),
    dad_country = as_factor(COBN_F),
    imm_category = as_factor(IMMIG),
    age_arrival = ST21Q01,
    age = AGE,
    gender = as_factor(ST04Q01), 
    int_lang = ST25Q01 == 2,
    lang = as_factor(LANGN),
    # Matching variables
    parent_ed = as.numeric(HISCED),
    parent_ed_fct = as_factor(HISCED),
    mom_ed = as.numeric(MISCED),
    dad_ed = as.numeric(FISCED),
    mom_ed_fct = as_factor(MISCED),
    dad_ed_fct = as_factor(FISCED),
    early_ed = case_when(
      ST05Q01 == 1 ~ 'No',
      ST05Q01 == 2 ~ '0-1',
      ST05Q01 == 3 ~ '1+'),
    cultural_pos = (CULTPOS - mean(CULTPOS, na.rm = T))/sd(CULTPOS, na.rm = T),
    home_ed = (HEDRES - mean(HEDRES, na.rm = T))/sd(HEDRES, na.rm = T),
    math1 = as.numeric(PV1MATH), math2 = as.numeric(PV2MATH), math3 = as.numeric(PV3MATH),
    math4 = as.numeric(PV4MATH), math5 = as.numeric(PV5MATH),
    read1 = as.numeric(PV1READ), read2 = as.numeric(PV2READ), read3 = as.numeric(PV3READ), 
    read4 = as.numeric(PV4READ), read5 = as.numeric(PV5READ),
    scie1 = as.numeric(PV1SCIE), scie2 = as.numeric(PV2SCIE), 
    scie3 = as.numeric(PV3SCIE), scie4 = as.numeric(PV4SCIE), scie5 = as.numeric(PV5SCIE),
    # other variables
    stratum = STRATUM,
    mom_occ = as_factor(OCOD1),
    dad_occ = as_factor(OCOD2),
    wealth = standardize(WEALTH),
    home_pos = standardize(HOMEPOS),
    ict_res = standardize(ICTHOME),
    parent_ed_years = as.numeric(PARED),
    parent_isei = HISEI,
    mom_isei = BMMJ1,
    dad_isei = BFMJ2,
    family_structure = case_when(FAMSTRUC == 1 ~ 'single-parent family',
                                 FAMSTRUC == 2 ~ 'two-parent family',
                                 FAMSTRUC == 3 ~ 'does not live with parents'),
    escs = standardize(ESCS),
    school_belonging = standardize(BELONG),
    unfair_teacher_2012 = ST86Q05,
    school_location = as_factor(SC03Q01),
    senate_weight = SENWGT_STU*5)





# Combine three years into one dataset
new_vars <- c('id', 'year', 'country', 'weight', 'birth_country', 'mom_country', 
              'dad_country', 'imm_category',
              'gender', 'age_arrival', 'age', 'parent_ed', 'mom_ed', 'dad_ed', 
              'early_ed', 'cultural_pos', 'home_ed',
              'int_lang', 'school_id', 'lang',
              paste0('math', 1:5), paste0('read', 1:5), paste0('scie', 1:5),
              'stratum', 	'hh_income', 	'mom_occ', 	'dad_occ', 	'wealth', 	'home_pos', 	
              'ict_res', 'parent_ed_years', 	'parent_isei', 	
              'mom_isei', 	'dad_isei', 	'family_structure', 	'escs', 	
              'school_belonging', 	'bullied', 	'unfair_teacher', 	'unfair_teacher_2012',
              'school_location', 'senate_weight', 'parent_ed_fct', 'mom_ed_fct', 'dad_ed_fct')


pisa <- bind_rows(
    pisa_2012, pisa_2015, pisa_2018) %>%
  select(new_vars) %>%
  mutate(imm_year = round(as.numeric(year) - (age - age_arrival), 0),
         early_ed = as_factor(early_ed),
         year = as_factor(year),
         female = as.numeric(gender == 'Female'),
         early_ed = factor(early_ed, c('No', '0-1', '1+'))) %>%
  select(-gender)


saveRDS(pisa, here('Data', 'pisa.rds'))



  
