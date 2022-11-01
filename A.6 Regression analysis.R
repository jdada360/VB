#=======================================================
# This do-file explores the correlates of the need of prescription glasses. 
# We look at differences across quintiles of population density. Population 
# density is proxied with the number of schools of the same level (primaria, 
# secundaria) in a 10-km radius. We include and exclude cases with many schools
# reporting the exact same coordinates under the premise that, although incorrect, 
# those cases may correspond to dense urban areas.
#=======================================================

setwd("~/Desktop/DIL/Ver Bien/Data")

enrlmntdta <- read_dta("Clean data/Enrollment, need and use 2015-2020.dta")

sc_rd <- read_dta("Clean data/Count of schools by radius.dta")

enrlmntdta <- enrlmntdta %>% 
  group_by(nivel, schoolyear, schoolid, shift) %>% 
  summarise_at(c("enrolled", "beneficiaries", "myopia", "wearsglasses"),
               sum, na.rm = T) %>% 
  left_join(., sc_rd,
            by = "schoolid") %>%
  drop_na(u0) %>% 
  filter(u0 != 0) %>% 
  mutate(needall = (beneficiaries/enrolled)*100,
         myopiaall = (myopia/enrolled)*100,
         wearsall = (wearsglasses/beneficiaries)*100,
         state = substr(schoolid, 1,2),
         level = as.factor(nivel),
         ln_enrolled = log(enrolled))

reg_enrlmntdta <- enrlmntdta %>% 
  filter(u0 <= 1) %>% 
  mutate(q10 = ifelse(level %in% 1:2, "_", NA))

# L28 - leaves schools that are isolated, i.e. no other schools that
# are nearby

# L31 - what is the _ for throughout
# A - create quintiles of count of schools in a 10 kilometer radius
# Seperately for elementary or middle schools 
# Level = 1 = primary school 
# quintiles of u10 weighted by enrollement 
# quntiles of the numbers of schools in a 10 km radius, weighted by enrollment



# nq(5) = number of quantiles
# L38 - By nivel or level which we encoded above?
# L38 - i.q10  
# areg - absorbing variable (consider the fixed effects but dont report tem)
# is state or the fixed effects defined for each of the states
# i.q10 - creates dummy variables for each of the levels of q10

# Need of glasses grow with density. 
# 62% more likely to need glasses if you attend a school in a low density area vs
# high area

# Parmby command - executes the quoted command with certain options
# do it by nivel, es(N, r2) <- include number of observations and r2
# change to condfidence level 99.
# merge with the first round of regression.
# average number of schools in each quintile. 
# enrolled is weighted

# corrolates - covariates 
# is need related to anything else
#

# Pablo will give me the go ahead.

# the second version is done without filtering u10 <= 0




# Need to regress needall, myopiall, wearsall

# xtile command

felm(formula = needall ~  )
lm(formula = myopiaall ~ wearsglasses + ln_enrolled + beneficiaries, enrlmntdta )
  

