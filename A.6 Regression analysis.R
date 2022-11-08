#=======================================================
# This do-file explores the correlates of the need of prescription glasses. 
# We look at differences across quintiles of population density. Population 
# density is proxied with the number of schools of the same level (primaria, 
# secundaria) in a 10-km radius. We include and exclude cases with many schools
# reporting the exact same coordinates under the premise that, although incorrect, 
# those cases may correspond to dense urban areas.
#=======================================================

setwd("~/Desktop/DIL/Ver Bien/Data")

enrlmnt_dta <- read_dta("Clean data/Enrollment, need and use 2015-2020.dta")

sc_rd <- read_dta("Clean data/Count of schools by radius.dta")

enrlmntdta <- enrlmnt_dta %>% 
  group_by(nivel, schoolyear, schoolid, shift) %>% 
  summarise_at(c("enrolled", "beneficiaries", "myopia", "wearsglasses"),
               sum, na.rm = T) %>% 
  left_join(., sc_rd,
            by = "schoolid") %>%
  drop_na(u0) %>% 
  replace(is.na(.), 0) %>% 
  mutate(needall = (beneficiaries/enrolled)*100,
         myopiaall = (myopia/enrolled)*100,
         wearsall = (wearsglasses/beneficiaries)*100,
         state = substr(schoolid,1,2),
         ln_enrolled = log(enrolled)) %>% 
  mutate(state = as.factor(state)) 

enrlmntdta$level <- as.numeric(as.factor(enrlmntdta$nivel))


reg_enrlmntdta <- enrlmntdta %>% 
  filter(u0 <= 1)

setDT(reg_enrlmntdta)

reg_enrlmntdta[ , q10 := cut(u10,
                             breaks = quantile(u10, probs = seq(0,1,1/5),
                                               weights = "enrolled"),
                             labels = 1:5, right = FALSE)]

# na.rm - T, weights = "enrolled"))

# L28 - leaves schools that are isolated, i.e. no other schools that
# are nearby

# L31 - what is the _ for throughout
# A - create quintiles of count of schools in a 10 kilometer radius
# Seperately for elementary or middle schools 
# Level = 1 = primary school 
# quintiles of u10 weighted by enrollement 
# quntiles of the numbers of schools in a 10 km radius, weighted by enrollment



# nq(5) = number of quantiles

# areg - absorbing variable (consider the fixed effects but dont report tem)
# is state or the fixed effects defined for each of the states
# i.q10 - creates dummy variables for each of the levels of q10

# Need of glasses grow with density
# 62% more likely to need glasses if you attend a school in a low density area vs
# high area

# Parmby command - executes the quoted command with certain options
# do it by nivel, es(N, r2) <- include number of observations and r2
# change to condfidence level 99.
# merge with the first round of regression.
# average number of schools in each quintile. 
# enrolled is weighted


# Pablo will give me the go ahead.

# the second version is done without filtering u10 <= 0

# Need to regress needall, myopiall, wearsall

# xtile command

outcomes = c("needall", "myopiall","wearsall")

models <- dlply(reg_enrlmntdta,"level",
                function(df){
                  felm(formula =  wearsall ~ q10 | state, data = df,
                       weight = df$enrolled)
                })

ldply(models, coef)

l_ply(models, stargazer(models, type= "text"), .print = T)


models <- dlply(reg_enrlmntdta,"level",
                function(df){
                  felm(formula = myopiaall  ~ q10 | state , data = df,
                       weight = df$enrolled)
                })

ldply(models, coef)

l_ply(models, stargazer(models, type= "text"), .print = T)


# reg_func <- function(df){
#   summary(felm(formula = needall ~ q10 | state, data = df,
#                   weight = df$enrolled))}
# 
# by(models, models$level, reg_func)


# First Regression is done by restricting sample to u0<=1

# by_level <- group_by(reg_enrlmntdta, level)
# 
# reg_1_needall <- do(by_level,
#    tidy(felm(formula = needall ~ q10 | state, data = reg_enrlmntdta, 
#              weights = reg_enrlmntdta$enrolled)))
# 
# 
# reg_1_myopiall <- do(by_level,
#              tidy(felm(formula = myopiaall ~ q10 | state, data = reg_enrlmntdta, 
#                        weight = reg_enrlmntdta$enrolled)))
# 
# reg_1_wearsall <- do(by_level,
#                tidy(felm(formula = wearsall ~ q10 | state, data = reg_enrlmntdta, 
#                          weight = reg_enrlmntdta$enrolled)))
# 




# I find higher density is correlated with higher need and myopia.


# # Second Regression is done by not restricting sample
# 
# reg_enrlmntdta_2 <- enrlmntdta 
#   
# setDT(reg_enrlmntdta_2)
# 
# reg_enrlmntdta_2[ , q10 := cut(u10,
#                              breaks = quantile(u10, probs = seq(0,1,1/5),
#                                                weights = "enrolled"),
#                              labels = 1:5, right = FALSE)]
# 
#   
# by_country <- group_by(reg_enrlmntdta_2, level)
# 
# 
# reg_1_needall <- do(by_country,
#                     tidy(felm(formula = needall ~ q10 | state, data = reg_enrlmntdta_2, 
#                               weight = reg_enrlmntdta_2$enrolled)))
# 
# reg_1_myopiall <- do(by_country,
#                      tidy(felm(formula = myopiaall ~ q10 | state, data = reg_enrlmntdta_2, 
#                                weight = reg_enrlmntdta_2$enrolled)))
# 
# reg_1_wearsall <- do(by_country,
#                      tidy(felm(formula = wearsall ~ q10 | state, data = reg_enrlmntdta_2, 
#                                weight = reg_enrlmntdta_2$enrolled)))

