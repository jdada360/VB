#=======================================================
# This script explores the correlates of the need of prescription glasses. 
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
  # replace(is.na(.), 0) %>% 
  mutate(needall = (beneficiaries/enrolled)*100,
         myopiaall = (myopia/enrolled)*100,
         wearsall = (wearsglasses/beneficiaries)*100,
         state = substr(schoolid,1,2),
         state = as.factor(state),
         ln_enrolled = log(enrolled)) 

enrlmntdta$level <- as.numeric(as.factor(enrlmntdta$nivel))


# First Regression is done by restricting sample to u0<=1

reg_enrlmntdta <- enrlmntdta %>% 
  filter(u0 <= 1 & level == 1) %>% 
  mutate(q10 =  xtile(u10, wt = enrolled, n = 5))

# table(reg_enrlmntdta$q10, reg_enrlmntdta$level)
# 
# tab1(reg_enrlmntdta_2$q10)


reg_enrlmntdta_2 <- enrlmntdta %>% 
  filter(u0 <= 1 & level == 2) %>% 
  mutate(q10 =  xtile(u10, wt = enrolled, n = 5))

reg_list = list(reg_enrlmntdta, reg_enrlmntdta_2)


reg_results = as.data.frame()

# We got it!!!!

lapply(reg_list, function(df){
   x <- felm(formula = needall ~ factor(q10) | factor(state) ,  data = df,
         weights = df$enrolled)
   
    # stargazer(x, type = "text")
    
     
    x$N
    summary(x)$coefficients
  
    y <- felm(formula = myopiaall ~ factor(q10) | factor(state), data = df,
              weights = df$enrolled)
    
    # stargazer(y, type = "text")
    
    z <- felm(formula = wearsall ~ factor(q10) | factor(state), data = df,
              weight = df$enrolled)
    # stargazer(z, type = "text")
    })

# 2

# Second Regression is done without restricting the sample

reg_enrlmntdta_3 <- enrlmntdta %>% 
  filter(level == 1) %>% 
  mutate(q10 =  xtile(u10, wt = enrolled, n = 5))


reg_enrlmntdta_4 <- enrlmntdta %>% 
  filter(level == 2) %>% 
  mutate(q10 =  xtile(u10, wt = enrolled, n = 5))

reg_list_2 = list(reg_enrlmntdta_3, reg_enrlmntdta_4)

lapply(reg_list_2, function(df){
  x <- felm(formula = needall ~ factor(q10) | factor(state) ,  data = df,
            weights = df$enrolled)
  # stargazer(x, type = "text")

  
  y <- felm(formula = myopiaall ~ factor(q10) | factor(state), data = df,
            weights = df$enrolled)
  stargazer(y, type = "text")
  
  z <- felm(formula = wearsall ~ factor(q10) | factor(state), data = df,
            weight = df$enrolled)
  stargazer(z, type = "text")
})

detach(package:plyr)

reg_lists<- bind_rows(reg_list, reg_list_2) %>% 
  ungroup() %>%
  group_by(nivel, q10) %>% 
  mutate(n = 1) %>% 
  summarise(schools = sum(n),
            enrolled = sum(enrolled),
         across(c("u10", "needall", "myopiaall", "wearsall"),
               ~mean(.x, na.rm = F))) %>% 
  distinct(q10, nivel, .keep_all = T) %>% 
  ungroup() %>% 
  dplyr::select(nivel, q10, u10, enrolled, needall,
                myopiaall, wearsall) %>% 
  mutate_if(is.numeric, ~round(.x, 0))
  
# reg_list_2 <- bind_rows(reg_list_2)

# reg_lists <- bind_rows(reg_list, reg_list_2) %>% 
#   group_by(level, q10) %>%
#   summarize(sm = sum(enrolled))
#   


# Questions

# L62 - Schools = enrolled, unsure what summary 

