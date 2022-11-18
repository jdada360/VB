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
  ungroup() %>% 
  filter(u0 <= 1) %>% 
  group_by(level) %>% 
  mutate(q10 = xtile(u10, n = 5, wt = enrolled)) %>% 
  ungroup() 

reg_enrlmntdta_2 <- enrlmntdta %>% 
  ungroup() %>% 
  group_by(level) %>% 
  mutate(q10 = xtile(u10, n = 5, wt = enrolled)) %>% 
  ungroup()

# reg_enrlmntdta_2 <- enrlmntdta %>%
#   ungroup() %>% 
#   filter(u0 <= 1 & level == 2) %>% 
#   mutate(q10 =  xtile(u10, wt = enrolled, n = 5),
#          q10 = as.factor(q10))


reg_list = list(reg_enrlmntdta, reg_enrlmntdta_2)

# purrr::map(reg_list, ~.x %>% group_by(level) %>% 
#              
# by(reg_list, )


output <- lapply(seq_along(reg_list))
                 
linear_map <- function(df){
   x <- felm(formula = needall ~ factor(q10) | factor(state),  data = reg_list[[df]],
             weights = reg_list[[df]][["enrolled"]])

   x <-  bind_cols(tidy(x)[1:3], nobs(x), summary(x)$r.squared, level = df,
                   outcome = "needall")

   y <- felm(formula = myopiaall ~ factor(q10) | factor(state), data = reg_list[[df]],
            weights = reg_list[[df]][["enrolled"]])

   y <- bind_cols(tidy(y)[1:3], nobs(y), summary(y)$r.squared,
                  level = df, outcome = "myopiaall")

   z <- felm(formula = wearsall ~ factor(q10) | factor(state), data = reg_list[[df]],
            weights = reg_list[[df]][["enrolled"]])

   z <-  bind_cols(tidy(z)[1:3], nobs(z), summary(z)$r.squared,
                  level = df,
                  outcome = "wearsall")

   list(x, y, z)}

m <- lapply(split())


output_1 <- bind_rows(output) %>%
  mutate(nivel = case_when(
    level == 1 ~ "PRIMARIA",
    TRUE ~ "SECONDARIA")) %>%
  rename("N" = 4,
         "r2" = 5) %>%
  dplyr::select(level, nivel, outcome, term, estimate, std.error, N, r2)

# 2

# Second Regression is done without restricting the sample




# reg_enrlmntdta_3 <- enrlmntdta %>% 
#   filter(level == 1) %>% 
#   mutate(q10 =  xtile(u10, wt = enrolled, n = 5))
# 
# reg_enrlmntdta_4 <- enrlmntdta %>% 
#   filter(level == 2) %>% 
#   mutate(q10 =  xtile(u10, wt = enrolled, n = 5))
# 
# reg_list_2 = list(reg_enrlmntdta_3, reg_enrlmntdta_4)
# 
# lapply(reg_list_2, function(df){
#   x <- felm(formula = needall ~ factor(q10) | factor(state) ,  data = df,
#             weights = df$enrolled)
#   # stargazer(x, type = "text")
# 
#   
#   y <- felm(formula = myopiaall ~ factor(q10) | factor(state), data = df,
#             weights = df$enrolled)
#   stargazer(y, type = "text")
#   
#   z <- felm(formula = wearsall ~ factor(q10) | factor(state), data = df,
#             weight = df$enrolled)
#   stargazer(z, type = "text")
# })

# detach(package:plyr)

reg_lists <- bind_rows(reg_list, reg_list_2) %>% 
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


# Method 2

library(plyr)

need_all = list()
myopia_all = list()
wears_all = list()

output_2 = list()
outcomes = c("needall","myopiaall","wearsall")

lapply(outcomes, function(i){
  reg <- reg_enrlmntdta %>% 
  group_by(level) %>% 
  do(model = tidy(felm(formula = i ~ factor(q10) | factor(state),  data = .,
                  weights = .$enrolled)))
  output_2[i] = reg})




# Method 2 
output_1 <- dlply(reg_enrlmntdta, "level", function(df){
  
 x <- felm(formula = needall ~ factor(q10) | factor(state),  data = df,
                  weights = df$enrolled)
 
  x = tidy(x)
  append(need_all, x)

  felm(formula = myopiaall ~ factor(q10) | factor(state),  data = df,
       weights = df$enrolled)
  felm(formula = wearsall ~ factor(q10) | factor(state),  data = df,
       weights = df$enrolled)

})

# ldply(output_1, coef)
#  l_ply(output_1, summary, .print = T)



# 
# output_2 <- dlply(reg_enrlmntdta_3, "level", function(df){
#   felm(formula = needall ~ factor(q10) | factor(state),  data = df,
#        weights = df$enrolled)
#   felm(formula = myopiaall ~ factor(q10) | factor(state),  data = df,
#        weights = df$enrolled)
#   felm(formula = wearsall ~ factor(q10) | factor(state),  data = df,
#        weights = df$enrolled)
# })
# 
# ldply(output_2, coef)
# l_ply(output_2, summary, .print = T)





# Questions

# L62 - Schools = enrolled, unsure what summary 


