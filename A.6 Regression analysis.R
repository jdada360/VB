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
         ln_enrolled = log(enrolled),
         level = as.numeric(as.factor(nivel))) 

# First Regression is done by restricting sample to u0<=1

reg_dta_1 <- lapply(c(1:2), function(i){
reg_enrlmntdta <- enrlmntdta %>% 
  ungroup() %>% 
  filter(u0 <= 1,
         level == i) %>% 
  group_by(nivel) %>% 
  mutate(q10 = xtile(u10, n = 5, wt = enrolled)) 
print(table(reg_enrlmntdta$q10))
list(reg_enrlmntdta)
})

output <- lapply(seq_along(reg_dta_1), function(df){
  x <- felm(formula = needall ~ factor(q10) | state , 
            data = reg_list[[df]],
            weights = reg_list[[df]][["enrolled"]])
  
  print(summary(x))
  
  x <-  bind_cols(tidy(x)[1:3], nobs(x), summary(x, robust = TRUE )$r.squared, level = df,
                  outcome = "needall")
  
  y <- felm(formula = myopiaall ~ factor(q10) | factor(state), data = reg_list[[df]],
            weights = reg_list[[df]][["enrolled"]])
  
  print(summary(y))
  
  y <- bind_cols(tidy(y)[1:3], nobs(y), summary(y, robust = TRUE )$r.squared,
                 level = df, outcome = "myopiaall")
  
  z <- felm(formula = wearsall ~ factor(q10) | factor(state), data = reg_list[[df]],
            weights = reg_list[[df]][["enrolled"]])
  
  print(summary(z))
  
  z <-  bind_cols(tidy(z)[1:3], nobs(z), summary(z, robust = TRUE )$r.squared,
                  level = df,
                  outcome = "wearsall")
  
  list(x, y, z)})

# Clustered Standard Errors - when I cluster by state and q10 I get different
# standard errors than stata.

output_reg_1 <- bind_rows(output) %>%
  mutate(nivel = case_when(
    level == 1 ~ "PRIMARIA",
    TRUE ~ "SECONDARIA")) %>%
  setnames(., old = c("...4","...5"),
           new = c("N","r2")) %>% 
  dplyr::select(level, nivel, outcome, term, estimate, std.error, N, r2) %>% 
  mutate(method = "A")


need_reg_1 <- output_reg_1 %>% 
  filter(outcome == "needall")

myopia_reg_1 <- output_reg_1 %>%
  filter(outcome == "myopiaall")

wear_reg_1 <- output_reg_1 %>% 
  filter(outcome == "wearsall")


# Second Regression is done without restricting sample to u0<=1


reg_dta_2 <- lapply(c(1:2), function(i){
  reg_enrlmntdta <- enrlmntdta %>% 
    ungroup() %>% 
    filter(level == i) %>% 
    mutate(q10 = xtile(u10, n = 5, wt = enrolled)) 
  # print(table(reg_enrlmntdta$q10))
})


output_2 <- lapply(seq_along(reg_dta_2), function(df){
  a <- felm(formula = needall ~ factor(q10) | state,  
            data = reg_dta_2[[df]],
            weights = reg_dta_2[[df]][["enrolled"]])
  
  print(summary(a))
  
  a <-  bind_cols(tidy(a)[1:3], nobs(a), summary(a)$r.squared, level = df,
                  outcome = "needall")
  
  b <- felm(formula = myopiaall ~ factor(q10) | state, 
            data = reg_dta_2[[df]],
            weights = reg_dta_2[[df]][["enrolled"]])
  
  print(summary(b))
  
  b <- bind_cols(tidy(b)[1:3], nobs(b), summary(b)$r.squared,
                 level = df, outcome = "myopiaall")
  
  c <- felm(formula = wearsall ~ factor(q10) | state,
            data = reg_dta_2[[df]],
            weights = reg_dta_2[[df]][["enrolled"]])
  
  print(summary(c))
  
  c <-  bind_cols(tidy(c)[1:3], nobs(c), summary(c)$r.squared,
                  level = df,
                  outcome = "wearsall")
  
  list(a, b, c)})

output_reg_2 <- bind_rows(output_2) %>%
  mutate(nivel = case_when(
    level == 1 ~ "PRIMARIA",
    TRUE ~ "SECONDARIA")) %>%
  setnames(., old = c("...4","...5"),
           new = c("N","r2")) %>% 
  dplyr::select(level, nivel, outcome, term, estimate, std.error, N, r2) %>% 
  mutate(method = "B")


need_reg_2 <- output_reg_2  %>% 
  filter(outcome == "needall") %>% 
  bind_rows(need_reg_1)

myopia_reg_2 <- output_reg_2 %>%
  filter(outcome == "myopiaall") %>% 
  bind_rows(myopia_reg_2)

wear_reg_2 <- output_reg_2  %>% 
  filter(outcome == "wearsall") %>% 
  bind_rows(wear_reg_2)

sheet_names = list('Sheet1' = need_reg_2,
                   'Sheet2' = myopia_reg_2,
                   'Sheet3' = wear_reg_2)

write.xlsx(sheet_names, file = 'Regression Results.xlsx', append = TRUE)


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
  



# Method 2

# library(plyr)

# output_1 <- dlply(reg_enrlmntdta, "level", function(df){
#   
#   felm(formula = needall ~ factor(q10) | factor(state),  data = df,
#                   weights = df$enrolled)
# 
#   felm(formula = myopiaall ~ factor(q10) | factor(state),  data = df,
#        weights = df$enrolled)
#   felm(formula = wearsall ~ factor(q10) | factor(state),  data = df,
#        weights = df$enrolled)
# 
# })

# ldply(output_1, coef)
#  l_ply(output_1, summary, .print = T)



# Audit

# Convert SE to Robust SE to match Pablos

# L62 - Schools = enrolled. Unsure how to replicate these summary
# statistics since I dont know what is being summarised for schools and enrolled.

# Same results i.e. same coefficients.



