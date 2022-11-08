
#=======================================================
# This script produces basic descriptive tables and figures 
# regarding need and use of prescription glasses:
#
# - Table 1: number of students in the sample, by school year
# - Table 2: need and use by gender
# - Figure 1: need and use by gender and grade
# - Figure 2: distribution of the school-level need
# - Figure 3: need by state
#=======================================================

# enrlmnt_dta <- read.xlsx("~/Desktop/DIL/Ver Bien/Data/Clean data/2. Working Data\Enrollment, need 
# and use 2015-2020.xlsx"))

setwd("~/Desktop/DIL/Ver Bien/Data")

# egen tag = first instance of a unique combination is tagged
enrlmnt_dta <- mrg %>% 
  mutate("state" = substr(schoolid,1,2)) %>% 
  mutate("states" = as.integer(!duplicated(
           across(c(schoolyear, state)))),
         "schools" = as.integer(!duplicated(
      across(c(schoolyear, schoolid, turno))))) %>% 
  group_by(nivel, schoolyear) %>% 
  summarise_at(
    c("states","schools","enrolled","girls","boys",
      "beneficiaries","bengirls",
      "benboys","hasworngirls",
      "haswornboys", "wearsgirls",
      "wearsboys", "wearsglasses","haswornglasses"), 
    sum, na.rm = T) %>% 
  slice(rep(1:n(), each = 2)) %>% 
  ungroup() %>% 
  mutate(schoolyear = ifelse(
    (row_number() %% 2 == 1), "Total", schoolyear
  )) %>% 
  group_by(nivel, schoolyear) %>% 
  summarise_at(
    c("states","schools","enrolled","girls","boys",
      "beneficiaries","bengirls",
      "benboys","hasworngirls",
      "haswornboys", "wearsgirls",
      "wearsboys", "wearsglasses","haswornglasses"), 
    sum, na.rm = T) %>% 
  mutate("girl_pc" = round((girls/enrolled)*100, 1),
         "needgirl_pc" = round((bengirls/beneficiaries)*100, 1),
         "usegirl_pc" = round((wearsgirls/wearsglasses)*100, 1), 
         "pastusegirl_pc" = round((hasworngirls/haswornglasses)*100, 1))


# Table 1 : Focusing on girls

smry_tbl_1 <- enrlmnt_dta %>% 
  arrange(nivel, schoolyear, states, schools, enrolled, girl_pc, 
          beneficiaries, needgirl_pc, haswornglasses, pastusegirl_pc, 
          wearsglasses, usegirl_pc) %>% 
  dplyr::select(-c("girls":"wearsboys")) 

# write_xlsx(smry_tbl_1, file = "/Data/Output\A. Descriptive analysis.xlsx",
# sheetName="Raw Table 1", row.names=T)


# Table 2 : need and use by gender

smry_tbl_2 <- enrlmnt_dta %>% 
  mutate("nall_pc" = (beneficiaries/enrolled)*100,
         "ngirls_pc" = (bengirls/girls)*100,
         "nboys_pc" = (benboys/boys*100),
         "puall_pc" = (haswornglasses/beneficiaries)*100,
         "pugirls_pc" = (hasworngirls/bengirls)*100,
         "puboys_pc" = (haswornboys/benboys)*100,
         "uall_pc" = (wearsglasses/beneficiaries)*100,
         "ugirls_pc" = (wearsgirls/bengirls)*100,
         "uboys_pc" = (wearsboys/benboys)*100) %>% 
  mutate_at("nall_pc":"uboys_pc", suppressWarnings(round(., 1))) %>% 
  mutate("or_need" =  ngirls_pc/nboys_pc,
         "or_pastuse" = pugirls_pc/puboys_pc,
         "or_use" = ugirls_pc/uboys_pc,
         "or_total" = (wearsgirls/girls)/(wearsboys/boys)) %>% 
  mutate_at("or_need":"or_total", round(., 2)) %>% 
  dplyr::select(nivel, schoolyear, nall_pc:or_total) %>% 
  arrange(nivel, schoolyear, nall_pc, ngirls_pc, nboys_pc, or_need,
          puall_pc, pugirls_pc, puboys_pc, or_pastuse, uall_pc,
          ugirls_pc, uboys_pc, or_use) 
                               
# write_xlsx(smry_tbl_2, file ="/Data/Output\A. Descriptive analysis.xlsx",
# sheetName = "Raw Table 2", row.names=T) 


# Figure 1

fig_1_dta <- mrg %>%
  mutate("obs" = 1) %>% 
  group_by(grade) %>% 
  summarise_at(c("enrolled","girls","boys","beneficiaries","bengirls",
                 "benboys", "hasworngirls","haswornboys", "wearsgirls",
                 "wearsboys", "wearsglasses","haswornglasses","obs"),
               sum, na.rm = T) %>% 
  mutate("bb" = (benboys/boys)*100,
         "bg" = (bengirls/girls)*100,
         "hb" = (haswornboys/benboys)*100,
         "hg" = (hasworngirls/bengirls)*100,
         "wb" = (wearsboys/benboys)*100,
         "wg" = (wearsgirls/bengirls)*100)
  

# two way scatter of bb bg wrt to grade if grade in 1,6 
# then if grade in 7,9

# ggplot(filter(fig_1_dta,
#               grade %in% 1:6), aes(x=grade,
#                                    colour = gender)) +
#   geom_point(aes(y=bb, color = "beneficiaries boys"),
#             color = "red") +
#   geom_point(aes(y=bg, color = "beneficiaries girl"),
#             color = "green") +
#   labs(x = "Grade", y = "(%)", color = "Legend") +
# 
#   geom_point() 

# Figure 2

fig_2_dta <- mrg %>% 
  group_by(nivel, schoolyear, schoolid, shift) %>% 
  summarise_at(
    c("enrolled","beneficiaries"), 
    sum, na.rm = T) %>% 
  mutate("needall" = (beneficiaries/enrolled)*100,
         "schools" = 1) %>% 
  mutate("needcat" = (round((needall/2), 0)*2 + 1)) %>% 
  ungroup() %>% 
  group_by(nivel, needcat) %>% 
  summarise_at(
    c("schools","enrolled"),
    sum, na.rm = T) %>% 
  ungroup(needcat) %>% 
  mutate("total_enrolled" = sum(enrolled)) %>% 
  mutate("percent" = (enrolled/total_enrolled)*100,
         "zero" = 0)
  
         

# State averages

state_averages <- mrg %>% 
  mutate("state" = as.numeric(substr(schoolid,1,2)),
         "level" = as.numeric(as.factor(nivel))) %>% 
  group_by(level, state) %>% 
  summarise_at(c("enrolled","beneficiaries"),
               sum, na.rm = T) %>% 
  mutate("need" = (beneficiaries/enrolled)*100)

tbl <- binom.confint(state_averages$beneficiaries, 
                     state_averages$enrolled, conf.level=0.99, methods = "exact")

state_averages <- state_averages %>% 
  bind_cols(tbl$lower, tbl$upper) %>% 
  rename("lb" = 6,
         "ub" = 7) %>% 
  mutate(lb = lb*100,
         ub = ub*100) %>% 
  dplyr::select(c("state","need","level","ub","lb")) %>% 
  pivot_wider(names_from = level, 
              values_from = c("need","ub","lb")) %>% 
  filter(!is.na(need_1) & !is.na(need_2))
  

# Figure 4

fig_4_dta <- mrg %>% 
  group_by(grade) %>% 
  summarise_at(c("enrolled","girls","boys","beneficiaries","bengirls",
                 "benboys", "myoboys","myogirls","myopia"),
               sum, na.rm = T) %>% 
  mutate(mboys = (myoboys/boys)*100,
         mgirls = (myogirls/girls)*100,
         m = (myopia/enrolled)*100)
  


# Figure 4




