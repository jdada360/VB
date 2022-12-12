#  This do-file combines the Ver Bien and GPA data in three steps:
# 
#     I   Perfect match
#     II  Imperfect match within the same classroom
#     III Imperfect match within the same school
# 
# Imperfect match is defined as when the "editing" distance divided by the 
# lentgh of the official name is lower than or equal to one third.


setwd("/Users/joydada/Library/CloudStorage/Box-Box/DIL Research Ver Bien")

states = c("AS","DG","GT","OA","SO","TM","YU")


output = lapply(states, function(state){
  # Step I
  read_dta(paste0("Data/5. Temp data/", state, " shift adjusted.dta")) %>% 
    inner_join(., read_dta(paste0("Data/5. Temp data/VB",
                         state, " shift adjusted.dta")),
               by = c("schoolyear","shift","schoolid",
                      "grade","group")) 
})
  
 
