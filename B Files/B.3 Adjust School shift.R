# This script prepares the Ver Bien and GPA data to be combined.
# It adjusts the shift f schools to make them compatible. Whenever
# a school has a single shift, that is reflected in both data sets.

setwd(file.path(path, "Data"))

VB_data = read_dta("Temp Data/A. Ver Bien.dta")
VB_data <- zap_labels(VB_data)

states = c("AS","DG","GT","OA","SO","TM","YU")

v = case_when(
  states == "AS" ~ 1,
  states == "DG" ~ 10,
  states == "GT" ~ 11,
  states == "OA" ~ 20,
  states == "SO" ~ 26,
  states == "TM" ~ 28,
  states == "YU" ~ 31
)

for (state in states){
  y <- VB_data %>% 
    filter(state == v[[which(states == state)]])
  
  read_dta(paste0("Working data/",state, " GPA visited schools.dta")) %>% 
    mutate(N = 1) %>% 
    group_by(schoolid, shift) %>% 
    summarise(sum = sum(N)) %>% 
    ungroup()%>%
    group_by(schoolid) %>% 
    mutate(shifts = count(N)) %>% 
    filter(shifts == 1) %>% 
    dplyr::select(schoolid) %>% 
    inner_join(., y, by = 'schoolid')
  # write_dta(x, "Temp Data/", state, "single shift school ID.dta")
}