
setwd("~/Desktop/DIL/Ver Bien/Data/Raw data")

# Import from Excel

csvfiles <- list.files(pattern="Primaria y secundaria")
import <- lapply(csvfiles,read.csv)
names(import) <- gsub("\\.csv$","",csvfiles)


# Rename choice variables and convert to string

SEP_clean <- lapply(import, function(x){
  x <- janitor::clean_names(x)
  tibble(x) 
  setnames(x, old = c("clavecct", "n_turno"),
           new = c("schoolid","shift"))
    x <- dplyr::select(x, starts_with(c("insc","muj","hom")),
                  "nivel","schoolid","turno","shift") 
  x <- rename_with(x,function(y){
    (suppressWarnings(stri_replace_all_regex(y,"insc","enrolled")))}) 
  x <- rename_with(x,function(y){
    (suppressWarnings(stri_replace_all_regex(y,"muj","girls")))})
  rename_with(x,function(y){
    (suppressWarnings(stri_replace_all_regex(y,"hom","boys")))})
  })

# Reshape enrolled, grade and boys columns to long

SEP_clean <- lapply(SEP_clean, function(x){
  x <- x %>% 
    dplyr::select(-ends_with("_t")) %>% 
    mutate(across("nivel", trimws)) %>% 
    group_by(nivel, schoolid, shift, turno) %>% 
    summarise_at(1:18,
                 sum,
                 na.rm = TRUE) %>%
    long_panel(., prefix = "_", label_location = "end", begin = 1,
               end = 6, wave = "grade") %>% 
    replace(is.na(.), 0) 
  }) 



# sum(with(SEP_clean[["Primaria y secundaria 2016"]],
#             (enrolled != girls + boys)))


# Add school year column

i = 2015
for(j in 1:5){
  if (i == 2020){break}
  SEP_clean[[j]] <- SEP_clean[[j]] %>% 
    add_column(schoolyear = paste(i,"-",i+1))
  i <- i+1
}

# Here we append the fives school years

# Pivoting 

SEP_clean <- bind_rows(SEP_clean) %>% 
  mutate("grade" = as.numeric(grade),
         "shift" = ifelse((grepl("CONTINUO",shift) |
                             grepl("AMPLIAD", shift)),
                          "CONTINUO", shift)) %>%
  filter(!((grepl("SECUNDARIA", nivel)) & (grade %in% 4:6))) %>% 
  ungroup() %>% 
  dplyr::select(-"id") 

# write.xlsx(clean,"SEP 2015-2020.xlsx")

# N =  3,504,774

#tab1(SEP_clean$"schoolyear")
#tab1(SEP_clean$grade)
#tab1(SEP_clean$nivel)

# Audit
# No differences.



