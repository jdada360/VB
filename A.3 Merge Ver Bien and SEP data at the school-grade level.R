
setwd("~/Desktop/DIL/Ver Bien/Data")

# Time lag - execute scripts in sequence
# SEP_1520 <- read.xlsx("~/Desktop/DIL/Ver Bien/Data/Raw data/SEP 2015-2020.xlsx")

SEP_1520 <- SEP_clean %>% 
  apply_labels(grade = "Grade")

A <- SEP_1520 %>% 
  dplyr::select(c("schoolyear","schoolid","shift")) %>%
  mutate(schoolid = trimws(schoolid)) %>% 
  distinct() 

A_prime <- A %>% 
  get_dupes(c("schoolid","schoolyear"))

A <- anti_join(A, A_prime) %>% 
  rename("sep_shift" = shift)

# N = 662,301

# character distance  - levenschtein distance

# VB_1520 <- read.xlsx(file.path(path, "~/Desktop/DIL/Ver Bien/Data/Clean data/VB 2015-2020.xlsx"))

VB_1520 <- appended 

# Pablo - n =  1,010,767 % 1,022,710

# I am matching 29 less
# 29 additional entries not matched from VB_1520

mrg <- merge(VB_1520, A, by = c('schoolyear','schoolid'),
                 all.x = T) %>% 
  mutate(shift = sep_shift) %>% 
  dplyr::select(-"sep_shift") %>% 
  mutate(beneficiaries = 1,
         bengirls = ifelse(gender == "Female",1,0),
         benboys = ifelse(gender == "Male",1,0),
         myopia = ifelse((diagre %in% c("M","AMC","AMS") |
                            diagle %in% c("M","AMC","AMS")),
                         1,0),
         myogirls = ifelse(((myopia == 1) & (gender == "Female")),
                           1,0),
         myoboys = ifelse(((myopia == 1) & (gender == "Male")),
                          1,0),
         hasworngirls = ifelse(((haswornglasses == 1) & (gender == "Female")),
                               1,0),
         haswornboys = ifelse(((haswornglasses == 1) & (gender == "Male")),
                             1,0),
         wearsgirls = ifelse(((wearsglasses == 1) & (gender == "Female")),
                             1,0),
         wearsboys = ifelse(((wearsglasses == 1) & (gender == "Male")),
                             1,0)) %>% 
  mutate("grade" = as.numeric(grade)) %>% 
  group_by(schoolyear, schoolid, shift, grade) %>%
  summarise_at(c("beneficiaries","bengirls",
                 "benboys","myopia","myogirls",
                 "myoboys", "hasworngirls",
                 "haswornboys", "wearsgirls",
                 "wearsboys","wearsglasses","haswornglasses"), 
               sum, na.rm = TRUE) %>% 
  merge(., SEP_1520, 
             by = c("schoolyear","schoolid","shift","grade"),
        all.x = T) %>% 
  mutate("grade" = ifelse(grepl("SECUNDARIA", nivel), 
                          grade + 6,
                          grade),
         across(c((beneficiaries:wearsboys),
                  (enrolled:girls)),
                ~replace_na(.x, 0)))


# Pablos - 156,547 matched 
# Mine - 154,881 matched (1668 missing) if 
# shift = sep_shift, OR 159,231 (2684 extra) if sep_shift = shift

# L51 - 63 not included

# write_xlsx(mrg, file.path(path, "/Data/Clean data/2. Working Data\Enrollment, need 
# and use 2015-2020.xlsx"))
  


# Tabulations
# tab1(mrg$schoolyear)
# 
# count(x = mrg, schoolyear, wt = beneficiaries)
# 

# tab1(mrg$haswornboys)
# tab1(mrg$hasworngirls)
# tab1(mrg$beneficiaries)
# tab1(mrg$bengirls)
# tab1(mrg$benboys)
# tab1(mrg$myopia)
# tab1(mrg$myogirls)
# tab1(mrg$myoboys)
# wtd.table(merge$schoolyear, weights = merge$beneficiaries)
# table(mrg$grade, mrg$nivel)

  
