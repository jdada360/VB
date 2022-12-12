# This script prepares the Ver Bien data.
# It creates a data set with beneficiaries 
# in the school years and states for which 
# we have GPA information.

setwd("Data/Raw data")

# VER BIEN 2014-2015

VB_1415 <- read_dta("CICLO 2014-2015_1.DTA") 

clean_15 <- zap_formats(VB_1415) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  dplyr::select(-c("id_estado",
                   "municipio",
                   "esfera_der":"dip",
                   "arm",
                   "c30","c31")) %>% 
  setnames(., old = c("clave","turno",
                      "grado","grupo",
                      "nombre_s","ap_paterno",
                      "ap_materno","edad",
                      "sexo"),
           new = c("schoolid","shift",
                   "grade","group",
                   "givenname","paternallastname",
                   "maternallastname","age","gender")) %>% 
  mutate(gender = case_when(
    grepl("H",toupper(gender)) ~ "Male",
    grepl("M", toupper(gender)) |  grepl("F", toupper(gender)) ~ "Female",
    TRUE ~ "Error"),
    age = as.numeric(age)) %>% 
  mutate_at(12:15,
            ~suppressWarnings(as.numeric(.x))) %>% 
  setnames(., old = c("av_s_rx_od",
                      "av_s_rx_oi",
                      "av_c_rx_od",
                      "av_c_rx_oi", "hace_cuanto_tiempo_usa_lentes"),
           new = c("vadre_withoutrx",
                   "vadle_withoutrx",
                   "vadre_withrx","vadle_withrx",
                   "yearswearingglasses")) %>% 
  mutate(diagre = trimws(diagnostico_der),
         diagle = trimws(diagnostico_izq),
         yearswearingglasses = as.numeric(yearswearingglasses)) %>% 
  dplyr::select(-starts_with("diagnostico")) %>%
  mutate_at(c("givenname","schoolid", "shift",
              "paternallastname",
              "maternallastname",
              "ha_utilizado_lentes_anteriorment",
              "usa_lentes_actualmente",
              "sus_lentes_son_de_vbpam"),
            ~(toupper(trimws(as.character(.x))))) %>% 
  mutate(haswornglasses = ifelse(grepl("SI",
                                ha_utilizado_lentes_anteriorment),
                                1,0),
         wearsglasses = ifelse(grepl("SI",
                                     usa_lentes_actualmente),
                               1,0),
         verbienglasses = ifelse(grepl("SI",
                                       sus_lentes_son_de_vbpam),
                                 1,0),
         eyeexamdate = NA_Date_) %>%
  mutate(schoolyear = paste0(2014,"-",2015),
         eyeexamdate = as.POSIXct(eyeexamdate,
                                  tz = "UTC", format = "%Y/%m/%d"))  %>% 
  dplyr::select(c('schoolyear','schoolid','shift','givenname','paternallastname',
                  'maternallastname','age','gender','grade','group','vadre_withoutrx',
                  'vadle_withoutrx','vadre_withrx','vadle_withrx','diagre','diagle',
                  'haswornglasses','wearsglasses','yearswearingglasses','verbienglasses',
                  'eyeexamdate')) 
 
# Tabulations
table(clean_15$gender)

# yearswearingglasses  not cleaned in Pablo's
# Export clean data frame

# Ver Bien 2013-2014

VB_1314 <- read_xlsx("BASE DE DATOS CICLO 2013-2014.xlsx")

clean_14 <-  zap_formats(VB_1314) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  dplyr::select(-c("id_estado","municipio",
                   "esfera_der":"dip","arm",
                   "optometrista","periodo")) %>% 
  setnames(., old = 1:20,
           new = c("schoolid",
                   "shift","givenname","paternallastname",
                   "maternallastname","age","gender",
                   "grade","group","diagre","diagle",
                   "vadre_withoutrx","vadle_withoutrx",
                   "vadre_withrx","vadle_withrx",
                   "haswornglasses","wearsglasses",
                   "yearswearingglasses","verbienglasses","eyeexamdate")) %>%
  mutate(gender = case_when(
    grepl("H", toupper(gender)) ~ "Male",
    grepl("M", toupper(gender)) | grepl("F", toupper(gender)) ~ "Female",
    TRUE ~ "Error"),
    age = as.numeric(age)) %>% 
  mutate_at(c(12:15,16:19),
            ~suppressWarnings(as.numeric(.))) %>% 
  mutate_at(10:11,
            ~trimws(toupper(.x))) %>% 
  mutate(haswornglasses = as.numeric(haswornglasses),
         wearsglasses = as.numeric(haswornglasses),
         verbienglasses = as.numeric(verbienglasses))  %>% 
  mutate(schoolyear = paste0(2013,"-", 2014),
         schoolid = as.character(schoolid),
         shift = as.character(shift),
         yearswearingglasses = as.numeric(yearswearingglasses),
         eyeexamdate = as.POSIXct(eyeexamdate,
                                  tz = "UTC", format = "%Y/%m/%d")) %>% 
  dplyr::select(c('schoolyear','schoolid','shift','givenname','paternallastname',
                  'maternallastname','age','gender','grade','group','vadre_withoutrx',
                  'vadle_withoutrx','vadre_withrx','vadle_withrx','diagre','diagle',
                  'haswornglasses','wearsglasses','yearswearingglasses','verbienglasses',
                  'eyeexamdate'))
  
  
# VER BIEN 2012-2013


VB_1213 <- read_dta("VB 1213.dta")

clean_13 <- zap_formats(VB_1213) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  dplyr::select(-c("id_estado","municipio",
                   "esfera_der":"dip","arm")) %>% 
  setnames(., old = 1:11,
           new = c("schoolid","shift","givenname",
                   "paternallastname", "maternallastname",
                   "age","gender","grade","group","diagre",
                   "diagle")) %>% 
  mutate(haswornglasses = NA_real_,
         wearsglasses = NA_real_,
         yearswearingglasses = NA_real_,
         verbienglasses = NA_real_,
         eyeexamdate = NA_character_,
         vadre_withoutrx = NA_real_,
         vadre_withrx = NA_real_,
         vadle_withoutrx = NA_real_,
         vadle_withrx = NA_real_) %>% 
  mutate_at(c(3:5, 9:11), 
            ~(substr(trimws(toupper(.x)),1,20))) %>% 
  mutate_at(c(1:2, 9),
            ~(trimws(toupper(.x))))  %>% 
  mutate(schoolyear = paste0(2012,"-", 2013),
         shift = as.character(shift),
         schoolid = as.character(schoolid),
         age = as.numeric(age),
         eyeexamdate = as.POSIXct(eyeexamdate,
                                  tz = "UTC", format = "%Y/%m/%d")) %>% 
  dplyr::select(c('schoolyear','schoolid','shift','givenname','paternallastname',
                  'maternallastname','age','gender','grade','group','vadre_withoutrx',
                  'vadle_withoutrx','vadre_withrx','vadle_withrx','diagre','diagle',
                  'haswornglasses','wearsglasses','yearswearingglasses','verbienglasses',
                  'eyeexamdate')) 
  
  
# VER BIEN 2012-2020

VB_1520 <- read_dta("VB 2015-2020.dta")

early_years = c("2012-2013",
               "2013-2014","2014-2015")

  clean_1220 <- bind_rows(zap_formats(VB_1520),
                          clean_15,clean_14, clean_13) %>% 
    mutate(state = as.numeric(substr(schoolid, 1, 2)),
           N = 1) %>% 
    filter(state %in% c(1,10,11,20,26,28,31)) %>%
    filter(!(state %in% c(1,10,11,20,26,28,31) & 
               schoolyear %in% early_years)) %>% 
    mutate_at(2:6,
              ~(trimws(toupper(as.character(.x))))) %>% 
    filter(
      (grepl("[0-9]", (substr(schoolid,1,2)))),
      !(substr(schoolid,3,5) == "PBH"), # PBH = Centro de Bachillerato General
      !(substr(schoolid,3,5) == "EBH"), # EBH = Centro de Bachillerato General
      !(substr(schoolid,3,5) == "UBH"), # UBH = Centro de Bachillerato General
      !(substr(schoolid,3,5) == "PCB"), # PCB = Colegio de Bachilleres
      !(substr(schoolid,3,5) == "ECB"), # ECB = Colegio de Bachilleres
      !(substr(schoolid,3,5) == "XTA"), # XTA = Centro de Bachillerato Tecnológico 
      # Agropecuario o Forestal
      !(substr(schoolid,3,5) == "DTA"), #  DTA = Centro de Bachillerato Tecnológico 
      # Agropecuario o Forestal
      !(substr(schoolid,3,5) == "ETK"), # ETK = Telebachillerato Comunitario
      !(substr(schoolid,3,5) == "DPT"), # DPT = Colegio Nacional de Educación Profesional 
      # Técnica
      !(substr(schoolid,3,5) == "DCT"), # DCT = Centro de Estudios Tecnológicos Industrial 
      # y de Servicios
      !(substr(schoolid,3,5) == "EET"), # EET = Centro de Estudios Tecnológicos 
      # Industrial y de Servicios
      !(substr(schoolid,3,5) == "DML"), # DML = Centro Múltiple de Educación Especial
      !(substr(schoolid,3,5) == "EML"), # EML = Centro Múltiple de Educación Especial
      !(substr(schoolid,3,5) == "DNS"), # Escuela Normal Superior de Educación Secundaria
      !(substr(schoolid,3,5) == "ETC"), # Colegio de Estudios Científicos y 
      # Tecnológicos del Estado
      !(substr(schoolid,3,5) == "KJN"), # Preescolar Comunitario
      !(substr(schoolid,3,5) == "INE"), # ?
      !(substr(schoolid,4,5) == "JN")) %>%   # Kindergarten
      mutate(givenname = substr(givenname, 1, 50))
  
table(clean_1220$schoolyear,
      clean_1220$N)

table(clean_1220$state,
      clean_1220$schoolyear)
  # 
  
  # 
  # 
  # %>% 
  #   apply_labels(haswornglasses = c("Yes" = 1, "No" = 0),
  #                wearsglasses=c("Yes" = 1, "No" = 0),
  #                verbienglasses=c("Yes" = 1, "No" = 0),
  #                schoolid = "School ID (CCT)",
  #                shift = "School shift (turno)",
  #                group = "Group",
  #                grade = "Grade",
  #                givenname = "Given name(s)",
  #                paternallastname = "Paternal last name",
  #                maternallastname = "Maternal last name",
  #                age = "Age in years",
  #                gender = "Gender",
  #                diagre = "Diagnosis - right eye",
  #                diagle ="Diagnosis - left eye",
  #                vadre_withoutrx = "Visual acuity deficit without 
  #              prescription - right eye",
  #                vadle_withoutrx = "Visual acuity deficit
  #              without prescription - left eye",
  #                vadre_withrx = "Visual acuity deficit with prescription - 
  #              right eye",
  #                vadle_withrx = "Visual acuity deficit with prescription - left eye",
  #                haswornglasses = "Student has worn prescription glasses before",
  #                wearsglasses = "Student currently wears prescription glasses",
  #                yearswearingglasses = "Years wearing glasses",
  #                verbienglasses = "Student's current glasses are from Ver Bien",
  #                eyeexamdate = "Date of eye exam") %>% 
  
  