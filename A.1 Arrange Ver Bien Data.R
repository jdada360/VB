
# Creating path to Ver Bien's 2019-2020 data

d_path <- file.path(path,"/Data/Raw data/2019-2020.xlsx")

#Finds sheet names and creates separate data frames for each sheet


sheets <- openxlsx::getSheetNames(d_path)
VB_1920 <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=d_path,
                  colNames = F)
  
# lapply(sheets, read_excel, path = d_path, col_types = "text")


# Assigning names to the data frames

names(VB_1920) <- c("VB_1920_1","VB_1920_2","VB_1920_3","VB_1920_4")
VB_1920_raw <- VB_1920 

# Rename columns individually and then append them

col_names <- c("planeación","bloque","estado","optometrista","cct","escuela",
                "turno","rx","nombre","apellidopaterno",
                "apellidomaterno","grado","grupo","edad","sexo","rxespecialmaterial",
                "rxespecialdiseño","esferad","cilindrod","ejed","alturad","adiciónd",
                "diagnósticod","esferai","cilindroi","ejei","alturai","adicióni", "diagnósticoi","dip",
                "dipcerca","armazónclave","armazón","avsrxod","avsrxoi","avcrxod","avcrxoi",
                "patologíavisual","comentarios","hautilizadoantes","usaslentesactualmente",
                "cuántotiempohausadolentesa","cuántotiempohausadolentesm","suslentessondeverbien","estatus","fechahora",
                "lote","municipio","fecha","hora","usuario",
               "categoría", "fechacompleta") 


VB_1920[2:4] <- lapply(VB_1920_raw[2:4], setNames, nm = col_names)


VB_1920[["VB_1920_1"]]<- VB_1920_raw[["VB_1920_1"]] %>%
  slice(-1) %>% 
  dplyr::select(-(c(8:9, 56:57)))  %>% 
  setNames(nm = col_names) %>% 
  mutate_at(c("rx","grado","edad","esferad","cilindrod","ejei","ejed",
              "alturai","alturad","adicióni","dip","dipcerca","cuántotiempohausadolentesa",
              "fecha","cuántotiempohausadolentesm","adiciónd","esferai",
              "cilindroi"),
            as.numeric)

VB_1920 <- VB_1920[["VB_1920_1"]] %>% 
  bind_rows(VB_1920[["VB_1920_2"]],
                  VB_1920[["VB_1920_3"]],
                  VB_1920[["VB_1920_4"]])

# 2019-2020

# N = 249,072 vs 249,072 

clean_20 <- VB_1920 %>% 
  tibble() %>%
  janitor::clean_names() %>%
  zap_label() %>% 
  setNames(., nm = c(col_names,"usuario","categoría", "fechacompleta", 
                     "tipodebeneficiario","númerodeempleado")) %>% 
  dplyr::select("cct","turno","grado","grupo","nombre","edad", "sexo",
                "hautilizadoantes","fechahora",
                starts_with(c("apellido","avs","avc","diag","usa","sus",
                              "cuánto"))) %>%
  mutate(across(c("diagnósticod","diagnósticoi"),
                toupper),
         across(c("diagnósticod","diagnósticoi","sexo",
                  "cuántotiempohausadolentesa","cuántotiempohausadolentesm",
                  "hautilizadoantes","suslentessondeverbien"),
                trimws),
         avsrxod = suppressWarnings(as.numeric(avsrxod)),
         avsrxoi = suppressWarnings(as.numeric(avsrxoi)),
         avcrxod = suppressWarnings(as.numeric(avcrxod)),
         avcrxoi = suppressWarnings(as.numeric(avcrxoi)),
         haswornglasses = ifelse(hautilizadoantes == "Sí",1,0),
         wearsglasses = ifelse(usaslentesactualmente == "Sí",1,0),
         verbienglasses = ifelse(suslentessondeverbien == "Sí",1,0),
         yearswearingglasses = as.numeric(cuántotiempohausadolentesa),
         monthswearingglasses = round(as.numeric(cuántotiempohausadolentesm), 0),
         eyeexamdate = substr(fechahora,1,10)) %>%
  setnames(., old = c("cct","turno","grado","grupo","nombre","apellidopaterno",
                      "apellidomaterno","edad","sexo","avsrxod","avsrxoi",
                      "avcrxod","avcrxoi"), 
           new = c("schoolid","shift","grade","group","givenname","paternallastname",
                   "maternallastname","age","gender","vadre_withoutrx",
                   "vadle_withoutrx","vadre_withrx","vadle_withrx")) %>% 
  mutate(gender = trimws(toupper(gender))) %>% 
  mutate(gender =
           case_when(
             grepl("MASC", gender) ~ "Male",
             grepl("FEM", gender) ~ "Female",
             TRUE ~ "Error"),
         diagre = case_when(
           (grepl("ASTIGMATISMO HIPERMETROPICO COMPUESTO",diagnósticod)) ~ "AHC",
           (grepl("ASTIGMATISMO HIPERMETROPICO SIMPLE",diagnósticod)) ~ "AHS",
           (grepl("ASTIGMATISMO MIOPICO COMPUESTO",diagnósticod)) ~ "AMC",
           (grepl("ASTIGMATISMO MIOPICO SIMPLE",diagnósticod)) ~"AMS",
           (grepl("ASTIGMATISMO MIXTO", diagnósticod)) ~ "AM",
           (grepl("EMETROPE",diagnósticod)) ~ "E",
           (grepl("HIPERMETROPIA",diagnósticod)) ~  "H",
           (grepl("MIOPIA",diagnósticod)) ~ "M",
           TRUE ~ diagnósticod),
          diagle = case_when(
           diagnósticoi == "ASTIGMATISMO HIPERMETROPICO COMPUESTO" ~ "AHC",
           diagnósticoi == "ASTIGMATISMO HIPERMETROPICO MIXTO" ~ "AHM",
           diagnósticoi == "ASTIGMATISMO HIPERMETROPICO SIMPLE" ~ "AHS",
           diagnósticoi == "ASTIGMATISMO MIOPICO COMPUESTO" ~ "AMC",
           diagnósticoi == "ASTIGMATISMO MIOPICO SIMPLE" ~"AMS",
           diagnósticoi == "ASTIGMATISMO MIXTO" ~ "AM",
           diagnósticoi == "EMETROPE" ~ "E",
           diagnósticoi == "HIPERMETROPIA" ~  "H",
           diagnósticoi == "MIOPIA" ~ "M",
           TRUE ~ diagnósticoi)) %>%
  dplyr::select(-c("diagnósticoi","diagnósticod")) %>%
  mutate(eyeexamdate = trimws(as.character(eyeexamdate)),
         shift = substring(shift,1,10),
         schoolyear = paste(2019,"-",2020)) %>% 
  mutate_at(3:5, ~substr(toupper(.x), 1,50)) %>% 
  mutate_at(c(3,12:14),
            ~suppressWarnings(as.numeric(.x))) %>%
  dplyr::select("schoolid","shift","grade","group","givenname",
                "paternallastname","maternallastname",
                "age","gender","diagre","diagle","vadre_withoutrx",
                "vadle_withoutrx", "vadre_withrx",
                "vadle_withrx","haswornglasses",
                "wearsglasses","yearswearingglasses",
                "verbienglasses","eyeexamdate","schoolyear")

# "monthswearingglasses",
# dev.off()
  # mutate(eyeexamdate = as.Date(trimws(eyeexamdate), format = "%d/%m/%y")) %>% 
# N = 249,069
# <NA> = 18354 diagre and diagle

# Tabulations

# tab1(clean_20$gender)
# tab1(VB_1920$diagnósticod)
# tab1(VB_1920$diagnósticoi)
# tab1(clean_20$"diagre")
# tab1(clean_20$"diagle")


# or table(clean_20$"diagle")
# 
# tab1(VB_1920$"hautilizadoantes")
# tab1(clean_20$haswornglasses)
# 
# tab1(VB_1920$"usaslentesactualmente")
# tab1(clean_20$wearsglasses)
# 

# table_1920 <- clean_20 %>%
#   filter(age %in% 5:15) %>%
#   mutate_at(., c("vadle_withoutrx","vadre_withoutrx"),
#             ~replace(., is.na(.), 0))%>%
#   group_by(age, gender) %>%
#   summarise(mean_vadre_withoutrx = mean(vadre_withoutrx),
#             mean_vadle_withoutrx = mean(vadle_withoutrx))

# write_xlsx(clean_20, file.path(path, "/Data/Clean data/VB 2019-2020.xlsx"))


#2018-2019

VB_1819 <- read_dta(file.path(path, "/Data/Raw data/VB 1819.dta"))

# N = 237,866

clean_19 <- VB_1819 %>%
  tibble() %>%
  janitor::clean_names() %>%
  zap_label() %>% 
  zap_formats(.) %>% 
  dplyr::select(c(3:11, 19:20, 22:29, 40)) %>%
  mutate(schoolid = substr(clave,1,10),
         yearswearingglasses = as.numeric(hacecuantotiempousalentes)) %>%
  setnames(., old = c("turno","grado","grupo","edad","sexo","fecha",
                      "diagnostico_der","diagnostico_izq","nombres",
                      "ap_paterno","ap_materno","avsrxod","avsrxoi",
                      "avcrxod","avcrxoi"),
           new = c("shift","grade","group","age","gender","eyeexamdate",
                   "diagre","diagle","givenname","paternallastname",
                   "maternallastname", "vadre_withoutrx",
                   "vadle_withoutrx","vadre_withrx","vadle_withrx")) %>%
  mutate(shift = substr(shift,1,10),
         across(c(3:5,7,10:11,20), ~as.character(trimws(toupper(.x)))),
         haswornglasses = ifelse(hautilizadolentesanteriorment == "NO",0,1),
         wearsglasses = ifelse(usalentesactualmente == "NO",0,1),
         verbienglasses = ifelse(suslentessondevbpam =="NO",0,1),
         yearswearingglasses = 
           suppressWarnings(as.numeric(hacecuantotiempousalentes)),
         gender = 
           case_when(
             "H" == gender ~ "Male", # typo of the original?
             "M" == gender ~ "Female",
             TRUE ~ "Error")) %>% 
  mutate_at(12:15, ~suppressWarnings(as.numeric(.x))) %>% 
  mutate_at(3:5, ~substr(.x, 1,20)) %>% 
  dplyr::select(c("schoolid","shift","grade","group","givenname",
                "paternallastname","maternallastname",
                "age","gender","diagre","diagle","vadre_withoutrx",
                "vadle_withoutrx", "vadre_withrx",
                "vadle_withrx","haswornglasses",
                "wearsglasses","yearswearingglasses",
                "verbienglasses","eyeexamdate")) %>%
  mutate(schoolyear = paste(2018,"-",2019))

# mutate(eyeexamdate = as.Date(eyeexamdate, "%d/%m/%y")) %>%

# Tabulations

# tab1(clean_19$gender)
# tab1(VB_1819$"DiagnosticoDer")
# tab1(VB_1819$"DiagnosticoIzq")
# tab1(clean_19$"diagre")
# tab1(clean_19$"diagle")
# 
# tab1(VB_1819$"SUSLENTESSONDEVBPAM")
# tab1(clean_19$verbienglasses)
# 
# tab1(VB_1819$"HAUTILIZADOLENTESANTERIORMENT")
# tab1(clean_19$haswornglasses)
# 
# tab1(VB_1819$"USALENTESACTUALMENTE")
# tab1(clean_19$wearsglasses)
# 
# table_1819 <- clean_19 %>%
#   filter(age %in% 5:15) %>% 
#   mutate_at(., c("vadle_withoutrx","vadre_withoutrx"),
#             ~replace(., is.na(.), 0))%>% 
#   group_by(age, gender) %>%
#   summarise(mean_vadre_withoutrx = mean(vadre_withoutrx),
#             mean_vadle_withoutrx = mean(vadle_withoutrx))


# write_xlsx(clean_19, file.path(path, "/Data/Clean data/VB 2018-2019.xlsx"))
  


# 2017-2018

VB_1718 <- read_dta(file.path(path, "/Data/Raw data/VB 1718.dta"))

# N = 189,761
clean_18 <- VB_1718 %>%
  tibble() %>%
  janitor::clean_names() %>%
  zap_label() %>% 
  zap_formats(.) %>% 
  dplyr::select("clave","turno","nombres","ap_paterno","ap_materno","edad",
                "sexo","grado","grupo","fecha",
                starts_with(c("av","diagnostico","sus","ha","usa"))) %>%
  setnames(., old = c("clave","turno","grado","grupo","nombres",
                      "ap_paterno","ap_materno",
                      "edad","sexo","fecha","avsrxod","avsrxoi","avcrxod","avcrxoi",
                      "hacecuantotiempousalentes","diagnostico_der",
                      "diagnostico_izq"),
           new = c("schoolid","shift","grade","group","givenname","paternallastname",
                   "maternallastname","age","gender","eyeexamdate","vadre_withoutrx",
                   "vadle_withoutrx", "vadre_withrx","vadle_withrx","yearswearingglasses","diagre","diagle")) %>% 
  mutate(across(c(ends_with("name"),starts_with("diag"),"gender","date",
                  "usalentesactualmente",
                  "hautilizadolentesanteriorment","suslentessondevbpam"),
                ~as.character(trimws(toupper(.x)))),
         haswornglasses = ifelse(grepl("NO",hautilizadolentesanteriorment),0,1),
         wearsglasses = ifelse(grepl("NO", usalentesactualmente),0,1),
         verbienglasses = ifelse(grepl("NO",suslentessondevbpam),0,1),
         gender = 
           case_when(
             gender == "M" ~ "Male",
             gender == "H" ~ "Female",
             TRUE ~ "Error"),
         shift = substr(shift,1,10),
         across(ends_with("name"), ~substr(.x, 1, 20)),
       yearswearingglasses = suppressWarnings(as.numeric(yearswearingglasses))) %>%
  mutate_at(c(11:14,19), ~suppressWarnings(as.numeric(.x))) %>% 
  dplyr::select("schoolid","shift","grade","group","givenname",
                "paternallastname","maternallastname",
                "age","gender","diagre","diagle","vadre_withoutrx",
                "vadle_withoutrx", "vadre_withrx",
                "vadle_withrx","haswornglasses",
                "wearsglasses","yearswearingglasses",
                "verbienglasses","eyeexamdate") %>%
  mutate(schoolyear = paste(2017,"-",2018))


# Tabulations

# tab1(clean_18$gender)
# tab1(VB_1718$"DiagnosticoDer")
# tab1(VB_1718$"DiagnosticoIzq")
# tab1(clean_18$"diagre")
# tab1(clean_18$"diagle")
# 
# tab1(VB_1718$"SUSLENTESSONDEVBPAM")
# tab1(clean_18$verbienglasses)
# 
# tab1(VB_1718$"HAUTILIZADOLENTESANTERIORMENT")
# tab1(clean_18$haswornglasses)
# 
# tab1(VB_1718$"USALENTESACTUALMENTE")
# tab1(clean_18$wearsglasses)
# 
# table_1718 <- clean_18 %>%
#   filter(age %in% 5:15) %>% 
#   mutate_at(., c("vadle_withoutrx","vadre_withoutrx"),
#             ~replace(., is.na(.), 0))%>% 
#   group_by(age, gender) %>%
#   summarise(mean_vadre_withoutrx= mean(vadre_withoutrx),
#             mean_vadle_withoutrx = mean(vadle_withoutrx))
#   
# write_xlsx(clean_18, file.path(path, "/Data/Clean data/VB 2017-2018.xlsx"))

# 2016-2017

# can use CICLO 2016 - 2017
VB_1617 <- read_dta(file.path(path, "/Data/Raw data/VB 1617.dta"))

# N = 218,487  
clean_17 <- VB_1617 %>% 
  tibble() %>%
  janitor::clean_names() %>%
  zap_label(.) %>% 
  zap_formats() %>% 
  dplyr::select("clave","turno","nombres","ap_paterno","ap_materno","edad",
                "sexo","grado","grupo","fechadeatencion",
                starts_with(c("av","diagnostico","sus","ha","usa"))) %>%
  setnames(., old = c("clave","turno","grado","grupo","nombres",
                      "ap_paterno","ap_materno",
                      "edad","sexo","fechadeatencion","avsrxod","avsrxoi",
                      "avcrxod","avcrxoi","hacecuantotiempousalentes",
                      "diagnostico_der", "diagnostico_izq"),
           new = c("schoolid","shift","grade","group","givenname","paternallastname",
                   "maternallastname","age","gender","eyeexamdate","vadre_withoutrx",
                   "vadle_withoutrx","vadre_withrx","vadle_withrx",
                   "yearswearingglasses","diagre",
                   "diagle")) %>% 
  mutate(across(c(starts_with("diag"),"gender", ends_with("name")),
                ~trimws(toupper(.x))),
         haswornglasses = ifelse(grepl("NO",hautilizadolentesanteriorment),0,1),
          wearsglasses = ifelse(grepl("NO",usalentesactualmente),0,1),
         verbienglasses = ifelse(grepl("NO",suslentessondevbpam),0,1),
         gender =
           case_when(
             gender == "M" ~ "Male",
             gender == "H" ~ "Female",
             TRUE ~ "Error"),
         across(starts_with(c("vad","years")), ~suppressWarnings(as.numeric(.x))),
         shift = substr(shift,1,10),
         eyeexamdate = as.character(eyeexamdate),
         across(ends_with("name"), ~substr(.x,1,20))) %>%
  dplyr::select("schoolid","shift","grade","group","givenname",
                "paternallastname","maternallastname",
                "age","gender","diagre","diagle","vadre_withoutrx",
                "vadle_withoutrx", "vadre_withrx",
                "vadle_withrx","haswornglasses",
                "wearsglasses","yearswearingglasses",
                "verbienglasses","eyeexamdate") %>%
  mutate(schoolyear = paste(2016,"-",2017))


# Tabulation

# tab1(clean_17$gender)
# tab1(VB_1617$"DiagnosticoDer")
# tab1(VB_1617$"DiagnosticoIzq")
# tab1(clean_17$"diagre")
# tab1(clean_17$"diagle")
# 
# table(VB_1617$"SUSLENTESSONDEVBPAM")
# tab1(clean_17$verbienglasses)
# 
# tab1(VB_1617$"HAUTILIZADOLENTESANTERIORMENT")
# tab1(clean_17$haswornglasses)
# 
# tab1(VB_1617$"USALENTESACTUALMENTE")
# tab1(clean_17$wearsglasses)
# 
# table_1617 <- clean_17 %>%
#   filter(age %in% 5:15) %>% 
#   mutate_at(., c("vadle_withoutrx","vadre_withoutrx"),
#             ~replace(., is.na(.), 0))%>% 
#   group_by(age, gender) %>%
#   summarise(mean_vadre_withoutrx = mean(vadre_withoutrx),
#             mean_vadle_withoutrx = mean(vadle_withoutrx))


# write_xlsx(clean_17, file.path(path, "/Data/Clean data/VB 2016-2017.xlsx"))

#2015 - 2016


VB_1516 <- read.xlsx(file.path(path, "/Data/Raw data/CICLO 2015-2016.xlsx"),
                     detectDates = T)

# n = 152,817 

clean_16 <- VB_1516 %>%
  tibble() %>%
  janitor::clean_names() %>%
  zap_label() %>% 
  zap_formats(.) %>% 
  ungroup() %>% 
  mutate(
    "cat" = case_when(
    (!is.na(ha_utilizado_lentes_anteriormente)
    & !is.na(optometrista)
    & !is.na(fecha_de_atencion)
    & grepl("AÑO", hace_cuanto_tiempo_usa_lentes)
    & !grepl("AÑO", ha_utilizado_lentes_anteriormente)) ~ "0",
    (!is.na(ha_utilizado_lentes_anteriormente))
    & (is.na(optometrista))
    & (is.na(fecha_de_atencion))
    & (grepl("AÑO",hace_cuanto_tiempo_usa_lentes))
    & (!grepl("AÑO", ha_utilizado_lentes_anteriormente)) ~ "1",
    (is.na(ha_utilizado_lentes_anteriormente))
    & (!is.na(hace_cuanto_tiempo_usa_lentes))
    & (!is.na(x34))
    & (is.na(x35))
    & (grepl("AÑO",optometrista))
    & (!grepl("AÑO",ha_utilizado_lentes_anteriormente)) ~ "2",
    (is.na(ha_utilizado_lentes_anteriormente))
    & (is.na(usa_lentes_actualmente))
    & (hace_cuanto_tiempo_usa_lentes == 0)
    & (sus_lentes_son_de_vbpam == 0)
    & (grepl("AÑO", x34)) ~ "3",
    TRUE ~ "5")) %>%
  mutate(cat = ifelse(
    ((ha_utilizado_lentes_anteriormente == "0")
     & (usa_lentes_actualmente == "0")
     & (grepl("AÑO",optometrista))
     & cat == "5"), "4", cat),
    across(c("ha_utilizado_lentes_anteriormente",
                  "hace_cuanto_tiempo_usa_lentes","sexo",
                  "optometrista","usa_lentes_actualmente","sus_lentes_son_de_vbpam"),
                suppressWarnings(toupper)),
         across(c("ha_utilizado_lentes_anteriormente",
                  "hace_cuanto_tiempo_usa_lentes", "sexo",
                  "optometrista","usa_lentes_actualmente","sus_lentes_son_de_vbpam"),
                suppressWarnings(trimws)),
    cat = as.numeric(cat)) %>% 
  mutate(ha_utilizado_lentes_anteriormente
         = ifelse((cat == 2 | cat == 4), hace_cuanto_tiempo_usa_lentes,
                  ifelse(cat == 3, optometrista, ha_utilizado_lentes_anteriormente)),
         usa_lentes_actualmente = ifelse((cat == 2 | cat == 4),sus_lentes_son_de_vbpam,
                                         ifelse(cat == 3,fecha_de_atencion,
                                                usa_lentes_actualmente)),
         hace_cuanto_tiempo_usa_lentes =
           ifelse((cat == 2 | cat == 4), optometrista,
                  ifelse(cat == 3, x34, hace_cuanto_tiempo_usa_lentes)),
         sus_lentes_son_de_vbpam
         = ifelse((cat %in% 3:4), NA,
                  ifelse(cat == 2, fecha_de_atencion,
                         sus_lentes_son_de_vbpam)),
         x34 = ifelse(cat %in% 2:3, NA, x34),
         x35 = ifelse(cat == 3, NA, x35)) %>% 
  mutate(optometrista = ifelse(
           cat == 2, x34, ifelse(
             cat %in% 3:4, NA, optometrista)),
         fecha_de_atencion = ifelse(
           cat == 3, x35, ifelse(
             (cat == 2 | cat == 4), NA, fecha_de_atencion)),
         haswornglasses = ifelse(
           grepl("SI",ha_utilizado_lentes_anteriormente),
                                   1,0),
         wearsglasses = ifelse(
           grepl("SI",usa_lentes_actualmente),1,0),
         verbienglasses = ifelse(
           grepl("SI",sus_lentes_son_de_vbpam),1,0)) %>% 
  mutate(hace_cuanto_tiempo_usa_lentes = str_replace(hace_cuanto_tiempo_usa_lentes,
                                                         "AÑ0S",""),
         hace_cuanto_tiempo_usa_lentes = suppressWarnings(
           substr(hace_cuanto_tiempo_usa_lentes,1,
                         (StrPos(hace_cuanto_tiempo_usa_lentes, "A")-1)))
         ) %>%
  mutate(hace_cuanto_tiempo_usa_lentes = case_when(
             grepl("1/2", hace_cuanto_tiempo_usa_lentes)  ~ 0.5,
             grepl("0", hace_cuanto_tiempo_usa_lentes) ~ 0,
             TRUE ~ 0)) %>% 
  mutate(studentexcluded = ifelse(cat == 5,1,0)) %>% 
  setnames(., old =c("clave","turno","grado", "grupo",
                     "nombre_s", "ap_paterno","ap_materno","edad","sexo", "fecha_de_atencion"),
           new = c("schoolid", "shift", "grade", "group",
                   "givenname","paternallastname","maternallastname","age",
                   "gender","eyeexamdate"))  %>% 

  group_by(schoolid) %>% 
  mutate_at("studentexcluded", sum) %>% 
  filter(studentexcluded == 0) %>% 
  ungroup() %>% 
  dplyr::select("schoolid", "shift", "grade", "group","cat",
                "givenname","paternallastname","maternallastname","age",
                "gender","eyeexamdate", "av_s_rx_od","av_s_rx_oi","av_c_rx_od","av_c_rx_oi",
                "hace_cuanto_tiempo_usa_lentes", starts_with("diag"), 
                ends_with("glasses")) %>%
mutate(across(c("av_s_rx_od","av_s_rx_oi","av_c_rx_od","av_c_rx_oi","age",
                "grade"),
              suppressWarnings(as.numeric)),
       gender =
         case_when(
           gender == "M" ~ "Male",
           gender == "H" ~ "Female",
           TRUE ~ "Error"),
       across(starts_with("av"), ~suppressWarnings(as.numeric(.x)))) %>% 
  mutate_all(na_if,"") %>% 
  setnames(., old = c("av_s_rx_od","av_s_rx_oi","av_c_rx_od","av_c_rx_oi",
                      "hace_cuanto_tiempo_usa_lentes"),
           new = c("vadre_withoutrx",
                   "vadle_withoutrx","vadre_withrx","vadle_withrx",
                   "yearswearingglasses")) %>%
  mutate(
         diagre = trimws(diagnostico_der),
         diagle = trimws(diagnostico_izq),
         shift = substr(shift,1,10),
         schoolid = substr(schoolid,1,10),
         givenname = substr(givenname,1,20),
         paternallastname = substr(paternallastname,1,20),
         maternallastname = substr(maternallastname,1,20),
         across(ends_with("name"),~suppressWarnings(trimws(toupper(.x))))) %>% 
  dplyr::select("schoolid","shift","grade","group","givenname",
                   "paternallastname","maternallastname",
                   "age","gender","diagre","diagle","vadre_withoutrx",
                   "vadle_withoutrx", "vadre_withrx",
                   "vadle_withrx","haswornglasses",
                   "wearsglasses","yearswearingglasses",
                   "verbienglasses","eyeexamdate") %>% 
  mutate(schoolyear = paste(2015,"-",2016)) 


# write_xlsx(clean_16, file.path(path, "/Data/Clean data/VB 2015-2016.xlsx"))

#Tabulations

# tab1(clean_16$gender)
# tab1(VB_1516$"Diagnostico.Der")
# tab1(VB_1516$"Diagnostico.Izq")
# tab1(clean_16$"diagre")
# tab1(clean_16$"diagle")
# 
# tab1(VB_1516$"SUS.LENTES.SON.DE.VBPAM")
# tab1(clean_16$verbienglasses)
# 
# tab1(VB_1516$"HA.UTILIZADO.LENTES.ANTERIORMENT")
# tab1(clean_16$haswornglasses)
# 
# tab1(VB_1516$"USA.LENTES.ACTUALMENTE")
# tab1(clean_16$wearsglasses)
# 
# table_1516 <- clean_16 %>%
#   filter(age %in% 5:15) %>% 
#   mutate_at(., c("vadle_withoutrx","vadre_withoutrx"),
#             ~replace(., is.na(.), 0))%>% 
#   group_by(age, gender) %>%
#   summarise(mean_vadre_withoutrx= mean(vadre_withoutrx),
#             mean_vadle_withoutrx = mean(vadle_withoutrx))


# Appending data across years

# N = 1,022,710

# before filter; N = 1,048,003

appended <- rbind(clean_20, clean_19, clean_18, clean_17, clean_16) %>% 
  mutate(schoolid =
              ifelse(schoolid == "2ETV0634S", "02ETV0634S", schoolid),
         shift = ifelse(
           ((grepl("NOCTUNO", shift))
            |(grepl("EXTENDIDO", shift))
              |(grepl("COMPLETO", shift))), "CONTINUO",
           shift),
         schoolid = trimws(schoolid)) %>% 
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
    !(substr(schoolid,4,5) == "JN")) %>%  # Kindergarten
  mutate_at(c(3,8,12:19),
            ~suppressWarnings(as.numeric(.x)))


# Labels
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
#                prescription - right eye",
#                vadle_withoutrx = "Visual acuity deficit
#                without prescription - left eye",
#                vadre_withrx = "Visual acuity deficit with prescription - 
#                right eye",
#                vadle_withrx = "Visual acuity deficit with prescription - left eye",
#                haswornglasses = "Student has worn prescription glasses before",
#                wearsglasses = "Student currently wears prescription glasses",
#                yearswearingglasses = "Years wearing glasses",
#                verbienglasses = "Student's current glasses are from Ver Bien",
#                eyeexamdate = "Date of eye exam") %>%


# Tabulations
# tab1(appended$diagle)
# tab1(appended$diagre)
# tab1(appended$gender)
# tab1(appended$"schoolyear")





# Audit
# No differences.


# write_xlsx(appended, file.path(path, "/Data/Clean data/VB 2015-2020.xlsx"))




