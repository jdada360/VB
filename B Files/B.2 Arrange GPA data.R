# This script prepares the GPA data. We start by merging by 
# school IDs with the Ver Bien data. That way we avoid cleaning
# data we will not use.

# Aguascalientes (AS)

setwd(file.path(path, "Data/Raw data"))

vb_schools = read_dta("Schools visited by Ver Bien.dta")

vb_schools = zap_label(vb_schools)


years = 2015:2019
ag_raw = lapply(years, function(y){
  read_dta(paste0("BD_AGUASCALIENTES/VBPAM ",y, "-", y+1, ".dta"))
})

ag_df = bind_rows(raw_list) %>% 
  tibble() %>%
  janitor::clean_names() %>%
  filter(entidad != "") %>% 
  mutate(schoolid = trimws(toupper(cct)),
         genero_alumno =  trimws(toupper(genero_alumno)))  %>% 
  mutate(gender = case_when(
    grepl("H", genero_alumno) ~ "Male",
    grepl("M", genero_alumno) ~ "Female",
    TRUE ~ "Error")) %>% 
  mutate_at(c("edad_alumno",
              "grupo",
              "turno"),
            trimws) %>% 
  mutate(shift = toupper(turno),
         age = as.numeric(edad_alumno),
         grade = as.numeric(grado),
         gpa = as.numeric(promedio_grado))
  
# Aguascalientes data don't seperate given and family names. 
# We have to approximate this counting words "backwards." 
# Before that we must deal with "composed" last names
# (the ones including "de" etc.)

ag_named <- ag_df %>% 
  mutate(n = toupper(trimws(nombre_alumno))) %>% 
  mutate(n = str_replace_all(n," ","_"))

words = data.frame(str_split_fixed(string = ag_named$n, pattern = '_', n = 6))

# I am unsure how pablo is determining what name is which

ag_named <- ag_named %>% 
  bind_cols(words)

#  Plots grade distribution of GPAs
# 
# ggplot(data = ag_named,
#        aes(x = gpa)) +
#   geom_histogram(bins = 100) + facet_wrap(~grade)


# Durango

# 2015-2016, 2016-2017, 2017-2018, 2018-2019

years = c(1516, 1617, 1718, 1819)

schoollevel = c("prim","sec")

du_raw <- lapply(schoollevel,
                 function(level){
                   lapply(years, function(y){
                     read_dta(paste0("BD_DURANGO/",
                                     level, y, ".dta"))  })})



du_clean <- bind_rows(du_raw) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  mutate(grade = as.numeric(substr(grupo, 1, 1)),
         group = substr(grupo, 2, 3),
         gender = case_when(
           grepl("H", toupper(sexo)) ~ "Male",
           grepl("M", toupper(sexo)) ~ "Female",
           TRUE ~ "Error")) %>% 
  dplyr::select(c("ciclo","cct","cve_turno",
                  "grade","group","nombre",
                  "gender","edad","curp","promedio_g")) %>% 
  setnames(., old = c(1:3,6:7, 9),
           new = c("schoolyear","schoolid",
                   "shift","name","age","gpa"))

# fix names

# tabulations
# table(du_clean$schoolyear,du_clean$level)

# 2018-2019 and 2019-2020

DG_1820_raw = lapply(schoollevel,
                 function(level){
                   read_dta(paste0("BD_DURANGO/",
                                   level, 1920,".dta")) %>% 
                     tibble() %>% 
                     janitor::clean_names() 
                 })

DG_1820 = bind_rows(DG_1820_raw) %>% 
  mutate(ciclo_esco = "2019-2020",
         grado = as.numeric(grado),
         gender = case_when(
           grepl("H", toupper(sexo)) ~ "Male",
           grepl("M", toupper(sexo)) ~ "Female",
           TRUE ~ "Error")) %>% 
  dplyr::select(c("ciclo_esco", "cct", "cve_turno", "grado",
                 "grupo", "nombre", "primero",
                 "segundo", "gender", "edad","curp", "promedio")) %>% 
  setnames(., old = 1:12,
           new = c("schoolyear",
           "schoolid",
           "shift", "grade","group",
           "givenname","paternallastname",
           "maternallastname","gender","age","curp","gpa")) %>% 
  mutate_at(6:8,
           ~substr(trimws(.x), 1 ,10)) %>% 
  inner_join(., vb_schools,
            by = 'schoolid')


# We get rid of irrelevant schools (not visited by Ver Bien)


# Guanajuato (GT)

years = 2014:2018

GT_raw = lapply(years, function(y){
  read.table(paste0("BG_GUANAJUANTO/alumnos ", y,".txt"),
             sep = "|", fill = TRUE, header = T)
})

GT_df <- bind_rows(GT_raw)  %>% 
  mutate(ciclo = paste0(suppressWarnings(as.numeric(ciclo))
                        ,"-", suppressWarnings(as.numeric(ciclo)+1))) %>% 
  dplyr::select(c("ciclo",
                  "ct",
                  "turno","grado",
                  "grupo",
                  "nombre",
                  "apellidopaterno",
                  "apellidomaterno",
                  "curp","edad",
                  "genero","algprome")) %>% 
  setnames(., old = 1:12,
           new = c("schoolyear",
                   "schoolid","shift",
                   "grade","group",
                   "givenname","paternallastname",
                   "maternallastname","curp","age",
                   "gender","gpa")) %>% 
  mutate(gender = case_when(
    grepl("H", toupper(gender)) ~ "Male",
    grepl("M", toupper(gender)) ~ "Female",
    TRUE ~ "Error"
  )) %>% 
  inner_join(., vb_schools, 
             by = 'schoolid')


# Oaxaca (OA)

# Grades 4, 5 and 6

years = 12:18

OA_raw = lapply(years, function(i){
                read_xlsx(paste0("BD_OAXACA/",
                   i,"-",i+1, " 4to 5to 6to.xlsx"),
                   sheet ="Hoja1",
                   col_names = TRUE)})


OA_df <- bind_rows(OA_raw) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  dplyr::select(c("ciclo",
                  "clave",
                  "turno","nombre",
                  "apepat","apemat","edad",
                  "genero","grado","grupo",
                  "promedio","curp")) %>% 
  setnames(., old = 1:12,
           new = c("schoolyear",
                   "schoolid","shift",
                   "givenname","paternallastname",
                   "maternallastname",
                   "age","gender","grade","group",
                   "gpa","curp")) %>% 
  mutate(gender = case_when(
    grepl("H", toupper(gender)) ~ "Male",
    grepl("M", toupper(gender)) ~ "Female",
    TRUE ~ "Error"),
    shift = case_when(
      grepl("CONTINUO|MATUTINO", toupper(shift)) ~ "M",
      grepl("VESPERTINO", toupper(shift)) ~ "V",
      grepl("NOCTURNO", toupper(shift)) ~ "N",
      TRUE ~ "")) %>% 
  mutate_at(c("group","givenname",
              "paternallastname",
              "maternallastname"),
            trimws) %>% 
  mutate_at(c("givenname",
              "paternallastname",
              "maternallastname"),
            ~substr(.x, 1, 15)) %>% 
  inner_join(., vb_schools,
             by = 'schoolid')
  

#  Sonora (SO)

years = 2014:2018

schoollevel = c("(Secundaria)",
                "(Primaria)")

S0_raw = list()
 for(level in schoollevel){
                 output = lapply(years, function(y){
                   x <- read_dta(paste0("BD_SONORA/Base de Datos Salud Escolar ", 
                                        level," ",y, "-", y+1,".dta")) 
                   x <- x %>% 
                     mutate(schoolyear = paste0( y, "-", y+1))
                   S0_raw[[paste0(y)]] = x})
                 S0_raw = append(S0_raw, output)}
  


S0_clean = list()

S0_clean[6:8] <- lapply(S0_raw[6:8], function(df){
    df %>% 
    tibble() %>% 
    janitor::clean_names() %>% 
    mutate(schoolid = trimws(esc_cve),
            shift = trimws(algcvetu),
            givenname = substr(trimws(toupper(alunombre)),
                               1, 50),
            paternallastname = substr(trimws(toupper(aluapepat)),
                                      1, 50),
            maternallastname = substr(trimws(toupper(aluapemat)),
                                      1, 50),
            gender = case_when(
              grepl("H", toupper(alu_sexo)) ~ "Male",
              grepl("M", toupper(alu_sexo)) ~ "Female",
              TRUE ~ "Error"),
            gpa = suppressWarnings(as.numeric(al_g_prome))) %>% 
    setnames(., old = c("grado","grupo"),
             new = c("grade","group")) %>%
    dplyr::select(c("schoolyear", "schoolid", "shift","grade",
                    "group","givenname","paternallastname", "maternallastname",
                    "curp","gender","gpa"))
})
                 
S0_clean[[9]] <- S0_raw[[9]] %>% 
    tibble() %>% 
    janitor::clean_names() %>% 
    mutate(schoolid = trimws(esc_cve),
           shift = trimws(turno),
           givenname = substr(trimws(toupper(alunombre)),
                              1, 50),
           paternallastname = substr(trimws(toupper(aluapepat)),
                                     1, 50),
           maternallastname = substr(trimws(toupper(aluapemat)),
                                     1, 50),
           gender = case_when(
             grepl("H", toupper(alu_sexo)) ~ "Male",
             grepl("M", toupper(alu_sexo)) ~ "Female",
             TRUE ~ "Error"),
           gpa = suppressWarnings(as.numeric(al_g_prome))) %>% 
    setnames(., old = c("grado","grupo"),
             new = c("grade","group")) %>%
    dplyr::select(c("schoolyear", "schoolid", "shift","grade",
                    "group","givenname","paternallastname", "maternallastname",
                    "curp","gender","gpa"))

S0_clean[[10]] <- S0_raw[[10]] %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  mutate(schoolid = trimws(cct),
         shift = trimws(turno),
         givenname = substr(trimws(toupper(p)),
                            1, 50),
         paternallastname = substr(trimws(toupper(apell1)),
                                   1, 50),
         maternallastname = substr(trimws(toupper(apell2)),
                                   1, 50),
         gender = case_when(
           grepl("H", toupper(genero)) ~ "Male",
           grepl("M", toupper(genero)) ~ "Female",
           TRUE ~ "Error"),
         gpa = suppressWarnings(as.numeric(promgrado))) %>% 
  setnames(., old = c("grado","grupo"),
           new = c("grade","group")) %>%
  dplyr::select(c("schoolyear", "schoolid", "shift","grade",
                  "group","givenname","paternallastname", "maternallastname",
                  "curp","gender","gpa"))


S0_clean[1:4] <- lapply(S0_raw[1:4],
                        function(df){
                           df %>% 
                            tibble() %>%
                            janitor::clean_names() %>% 
                            mutate(schoolid = trimws(esc_cve),
                                   shift = trimws(alg_cve_tu),
                                   givenname = substr(trimws(toupper(alu_nombre)),
                                                      1, 50),
                                   paternallastname = substr(trimws(toupper(alu_ape_pat)),
                                                             1, 50),
                                   maternallastname = substr(trimws(toupper(alu_ape_mat)),
                                                             1, 50),
                                   gender = case_when(
                                     substr(toupper(curp),11,11)  ==  "H" ~ "Male",
                                     substr(toupper(curp),11,11)  ==  "M" ~ "Female",
                                     TRUE ~ "Error"),
                                   gpa = suppressWarnings(as.numeric(al_g_prome))) %>% 
                            setnames(., old = c("grado","grupo"),
                                     new = c("grade","group")) %>%
                            dplyr::select(c("schoolyear", "schoolid", "shift","grade",
                                            "group","givenname","paternallastname", "maternallastname",
                                            "curp","gender","gpa")) })


S0_clean[[5]] <- S0_raw[[5]] %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  mutate(schoolid = trimws(cct),
         shift = trimws(turno),
         givenname = substr(trimws(toupper(o)),
                            1, 50),
         paternallastname = substr(trimws(toupper(apell1)),
                                   1, 50),
         maternallastname = substr(trimws(toupper(apell2)),
                                   1, 50),
         gender = case_when(
           grepl("H", toupper(genero)) ~ "Male",
           grepl("M", toupper(genero)) ~ "Female",
           TRUE ~ "Error"),
         gpa = suppressWarnings(as.numeric(promgrado))) %>% 
  setnames(., old = c("grado","grupo"),
           new = c("grade","group")) %>%
  dplyr::select(c("schoolyear", "schoolid", "shift","grade",
                  "group","givenname","paternallastname", "maternallastname",
                  "curp","gender","gpa"))
  
S0_df<- bind_rows(S0_clean) %>% 
  mutate(yob = case_when(
    suppressWarnings(as.numeric(substr(trimws(curp), 5,6))) < 20 ~ 2000 + 
      suppressWarnings(as.numeric(substr(trimws(curp), 5,6))),
    suppressWarnings(as.numeric(substr(trimws(curp), 5,6))) >= 20 ~ 1900 + 
      suppressWarnings(as.numeric(substr(trimws(curp), 5,6))),
    TRUE ~ NA_real_),
    age = suppressWarnings(as.numeric(substr(schoolyear,1,4))) - yob) %>% 
  inner_join(., vb_schools,
             by = 'schoolid')


# Tamaulipas (TM)

years = c(1415, 1516, 1617, 1718, 1819)

TM_raw = list()

for (y in years){
  for (i in 1:6){
    TM_raw[[i]] = read_dta(paste0("BD_TAMAULIPAS/CIC-", y, "/PRI", i,".dta")) %>% 
      mutate(schoolyear = y) %>% 
      tibble() %>% 
      janitor::clean_names() %>% 
      dplyr::select(c("schoolyear","cct",
                      "turno","nombre","primerapellido","segundoapellido",
                      "curp","edad","genero","grado",
                      "grupo","promedio"))}
  for (i in 1:3){
    TM_raw[[i + 6]] = read_dta(paste0("BD_TAMAULIPAS/CIC-", y, "/SEC", i,"_.dta")) %>% 
      mutate(schoolyear = y) %>% 
      tibble() %>% 
      janitor::clean_names() %>% 
      dplyr::select(c("schoolyear","cct",
                      "turno","nombre","primerapellido","segundoapellido",
                      "curp","edad","genero","grado",
                      "grupo","promedio"))}
}


TM_df <- bind_rows(TM_raw) %>% 
  setnames(., old = 2:12,
           new = c("schoolid","shift","givenname",
                   "paternallastname","maternallastname",
                   "curp","age","gender",
                   "grade","group","gpa")) %>% 
  filter(!is.na(schoolid),
         schoolid != "") %>% 
  mutate(schoolyear = case_when(
    schoolyear == 1415 ~ "2014-2015",
    schoolyear == 1516 ~ "2015-2016",
    schoolyear == 1617 ~ "2016-2017",
    schoolyear == 1718 ~ "2017-2018",
    schoolyear == 1819 ~ "2018-2019",
    TRUE ~ "Error"),
    gender = case_when(
      grepl("H", trimws(toupper(gender))) ~ "Male",
      grepl("M", trimws(toupper(gender))) ~ "Female",
      TRUE ~ "Error"),
    schoolid = trimws(schoolid),
    shift = case_when(
      grepl("CONTINUO", toupper(shift)) |  
        grepl("MATUTINO", toupper(shift)) ~ "M",
      grepl("VESPERTINO", toupper(shift)) ~ "V",
      grepl("NOCTURNO", toupper(shift)) ~ "N"),
    group = trimws(group),
    givenname = substr(trimws(toupper(givenname)), 1,
                       15),
    paternallastname = substr(trimws(paternallastname),1,15),
    maternallastname = substr(trimws(maternallastname),1,15)) %>% 
  inner_join(., vb_schools, by = 'schoolid')

# Tabulations

# table(TM_df$schoolyear)

# Yucatan (YU)

years = c("2014-2015","2016-2017","2017-2018","2018-2019")

YU_raw = lapply(years, function(y){
  read_csv(paste0("BD_YUCATAN/ciclo",y,".csv"),
           show_col_types = F) %>% 
    mutate(schoolyear = y)
})

YU_1516 <- read_xlsx("BD_YUCATAN/prim_2015_2016.xlsx",
                     sheet = "Hoja 1 - prim_2015_2016",
                     col_names = TRUE) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  setnames(., old = c("yucatan",
  "cct", "apell1", "apell2", "genero"),
  new = c("estado","clavecct","ap_paterno","ap_materno",
          "sexo")) %>% 
  mutate(turno = case_when(
    grepl("M", toupper(turno)) ~ "MATUTINO",
    grepl("V", toupper(turno)) ~ "VESPERTINO",
    grepl("D", toupper(turno)) ~ "DISCONTINUO",
    TRUE ~ "Error"))

YU_df = bind_rows(YU_raw, YU_1516) %>% 
  dplyr::select(4, 6:16) %>% 
  setnames(., old = 1:11,
           new = c("schoolid","shift","givenname",
                   "paternallastname",
                   "maternallastname",
                   "age","gender","curp",
                   "grade","group","gpa")) %>% 
  mutate(gender = case_when(
    grepl("H", trimws(toupper(gender))) ~ "Male",
    grepl("M", trimws(toupper(gender))) ~ "Female",
    TRUE ~ "Error"),
    schoolid = trimws(schoolid),
    shift = case_when(
      grepl("CONTINUO", toupper(shift)) |  
        grepl("MATUTINO", toupper(shift)) ~ "M",
      grepl("VESPERTINO", toupper(shift)) ~ "V",
      grepl("NOCTURNO", toupper(shift)) ~ "N"),
    group = trimws(group),
    givenname = substr(trimws(toupper(givenname)), 1,
                       15),
    paternallastname = substr(trimws(paternallastname),1,15),
    maternallastname = substr(trimws(maternallastname),1,15)) %>% 
  inner_join(., vb_schools, by = 'schoolid')





