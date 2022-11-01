setwd("~/Desktop/DIL/Ver Bien/Data/SEP")

#=======================================================
# This script fetches geocoordinates from Google Maps for the groups of three
#or more schools that share the exact same coordinates according to the official 
# data. The command "googleplaces" must be installed, and an API key must be used.
# It is a paid service. 
# APIkey is: AIzaSyCMXHzMMuCiUG7eEsPiwPNbobCgWsy48_c
#=======================================================

SEP_CE_raw <- read_dta("concentrado_escuelas (Primarias).dta") %>% 
  bind_rows(read_dta("concentrado_escuelas (Secundarias).dta")) 

api_key <- "AIzaSyCMXHzMMuCiUG7eEsPiwPNbobCgWsy48_c"

SEP_CE <- SEP_CE_raw %>% 
  mutate(schools = 1) %>% 
  group_by(Nivel_educativo,
           Ubicación_de_la_escuela_locali1,
           Ubicación_de_la_escuela_locali2) %>% 
  mutate(schools = sum(schools)) %>% 
  filter(!(schools <= 2)) %>% 
  ungroup() %>% 
  setnames(old = c("Clave_del_centro_de_trabajo","Nivel_educativo",
                      "Nombre_del_centro_de_trabajo", "Nombre_de_localidad",
                      "Nombre_del_municipio_o_delegaci","Nombre_de_la_entidad"),
           new = c("cct","level","name","city","municipality","state")) %>% 
  dplyr::select(c("cct","level","name","city","municipality","state")) %>% 
  mutate(cct = substr(cct, 1, 10),
         across(2:6, trimws)) %>% 
  mutate_at(c("name","level","city","municipality","state"),
            list(~ substr(., 1,20))) %>% 
  distinct() %>% 
  drop_na(name) %>% 
  mutate(name = paste("ESCUELA"," " ,level," ",name)) %>% 
  mutate_at(2:6,
         list( ~ mgsub::mgsub(., c("\\Á","\\É","\\Í","\\Ó","\\Ú","\\Ñ","\\Ü",
                                   "\\´","\\'","\\-","\\N°","\\#","\\¥",
                                   "\\,","\\/","\\¿","\\?","\\+Æ","\\+","\\:",
                                   "\\;","\\_","\\(","\\)","\\`"),
         c("A","E","I","O","U","N","U","","",""," ","NUM","NUM","N"," ",
           " ", " ","N"," "," "," "," ","","","")))) %>%
  mutate(search_name = paste(name, city, municipality,
                              state, cct, sep= " ")) 
  # mutate(x := (1:n()) %/% 100L) 

list_1 = list()
name <- as.list(SEP_CE$search_name)
# Given funds for Googleplaces, exclude line below and run a full search. 
name <- name[300:350]

for (i in name){
  list_1 <- append(list_1, google_places(search_string = i,
                                         key = "AIzaSyCMXHzMMuCiUG7eEsPiwPNbobCgWsy48_c"))
}

GP_results <- list_1[grep("results", names(list_1))]

for(i in 1:length(GP_results)){
  GP_results[[i]]$geometry$location$searchname <- unlist(name[i])
}

for(i in 1:length(GP_results)){
  if (length(unlist(GP_results[i])) == 1){
    GP_results <- GP_results[- i]
  }
}

GP_clean <- data.frame()

for (i in 1:length(GP_results)){
  GP_clean <- bind_rows(GP_clean,
                        GP_results[[i]][["geometry"]][["location"]])
}

# select the first coordinate given for each searched school
GP_crd <- GP_clean %>% 
  distinct(searchname, .keep_all = T) %>%
  mutate(searchname = str_sub(searchname, -10,-1)) %>% 
  setnames(., old = c("lat","lng","searchname"),
           new = c("g_lat","g_lng","schoolid")) %>% 
  drop_na(g_lng)

# write_xlsx(GP_crd,"Coordinates from Google.xlsx")

# Me - 81 lines; Pablo - 319 lines

# Stata/Pablo version below:

# write_xlsx(SEP_CE,"Schools with messy coordinates.xlxs")

# MAX <- max(SEP_CE$x)

# for any unique combination of these variables count how many schools are that 
# combinations
# Make a variable that is 1 and then sum by different observations.
# counts the number of observations  - correct

# Try a free apikey to see if the code clocks
# Inconsistent stopping

# L75 - seq() and block(100) combined - I made a new variable that goes from 0 to 
# 140 and each number has 100 entries  - should be correct
# 
# L76 - is this max of the sum of x? or in my case, is it 140?
# 144 blocks
# L 121 - what is g_status[_n-1]? 
# The status in the entry above? It's checking which entry is clogging the 
# googleplaces command

# Below we fetch the coordinates by blocks of 100 schools
# Notice that the command "googleplaces" requires an API key
# 
# print(paste("Fetching ", MAX, "blocks of 100 schools each..."))
# print(paste("Starting time: ", Sys.time()))
# 
# list = list()
# 
# quiet(
# for (i in 0:MAX){
#   paste("Block_", i) <- google_places(
#     search_string = SEP_CE$name,
#     key = "AIzaSyCMXHzMMuCiUG7eEsPiwPNbobCgWsy48_c")
#   append(list, paste("Block_", i))
#   print(paste("Block ", i, ": ", Sys.time()))
# })
# 

# GP_results[[i]]$geometry$location
# GP_results <- GP_results[["results"]][["geometry"]][["location"]]
# 
# 
# # Sometimes some variables appear as strings (i.e. alphabetic). We must turn 
# # them into numerical variables.
# 
# 
# for (i in 0:MAX){
#   paste("Block_", i) <- paste("Block_", i) %>% 
#     mutate(across(c("g_queryID","g_lat","g_lng"),
#                   as.numeric))
# }
# 
# # We append all blocks. Some stopped due to problems with one search. 
# 
# GP_it_1 <- bind_rows(list)
# 
# ggplot(GP_it_1 , aes(x = g_lng,
#                      y = g_lat)) + geom_point()
# 
# 
# # SECOND ITERATION: Once we remove the schools that stop the searches, we 
# # continue with a 2nd iteration.
# 
# GP_it_2  <- GP_it_1 %>% 
#   mutate(flag = ifelse(is.na(g_status) & is.na(lag(g_status)),
#                        1, 0)) %>% 
#   filter(is.na(g_status) & is.na(g_lat),
#          flag != 1) %>% 
#   mutate(x := (1:n()) %/% 100L) %>% 
#   select(-c("flag","originalid", starts_with("g_")))
# 
# MAX <- max(GP_it_2$x)
# 
# print(paste("Fetching ", MAX, "blocks of 100 schools each..."))
# print(paste("Starting time: ", Sys.time()))
# 
# list = list()
# 
# quiet(
#   for (i in 0:MAX){
#     paste("Block_", i) <- google_places(
#       search_string = paste(GP_it_2$name, GP_it_2$city,
#                             GP_it_2$municipality, GP_it_2$state,
#                             GP_it_2$cct, collapse = " "),
#       key = api_key)
#     append(list, paste("Block_", i))
#     print(paste("Block ", i, ": ", Sys.time()))
#   })
# 
# 
# for (i in 0:MAX){
#   paste("Block_", i) <- paste("Block_", i) %>% 
#     mutate(across(c("g_queryID","g_lat","g_lng"),
#                   as.numeric))
# }
# 
# GP_it_2 <- bind_rows(list)
# 
# ggplot(GP_it_2 , aes(x = g_lng,
#                      y = g_lat)) + geom_point()
# 
# 
# # THIRD ITERATION: Once we remove the schools that stop the searches, 
# # we continue with a 3rd iteration.
# 
# GP_it_3  <- GP_it_2 %>% 
#   mutate(flag = ifelse(is.na(g_status) & is.na(lag(g_status)),
#                        1, 0)) %>% 
#   filter(is.na(g_status) & is.na(g_lat),
#          flag != 1) %>% 
#   mutate(x := (1:n()) %/% 100L) %>% 
#   select(-c("flag","originalid", starts_with("g_")))
# 
# MAX <- max(GP_it_3$x)
# 
# 
# print(paste("Fetching ", MAX, "blocks of 100 schools each..."))
# print(paste("Starting time: ", Sys.time()))
# 
# list = list()
# 
# quiet(
#   for (i in 0:MAX){
#     paste("Block_", i) <- google_places(
#       search_string = paste(GP_it_3$name, GP_it_3$city,
#                             GP_it_3$municipality, GP_it_3$state,
#                             GP_it_3$cct, collapse = " "),
#       key = api_key)
#     append(list, paste("Block_", i))
#     print(paste("Block ", i, ": ", Sys.time()))
#   })
# 
# 
# for (i in 0:MAX){
#   paste("Block_", i) <- paste("Block_", i) %>% 
#     mutate(across(c("g_queryID","g_lat","g_lng"),
#                   as.numeric))
# }
# 
# GP_it_3 <- bind_rows(list)
# 
# ggplot(GP_it_3 , aes(x = g_lng,
#                      y = g_lat)) + geom_point()
# 
# 
# # FOURTH ITERATION: Once we remove the schools that stop the searches, 
# # we continue with a 4th iteration.
# 
# GP_it_4  <- GP_it_3 %>% 
#   mutate(flag = ifelse(is.na(g_status) & is.na(lag(g_status)),
#                        1, 0)) %>% 
#   filter(is.na(g_status) & is.na(g_lat),
#          flag != 1) %>% 
#   mutate(x := (1:n()) %/% 100L) %>% 
#   select(-c("flag","originalid", starts_with("g_")))
# 
# MAX <- max(GP_it_4$x)
# 
# print(paste("Fetching ", MAX, "blocks of 100 schools each..."))
# print(paste("Starting time: ", Sys.time()))
# 
# list = list()
# 
# quiet(
#   for (i in 0:MAX){
#     paste("Block_", i) <- google_places(
#       search_string = paste(GP_it_4$name, GP_it_4$city,
#                             GP_it_4$municipality, GP_it_4$state,
#                             GP_it_4$cct, collapse = " "),
#       key = api_key)
#     append(list, paste("Block_", i))
#     print(paste("Block ", i, ": ", Sys.time()))
#   })
# 
# 
# for (i in 0:MAX){
#   paste("Block_", i) <- paste("Block_", i) %>% 
#     mutate(across(c("g_queryID","g_lat","g_lng"),
#                   as.numeric))
# }
# 
# GP_it_4 <- bind_rows(list)
# 
# ggplot(GP_it_4 , aes(x = g_lng,
#                      y = g_lat)) + geom_point()
# 
# # FIFTH ITERATION: Once we remove the schools that stop the searches, 
# # we continue with a 5th iteration.
# 
# GP_it_5  <- GP_it_4 %>% 
#   mutate(flag = ifelse(is.na(g_status) & is.na(lag(g_status)),
#                        1, 0)) %>% 
#   filter(is.na(g_status) & is.na(g_lat),
#          flag != 1) %>% 
#   mutate(x := (1:n()) %/% 100L) %>%  # Our last "blocks" have one school each
#   select(-c("flag","originalid", starts_with("g_")))
# 
# MAX <- max(GP_it_5$x)
# 
# print(paste("Fetching ", MAX, "blocks of 100 schools each..."))
# print(paste("Starting time: ", Sys.time()))
# 
# list = list()
# 
# quiet(
#   for (i in 0:MAX){
#     paste("Block_", i) <- google_places(
#       search_string = paste(GP_it_5$name, GP_it_5$city,
#                             GP_it_5$municipality, GP_it_5$state,
#                             GP_it_5$cct, collapse = " "),
#       key = api_key)
#     append(list, paste("Block_", i))
#     print(paste("Block ", i, ": ", Sys.time()))
#   })
# 
# 
# for (i in 0:MAX){
#   paste("Block_", i) <- paste("Block_", i) %>% 
#     mutate(across(c("g_queryID","g_lat","g_lng"),
#                   as.numeric))
# }
# 
# GP_it_5 <- bind_rows(list)
# 
# 
# 
# # We bring together the results of the five iterations. There are 14,395 CCTs.
# # We succesfully fecthed coordinates for 11,553 (80.3%) of them. For the rest
# # we didn't find coordinates. The results are stores to be combined with the
# # original data from SEP.
# 
# GP_coord <- bind_rows(GP_it_1, GP_it_2,
#                       GP_it_3,GP_it_4, GP_it_5) %>% 
#   filter(!is.na(g_lat)) %>% 
#   dplyr::select(c("cct","g_lat","g_lng")) %>% 
#   mutate("schoolid" = substr(cct,1,10)) %>% 
#   dplyr::select(-cct) 
#   
# last one has 19 observations. 
# write_xlsx(GP_coord,"Coordinates from Google.xlsx")

# 20 hours a week for a week 
# Business 


