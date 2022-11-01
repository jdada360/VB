setwd("~/Desktop/DIL/Ver Bien/Data")
slist <- c("Primarias","Secundarias")

crd_ggl <- read_dta("~/Desktop/DIL/Ver Bien/Data/SEP/Coordinates from Google.dta")
crd_ggl_raw <- crd_ggl 

p_s_list <- list()
merge_list <- list()
cross_list <- list()
# dist_list_1 <- list()
# dist_list_2 <- list()

for (i in slist){
  p_s_list[[i]] <- read_dta(
    paste0("SEP/concentrado_escuelas ","(",i,")",".dta"))
  p_s_list[[i]] <- p_s_list[[i]] %>% 
    rename("lng"= "Ubicación_de_la_escuela_locali1",
           "lat" = "Ubicación_de_la_escuela_locali2") %>% 
    filter(between(lng, -120, -85),                                                 
           between(lat, 13,33)) %>%
    mutate(Clave_del_centro_de_trabajo = trimws(Clave_del_centro_de_trabajo)) %>% 
    mutate(schoolid = substr(Clave_del_centro_de_trabajo,
                             1,10)) %>% 
    dplyr::select(c("schoolid","lat","lng")) %>% 
    distinct() %>%  
    left_join(., crd_ggl, by = "schoolid") %>%                                      # We bring the coordinates fetched from Google Maps
    mutate(x = ifelse(!is.na(g_lng), g_lng, lng),
           y = ifelse(!is.na(g_lat), g_lat, lat)) %>% 
    dplyr::select(c("schoolid","x","y")) %>% 
    mutate(sx = round(x, 1), sy = round(y, 1))
  # The scatter plot shows a map of Mexico 
  # print(ggplot(p_s_list[[i]]
  #        , aes(x = x,
  #                      y = y)) + geom_point(size = 0.5))
  # To avoid calculating the distance for a large number of school pairs, 
  # we "join" them by approximate coordinates, rounding them to one tenth 
  # of latitude and longitude units, and creating "tables" with them.
}

for (i in slist){
  # setNames(paste0("_", names(.))) %>%
  # write_xlsx(p_s_list[[i]],"Coordinates from Google.xlsx")
  # We create "tables" of nine cells to make an efficient cartesian product of
  # schools to calculate distances. Each "table" is 0.3 degrees wide and 0.3
  # degrees high (0.3 degrees = 33 km). Each school is at the center cell of 
  # each table build around it. We call that cell 5. We start by creating that
  # center cell rounding up coordinates to the first decimal. The other eight 
  # cells are created by adding or subtracting a decimal to the latitude or
  # longitude. The minimum "radius" in the table is 0.1 degrees (11 km).
  # 
  #   *-----+-----+-----+
  #   *  1  |  2  |  3  |
  #   *-----+-----+-----+
  #   *  4  |  5  |  6  |
  #   *-----+-----+-----+
  #   *  7  |  8  |  9  |
  #   *-----+-----+-----+
  
  merge_list[[i]] <- p_s_list[[i]] %>% 
    rename("ry" = sy,
           "rx" = sx) %>% 
    arrange(schoolid) %>% 
    slice(rep(1:n(), each = 9)) %>% 
    mutate(square = rep(1:9, times = nrow(.)/9)) %>% 
    mutate(sx = case_when(
      square %in% c(1,4,7) ~ (rx - 0.1),
      square %in% c(2,5,8) ~ rx,
      square %in% c(3,6,9) ~ (rx + 0.1),
      TRUE ~ 0),
      sy = case_when(
      square %in% 1:3 ~ (ry + 0.1),
      square %in% 4:6 ~ ry,
      square %in% 7:9 ~ (ry - 0.1),
      TRUE ~ 0))}

for (i in slist){
merge_list[[i]] <- left_join(merge_list[[i]], p_s_list[[i]], 
                             by = c("schoolid"), all.x = T) %>% 
  mutate("d" = distm(c(as.list(x.x), as.list(y.x)), 
                       c(as.list(x.y),as.list(y.y)), fun = distVincentySphere
                     )) %>% 
    mutate(d = d/1000) %>% 
    mutate("u0" = ifelse(d==0, 1,0))}
  
  for (j in 1:10){
    p_s_list[[i]][[paste0("u", j)]] = ifelse(p_s_list[[i]][["d"]]< j, 
                                             1, 0)
    }
  
  p_s_list[[i]] <- p_s_list[[i]] %>% 
    replace(is.na(.), 0) %>% 
    mutate_at(13:23,
              as.numeric) %>% 
    group_by(schoolid.x) %>% 
    summarise_at(c("u0","u1","u2","u3","u4","u5",
                   "u6","u7","u8","u9","u10"),
                 sum, na.rm = T)
  


p_s_list <- bind_rows(p_s_list) %>% 
  distinct() %>% 
  rename("schoolid" = "schoolid.x")


# write_xlsx(p_s_list,"Coordinates from Google.xlsx")

# Audit

# L44 - renaming all variables to start with _
# L74 - undoes this renaming? So unsure why it is done in the first place?

  

    

