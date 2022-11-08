# Change path to reflect location of the Ver Bien folder in your computer
path <- "~/Desktop/DIL/Ver Bien"
setwd(path)

#Loads necessary packages

pacman::p_load(stataXml,lubridate, shape,openxlsx,tidyverse,imputeTS,dplyr,
               epiDisplay,stringi,stringr,stats,expss,writexl,haven,DescTools, 
               questionr, panelr,data.table, remotes, janitor, 
               readxl, PropCIs, binom, mgsub,googleway,ddpcr, purrr, tidyr,
               geosphere, swfscMisc, statar, cutr, Hmisc, lfe, broom, stargazer,
               kableExtra, gtsummary, survival, plyr, nlme)

# To see log of tidyverse functions, also load tidylog