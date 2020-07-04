##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-incidencia0620covid/import/src/import-everything.R

require(pacman)
p_load(tidyverse, janitor, here, data.table, readxl)

# Archivos

files <- list(input_idenm = here("import/input/IDEFC_NM_may2020.csv"),
              input_idmnm = here("import/input/IDM_NM_may2020.csv"),
              input_nombres = here("import/input/nombres_municipios.xlsx.gz"),
              input_pob_est = here("import/input/pob_mit_proyecciones.csv.gz"),
              input_pob_mun = here("import/input/municipios_sexo_15-30.csv.gz"),
              output_idenm = here("import/output/idenm.rds"),
              output_idmnm = here("import/output/idmnm.rds"),
              output_nombres1 = here("import/output/nombres_estados.rds"),
              output_nombres2 = here("import/output/nombres_municipios.rds"),
              output_pob_est = here("import/output/pob_estatal.rds"),
              output_pob_mun = here("import/output/pob_municipal.rds"))

# Abrimos bases

df_idenm  <- read.csv(files$input_idenm, fileEncoding = "ISO-8859-1", 
                      stringsAsFactors = F) %>% 
  clean_names() %>% 
  mutate(across(c(enero:diciembre), str_replace, ",", "")) %>% 
  mutate(across(c(enero:diciembre), as.numeric)) %>% 
  mutate(clave_ent = formatC(clave_ent, width = 2, flag = "0", format = "d"))
  
df_idmnm  <- read.csv(files$input_idmnm, fileEncoding = "ISO-8859-1",
                      stringsAsFactors = F) %>% 
  clean_names() %>% 
  mutate(across(c(enero:diciembre), str_replace, ",", "")) %>% 
  mutate(across(c(enero:diciembre), as.numeric)) %>% 
  mutate(clave_ent = formatC(clave_ent, width = 2, flag = "0", format = "d"),
         cve_municipio = formatC(cve_municipio, width = 5, flag = "0", format = "d"))

df_nombres1 <- read_excel(gunzip(files$input_nombres, overwrite=T, remove=F)) %>% 
  clean_names() %>% 
  distinct(cve_ent, .keep_all = T) %>% 
  select(cve_ent, nom_ent) %>% 
  mutate(nom_ent = case_when(
           str_detect(nom_ent, "Coahuila") == T ~ "Coahuila",
           str_detect(nom_ent, "Distrito Federal") == T ~ "Ciudad de México",
           str_detect(nom_ent, "Michoacán") == T ~ "Michoacán",
           str_detect(nom_ent, "Veracruz") == T ~ "Veracruz",
           T ~ nom_ent)) 

df_nombres2 <- read_excel(gunzip(files$input_nombres, overwrite=T, remove=F)) %>% 
  clean_names() %>% 
  mutate(inegi = paste0(cve_ent, cve_mun)) %>% 
  mutate(nom_ent = case_when(
    str_detect(nom_ent, "Coahuila") == T ~ "Coahuila",
    str_detect(nom_ent, "Distrito Federal") == T ~ "Ciudad de México",
    str_detect(nom_ent, "Michoacán") == T ~ "Michoacán",
    str_detect(nom_ent, "Veracruz") == T ~ "Veracruz",
    T ~ nom_ent)) 


df_pobest <- fread(files$input_pob_est, stringsAsFactors = F) %>% 
  mutate(cve_geo = formatC(cve_geo, width = 2, flag = "0", format = "d"))


saveRDS(df_cenapi, files$output_cenapi)
saveRDS(df_idenm, files$output_idenm)
saveRDS(df_idmnm, files$output_idmnm)
saveRDS(df_nombres1, files$output_nombres1)
saveRDS(df_nombres2, files$output_nombres2)
saveRDS(df_pobest, files$output_pob_est)

