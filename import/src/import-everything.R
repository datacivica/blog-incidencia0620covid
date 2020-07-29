##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-incidencia0620covid/import/src/import-everything.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here, data.table, readxl)

# Archivos

files <- list(input_idenm = here("import/input/IDEFC_NM_jun2020.csv"),
              input_pob_est = here("import/input/pob_mit_proyecciones.csv.gz"),
              output_idenm = here("import/output/idenm.rds"),
              output_pob_est = here("import/output/pob_estatal.rds"))

# Abrimos bases

print("Importando SESNSP")
df_idenm  <- read.csv(files$input_idenm, fileEncoding = "ISO-8859-1", 
                      stringsAsFactors = F) %>% 
  clean_names() %>% 
  mutate(across(c(enero:diciembre), str_replace, ",", ""),
         across(c(enero:diciembre), as.numeric),
         clave_ent = str_pad(clave_ent, width = 2, side = "left", pad = "0"))

print("Importando población")
df_pobest <- fread(files$input_pob_est, stringsAsFactors = F) %>% 
  mutate(cve_geo = str_pad(cve_geo, width = 2, side = "left", pad = "0"))


# Guardamos
print("Guardando bases")
saveRDS(df_idenm, files$output_idenm)
saveRDS(df_pobest, files$output_pob_est)
print("Bases importadas y guardadas")

