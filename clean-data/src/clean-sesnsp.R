##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-incidencia0620covid/clean-data/src/clean-sesnsp.R

require(pacman)
p_load(tidyverse, janitor, here, stringi)

# Archivos

files <- list(input_idenm = here("import/output/idenm.rds"),
              input_idmnm = here("import/output/idmnm.rds"),
              input_pob_est = here("import/output/pob_estatal.rds"),
              input_pob_mun = here("import/output/pob_municipal.rds"),
              output_ide = here("clean-data/output/clean-ide.rds"),
              output_idm = here("clean-data/output/clean-idm.rds"))

# Abrimos archivos

df_ide <- readRDS(files$input_idenm)
df_idm <- readRDS(files$input_idmnm)
df_pobest <- readRDS(files$input_pob_est)
df_pobmun <- readRDS(files$input_pob_mun)

# 1.- Incidencia Estatal

# 1.1 : Procesamos datos de incidencia estatal

ide <-  filter(df_ide, str_detect(subtipo_de_delito, 
                          "Homicidio doloso|Violación|Acoso|Abuso sexual|Hostigamiento|Secuestro|Robo"),
               str_detect(subtipo_de_delito, "maquinaria|bancaria|ganado") == F) %>% 
  mutate(total_anual = rowSums(select(., enero:diciembre), na.rm = T),
         total_anual = ifelse(ano == 2020, total_anual * (12/5), total_anual),
         total_enero_mayo =  rowSums(select(., enero:mayo), na.rm = T),
         subtipo_de_delito = case_when(
           str_detect(subtipo_de_delito, "Violación") ~ "Violación",
           str_detect(subtipo_de_delito, "Acoso|Hostigamiento") ~ 
             "Hostigamiento o acoso sexual",
           T ~ subtipo_de_delito)) %>% 
  group_by(ano, clave_ent, subtipo_de_delito) %>% 
  summarise(across(c(total_anual, total_enero_mayo), 
                   sum, na.rm = T)) %>% 
  ungroup() %>% 
  rename(year = ano, cve_ent = clave_ent, tipo_delito = subtipo_de_delito) 

# 1.2: Procesamos poblaciones estatales

pob_est <- filter(df_pobest, ano >= 2015 & ano <= 2020, cve_geo != "00") %>% 
  group_by(ano, entidad, cve_geo) %>%  
  summarise(pob_tot = sum(poblacion)) %>% 
  ungroup()

# 1.3: Cruzamos

ide <- left_join(ide, pob_est, by = c("year" = "ano", "cve_ent" = "cve_geo")) %>% 
  relocate(entidad, .after = year)

# 2.- Incidencia municipal

# 2.1: Procesamos incidencia municipal

idm <-  filter(df_idm, str_detect(subtipo_de_delito, 
                                  "Homicidio doloso|Violación|Acoso|Abuso sexual|Hostigamiento|Secuestro|Robo"),
               str_detect(subtipo_de_delito, "maquinaria|bancaria|ganado") == F) %>% 
  mutate(total_anual = rowSums(select(., enero:diciembre), na.rm = T),
         total_anual = ifelse(ano == 2020, total_anual * (12/5), total_anual),
         total_enero_mayo =  rowSums(select(., enero:mayo), na.rm = T),
         subtipo_de_delito = case_when(
           str_detect(subtipo_de_delito, "Violación") ~ "Violación",
           str_detect(subtipo_de_delito, "Acoso|Hostigamiento") ~ 
             "Hostigamiento o acoso sexual",
           T ~ subtipo_de_delito)) %>% 
  group_by(ano, entidad, clave_ent, municipio, cve_municipio, subtipo_de_delito) %>% 
  summarise(across(c(total_anual, total_enero_mayo), 
                   sum, na.rm = T)) %>% 
  ungroup() %>% 
  rename(year = ano, nom_ent = entidad, cve_ent = clave_ent,
         tipo_delito = subtipo_de_delito, cve_mun = cve_municipio,
         nom_mun = municipio) 
  

# 2.2: Reshapeamos para que cada municipio aparezca en todos los años

idm <-  pivot_longer(idm, c(total_anual, total_enero_mayo), names_to = "tipo_total",
               values_to = "valor") %>% 
  pivot_wider(names_from = c(tipo_delito, tipo_total, year), values_from = valor,
              names_sep = "-") %>% 
  pivot_longer(-c(cve_ent, nom_ent, cve_mun, nom_mun), 
               names_to = c("tipo_delito", "tipo_total", "year"),
               names_sep = "-", values_to = "valor") %>% 
  mutate(valor = replace_na(valor, 0),
         year = as.numeric(year)) %>% 
  pivot_wider(names_from = tipo_total, values_from = valor) %>% 
  select(year, everything())

# 2.3: Procesamos poblaciones municipales
  
pob_mun <- filter(df_pobmun, ano >= 2015 & ano <= 2020) %>% 
  group_by(ano, clave_ent, clave) %>% 
  summarise(pob_tot = sum(pob)) %>% 
  ungroup() %>% 
  rename(year = ano, cve_ent = clave_ent, cve_mun = clave )

# 2.4: Cruzamos

idm <- left_join(idm, pob_mun, by = c("year", "cve_ent", "cve_mun")) 

# Guardamos ambas bases

saveRDS(ide, files$output_ide)
saveRDS(idm, files$output_idm)
