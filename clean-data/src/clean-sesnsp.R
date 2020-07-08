##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-incidencia0620covid/clean-data/src/clean-sesnsp.R

pacman::p_load(tidyverse, janitor, here, stringi)

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

#----- 1: Incidencia Estatal ------- #

# 1.1 : Procesamos datos de incidencia estatal

ide <-  filter(df_ide, str_detect(subtipo_de_delito, 
                          "Homicidio doloso|Feminicidio|Violación|Acoso|Abuso sexual|Hostigamiento|Secuestro|Robo"),
               str_detect(subtipo_de_delito, "maquinaria|bancaria|ganado") == F) %>% 
  mutate(total_anual = rowSums(select(., enero:diciembre), na.rm = T),
         total_anual = ifelse(ano == 2020, total_anual * (12/5), total_anual),
         total_enero_mayo =  rowSums(select(., enero:mayo), na.rm = T),
         subtipo_de_delito = case_when(
           str_detect(subtipo_de_delito, "Violación") ~ "Violación",
           str_detect(subtipo_de_delito, "Acoso|Hostigamiento") ~ 
             "Hostigamiento o acoso sexual",
           T ~ subtipo_de_delito)) %>% 
  group_by(ano, clave_ent, entidad, subtipo_de_delito) %>% 
  summarise(across(c(total_anual, total_enero_mayo, enero:mayo), 
                   sum, na.rm = T)) %>% 
  ungroup() %>% 
  rename(year = ano, cve_ent = clave_ent, nom_ent = entidad,
         tipo_delito = subtipo_de_delito) %>% 
  rename_at(vars(enero:mayo), function(x) paste0("total_", x)) %>% 
  select(year:tipo_delito, total_enero:total_mayo, 
         total_enero_mayo, total_anual)

# 1.2: Reshapeamos para sumar homicidios con feminicidios

ide <- pivot_longer(ide, total_enero:total_anual, names_to = "tipo_total",
                     values_to = "valor") %>% 
  pivot_wider(names_from = tipo_delito, values_from = valor) %>% 
  mutate(`Homicidio doloso` = `Homicidio doloso` + Feminicidio) %>% 
  pivot_longer(-c(year:tipo_total), names_to = "tipo_delito", values_to = "valor") %>% 
  pivot_wider(names_from = tipo_total, values_from = valor)
  
# 1.3: Procesamos poblaciones estatales

pob_est <- filter(df_pobest, ano >= 2015 & ano <= 2020, cve_geo != "00") %>% 
  group_by(ano, cve_geo, sexo) %>%  
  summarise(pob_tot = sum(poblacion)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sexo, values_from = pob_tot) %>% 
  clean_names() %>% 
  mutate(pob_tot = hombres + mujeres) %>% 
  rename(pob_hom = hombres, pob_muj = mujeres) %>% 
  group_by(ano) %>% 
  mutate(pob_nacional_hom = sum(pob_hom),
            pob_nacional_muj = sum(pob_muj),
            pob_nacional = sum(pob_tot)) %>% 
  ungroup()

# 1.4: Cruzamos

ide <- left_join(ide, pob_est, by = c("year" = "ano", "cve_ent" = "cve_geo")) %>% 
  mutate(nom_ent = case_when(
                     str_detect(nom_ent, "Coahuila") == T ~ "Coahuila",
                     str_detect(nom_ent, "Ciudad de México") == T ~ "CDMX",
                     str_detect(nom_ent, "Michoacán") == T ~ "Michoacán",
                     str_detect(nom_ent, "Veracruz") == T ~ "Veracruz", 
                     T ~ nom_ent))

#----- 2: Incidencia Municipal ------- #

# 2.1: Procesamos incidencia municipal

idm <-  filter(df_idm, str_detect(subtipo_de_delito, 
                                  "Homicidio doloso|Feminicidio|Violación|Acoso|Abuso sexual|Hostigamiento|Secuestro|Robo"),
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
  summarise(across(c(enero:mayo, total_enero_mayo, total_anual), 
                   sum, na.rm = T)) %>% 
  ungroup() %>% 
  rename(year = ano, nom_ent = entidad, cve_ent = clave_ent,
         tipo_delito = subtipo_de_delito, cve_mun = cve_municipio,
         nom_mun = municipio)  %>% 
  rename_at(vars(enero:mayo), function(x) paste0("total_", x))

# 2.2: Reshapeamos para sumar homicidios con feminicidios

idm <- pivot_longer(idm, total_enero:total_anual, names_to = "tipo_total",
                    values_to = "valor") %>% 
  pivot_wider(names_from = tipo_delito, values_from = valor) %>% 
  mutate(`Homicidio doloso` = `Homicidio doloso` + Feminicidio) %>% 
  pivot_longer(-c(year:tipo_total), names_to = "tipo_delito", values_to = "valor") %>% 
  pivot_wider(names_from = tipo_total, values_from = valor)
  

# 2.3: Reshapeamos para que cada municipio aparezca en todos los años

idm <-  pivot_longer(idm, c(total_enero:total_anual), names_to = "tipo_total",
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

# 2.4: Procesamos poblaciones municipales
  
pob_mun <- filter(df_pobmun, ano >= 2015 & ano <= 2020) %>% 
  group_by(ano, clave_ent, clave, sexo) %>% 
  summarise(pob_tot = sum(pob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sexo, values_from = pob_tot) %>% 
  clean_names() %>% 
  mutate(pob_tot = hombres + mujeres) %>% 
  rename(year = ano, cve_ent = clave_ent, cve_mun = clave,
         pob_hom = hombres, pob_muj = mujeres) 

# 2.5: Cruzamos

idm <- left_join(idm, pob_mun, by = c("year", "cve_ent", "cve_mun")) 

# Guardamos ambas bases

saveRDS(ide, files$output_ide)
saveRDS(idm, files$output_idm)
