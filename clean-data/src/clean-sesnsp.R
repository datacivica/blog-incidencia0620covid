##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-incidencia0620covid/clean-data/src/clean-sesnsp.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here, stringi)

# Archivos

files <- list(input_idenm = here("import/output/idenm.rds"),
              input_pob_est = here("import/output/pob_estatal.rds"),
              output_ide = here("clean-data/output/clean-ide.rds"))

# Abrimos archivos

df_ide <- readRDS(files$input_idenm)
df_pobest <- readRDS(files$input_pob_est)

bases <- list(df_ide = df_ide, df_pobest = df_pobest)
rm(df_ide, df_pobest)

#----- 1: Incidencia Estatal ------- #

# 1.1 : Procesamos datos de incidencia estatal

print("Procesando incidencia estatal")
ide <-  filter(bases$df_ide, str_detect(subtipo_de_delito, 
                          "Homicidio doloso|Feminicidio|Violación|Acoso|Abuso sexual|Hostigamiento|Secuestro|Robo"),
               str_detect(subtipo_de_delito, "maquinaria|bancaria|ganado") == F) %>% 
  mutate(total_anual = rowSums(select(., enero:diciembre), na.rm = T),
         total_anual = ifelse(ano == 2020, total_anual * (12/6), total_anual),
         total_enero_junio =  rowSums(select(., enero:junio), na.rm = T),
         subtipo_de_delito = case_when(
           str_detect(subtipo_de_delito, "Violación") ~ "Violación",
           str_detect(subtipo_de_delito, "Acoso|Hostigamiento") ~ 
             "Hostigamiento o acoso sexual",
           T ~ subtipo_de_delito)) %>% 
  group_by(ano, clave_ent, entidad, subtipo_de_delito) %>% 
  summarise(across(c(total_anual, total_enero_junio, enero:junio), 
                   sum, na.rm = T)) %>% 
  ungroup() %>% 
  rename(year = ano, cve_ent = clave_ent, nom_ent = entidad,
         tipo_delito = subtipo_de_delito) %>% 
  rename_at(vars(enero:junio), function(x) paste0("total_", x)) %>% 
  select(year:tipo_delito, total_enero:total_junio, 
         total_enero_junio, total_anual)

# 1.2: Reshapeamos para sumar homicidios con feminicidios
ide <- pivot_longer(ide, total_enero:total_anual, names_to = "tipo_total",
                     values_to = "valor") %>% 
  pivot_wider(names_from = tipo_delito, values_from = valor) %>% 
  mutate(`Homicidio doloso` = `Homicidio doloso` + Feminicidio) %>% 
  pivot_longer(-c(year:tipo_total), names_to = "tipo_delito", values_to = "valor") %>% 
  pivot_wider(names_from = tipo_total, values_from = valor)
  
# 1.3: Procesamos poblaciones estatales
print("Procesando población estatal")
pob_est <- filter(bases$df_pobest, ano >= 2015 & ano <= 2020, cve_geo != "00") %>% 
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
print("Cruzando bases")
ide <- left_join(ide, pob_est, by = c("year" = "ano", "cve_ent" = "cve_geo")) %>% 
  mutate(nom_ent = case_when(
                     str_detect(nom_ent, "Coahuila") == T ~ "Coahuila",
                     str_detect(nom_ent, "Ciudad de México") == T ~ "CDMX",
                     str_detect(nom_ent, "Michoacán") == T ~ "Michoacán",
                     str_detect(nom_ent, "Veracruz") == T ~ "Veracruz", 
                     T ~ nom_ent))

# Guardamos base
print("Guardando base final")
saveRDS(ide, files$output_ide)
