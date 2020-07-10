# Author: AL
# Maintainer(s): AL
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# =====================================================
# blog-incidencia0620covid/grafs/src/grafs-sesnsp.R
#

pacman::p_load(tidyverse, here, extrafont, scales,
               ggpubr, ggrepel, lemon, viridis, janitor)

loadfonts(quiet = T)

files <- list(ide = here("clean-data/output/clean-ide.rds"),
              idm = here("clean-data/output/clean-idm.rds"),
              graf1 = here("grafs/output/1_fiebres-robos.svg"),
              graf2 = here("grafs/output/2_fiebres-alto.svg"),
              graf3 = here("grafs/output/3_fiebres-acum-robos.svg"),
              graf4 = here("grafs/output/4_fiebres-acum-alto.svg"),
              graf5 = here("grafs/output/5_scatter-variaciones.svg"),
              graf6 = here("grafs/output/6_scatter-variaciones.svg"),
              graf7 = here("grafs/output/7_dumbell-altoimpacto.svg"),
              graf8 = here("grafs/output/8_dumbell-robos.svg"),
              graf_dist1 = here("grafs/output/bars_dist1.svg"),
              graf_dist2 = here("grafs/output/bars_dist2.svg"))

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))


# Abrimos bases

ide <- readRDS(files$ide) %>% 
  mutate(periodo = case_when(
    year >= 2015 & year <= 2019 ~ "2015-2019",
    year == 2020 ~ "2020"),
    tipo_delito = case_when(
      str_detect(tipo_delito, "Robo a transeúnte en espacio") ~
        "Robo a transeúnte en espacio abierto", T ~ tipo_delito))

idm <- readRDS(files$idm) %>% 
  mutate(periodo = ifelse(year >= 2015 & year <= 2019, "2015-2019", "2020"),
         tipo_delito = case_when(
           str_detect(tipo_delito, "Robo a transeúnte en espacio") ~
             "Robo a transeúnte en espacio abierto",
           T ~ tipo_delito))

# Función para cambiar nombres

abreviar <- function(x) { 
  x <- mutate(x, abrev = case_when(
    cve_ent == "01" ~ "AGS",
    cve_ent == "02" ~ "BCN",
    cve_ent == "03" ~ "BCS",
    cve_ent == "04" ~ "CAM",
    cve_ent == "05" ~ "COA",
    cve_ent == "06" ~ "COL",
    cve_ent == "07" ~ "CHAP",
    cve_ent == "08" ~ "CHIH",
    cve_ent == "09" ~ "CDMX",
    cve_ent == "10" ~ "DUR",
    cve_ent == "11" ~ "GUA",
    cve_ent == "12" ~ "GRO",
    cve_ent == "13" ~ "HGO",
    cve_ent == "14" ~ "JAL",
    cve_ent == "15" ~ "MEX",
    cve_ent == "16" ~ "MIC",
    cve_ent == "17" ~ "MOR",
    cve_ent == "18" ~ "NAY",
    cve_ent == "19" ~ "NLE",
    cve_ent == "20" ~ "OAX",
    cve_ent == "21" ~ "PUE",
    cve_ent == "22" ~ "QRO",
    cve_ent == "23" ~ "ROO",
    cve_ent == "24" ~ "SLP",
    cve_ent == "25" ~ "SIN",
    cve_ent == "26" ~ "SON",
    cve_ent == "27" ~ "TAB",
    cve_ent == "28" ~ "TAM",
    cve_ent == "29" ~ "TLA",
    cve_ent == "30" ~ "VER",
    cve_ent == "31" ~ "YUC",
    cve_ent == "32" ~ "ZAC"))
}

# Graficamos

# Fiebres nacionales - Robo - 2015-2019 vs 2020

tempo <- filter(ide, year == 2019) %>% 
  mutate(periodo = "2019") %>% 
  bind_rows(ide, .) %>% 
  group_by(year, periodo, tipo_delito, pob_nacional) %>% 
  summarise(across(starts_with("total"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  relocate(pob_nacional, .after = last_col()) %>% 
  mutate(across(starts_with("total"),
                ~ round(.x / pob_nacional * 100000, digits = 2))) %>% 
  rename_at(vars(total_enero:total_anual), 
            function(x) str_replace(x, "total", "tasa")) %>% 
  filter(str_detect(tipo_delito, "Robo")) %>% 
  pivot_longer(tasa_enero:tasa_mayo, names_to = "mes", values_to = "tasa") %>% 
  group_by(periodo, tipo_delito, mes) %>% 
  summarise(mean_tasa = mean(tasa, na.rm = T),
            sd_tasa = round(sd(tasa, na.rm = T), digits = 2),
         min_tasa = min(tasa, na.rm = T), max_tasa = max(tasa, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mes = str_remove(mes, "tasa_"), mes = str_to_title(mes),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo")),
         tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2019",
                                              "2020")))

ggplot(tempo, aes(x = mes, y = mean_tasa, color = periodo, group = periodo)) +
  geom_ribbon(data = tempo %>% filter(periodo == "Promedio\n2015-2019"),
              aes(ymin = mean_tasa - sd_tasa, 
                  ymax = mean_tasa + sd_tasa, x = mes), fill = "#9a9aff",
              color = "#9a9aff",
              alpha = 0.4, size = 0.5, show.legend = F) +
  geom_line(alpha = 0.8, size = 1) +
  geom_point(size = 1.25, alpha = 0.6) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 3) +
  labs(title = "Tasa nacional de carpetas de investigación por robo",
       subtitle = "Tasas mensuales por tipo de robo",
       x = "", y = "Tasa por cada 100 mil habitantes", color = "", 
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        legend.position = "top")

ggsave(files$graf1, width = 14, height = 8)

# Fiebres nacionales - Alto Impacto - 2015-2019 vs 2020

tempo <-   filter(ide, year == 2019) %>% 
  mutate(periodo = "2019") %>% 
  bind_rows(ide, .) %>% 
  group_by(year, periodo, tipo_delito, pob_nacional) %>% 
  summarise(across(starts_with("total"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  relocate(pob_nacional, .after = last_col()) %>% 
  mutate(across(starts_with("total"),
                ~ round(.x / pob_nacional * 100000, digits = 2))) %>% 
  rename_at(vars(total_enero:total_anual), 
            function(x) str_replace(x, "total", "tasa")) %>% 
  filter(str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro")) %>% 
  pivot_longer(tasa_enero:tasa_mayo, names_to = "mes", values_to = "tasa") %>% 
  group_by(periodo, tipo_delito, mes) %>% 
  summarise(mean_tasa = mean(tasa, na.rm = T), sd_tasa = sd(tasa, na.rm = T),
            min_tasa = min(tasa, na.rm = T), max_tasa = max(tasa, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mes = str_remove(mes, "tasa_"), 
         mes = str_to_title(mes),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo")),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2019",
                                              "2020")))


ggplot(tempo, aes(x = mes, y = mean_tasa, color = periodo, group = periodo)) +
  geom_ribbon(data = tempo %>% filter(periodo == "Promedio\n2015-2019"),
              aes(ymin = mean_tasa - sd_tasa, 
                  ymax = mean_tasa + sd_tasa, x = mes), fill = "#9a9aff",
              color = "#9a9aff",
              alpha = 0.4, size = 0.5, show.legend = F) +
  geom_line(alpha = 0.8, size = 1) +
  geom_point(size = 1.25, alpha = 0.6) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 1) +
  labs(title = "Tasa nacional de carpetas de investigación por delitos de alto impacto",
       subtitle = "Tasas mensuales por tipo de delito",
       x = "", y = "Tasa por cada 100 mil habitantes", color = "", 
       caption = "Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)") +
  tema +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        legend.position = "top")

ggsave(files$graf2, width = 14, height = 8)

# Acumulados - robos

tempo <- filter(ide, year == 2019) %>% 
  mutate(periodo = "2019") %>% 
  bind_rows(ide, .) %>% 
  group_by(periodo, tipo_delito, pob_nacional) %>% 
  summarise(across(starts_with("total"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  relocate(pob_nacional, .after = last_col()) %>% 
  mutate(total_febrero = total_febrero + total_enero,
         total_marzo = total_marzo + total_febrero,
         total_abril = total_abril + total_marzo,
         total_mayo = total_mayo + total_abril,
         across(starts_with("total"),
                ~ round(.x / pob_nacional * 100000, digits = 2))) %>% 
  rename_at(vars(total_enero:total_anual), 
            function(x) str_replace(x, "total", "tasa")) %>% 
  filter(str_detect(tipo_delito, "Robo")) %>% 
  group_by(periodo, tipo_delito) %>% 
  summarise(across(c(tasa_enero:tasa_mayo), ~ round(mean(.x), digits = 2))) %>% 
  ungroup() %>% 
  pivot_longer(tasa_enero:tasa_mayo, names_to = "mes", values_to = "tasa") %>% 
  mutate(mes = str_remove(mes, "tasa_"), 
         mes = str_to_title(mes),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo")),
         tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2019",
                                              "2020")))

ggplot(tempo, aes(x = mes, y = tasa, color = periodo, group = periodo)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1, alpha = 0.6) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 3) +
  labs(title = "Tasa acumulada nacional de carpetas de investigación por robo",
       subtitle = "Tasas con totales acumulados por mes y tipo de robo",
       x = "", y = "Tasa por cada 100 mil habitantes", color = "", 
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        legend.position = "bottom")

ggsave(files$graf3, width = 14, height = 8)

# Acumulados - alto impacto

tempo <-  filter(ide, year == 2019) %>% 
  mutate(periodo = "2019") %>% 
  bind_rows(ide, .) %>% 
  group_by(year, periodo, tipo_delito, pob_nacional,
                  pob_nacional_muj) %>% 
  summarise(across(starts_with("total"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  relocate(pob_nacional, .after = last_col()) %>% 
  mutate(total_febrero = total_febrero + total_enero,
         total_marzo = total_marzo + total_febrero,
         total_abril = total_abril + total_marzo,
         total_mayo = total_mayo + total_abril) %>% 
  mutate_at(vars(starts_with("total")), list(~case_when(
    tipo_delito == "Feminicidio" ~ round(./ pob_nacional_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ round(./ pob_nacional * 100000, digits = 2)))) %>% 
  rename_at(vars(total_enero:total_anual), 
            function(x) str_replace(x, "total", "tasa")) %>% 
  filter(str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro")) %>% 
  group_by(periodo, tipo_delito) %>% 
  summarise(across(c(tasa_enero:tasa_mayo), ~ round(mean(.x), digits = 2))) %>% 
  ungroup() %>% 
  pivot_longer(tasa_enero:tasa_mayo, names_to = "mes", values_to = "tasa") %>% 
  mutate(mes = str_remove(mes, "tasa_"), 
         mes = str_to_title(mes),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo")),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2019",
                                              "2020")))


ggplot(tempo, aes(x = mes, y = tasa, color = periodo, group = periodo)) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 1) +
  labs(title = "Tasa nacional acumulada de carpetas de investigación por delitos de alto impacto",
       subtitle = "Tasas con totales acumulados por mes y tipo de delito",
       x = "", y = "Tasa por cada 100 mil habitantes", color = "", 
       caption = "Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)") +
  tema +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        legend.position = "bottom")

ggsave(files$graf4, width = 14, height = 8)

# Histogramas

tempo <- filter(ide, year == 2019) %>% 
  mutate(periodo = "2019") %>% 
  bind_rows(ide, .) %>%
  select(periodo, year:total_enero_mayo) %>% 
  group_by(periodo, tipo_delito) %>% 
  summarise(across(starts_with("total_"), sum)) %>% 
  pivot_longer(total_enero:total_mayo, names_to = "mes", values_to = "total") %>% 
  mutate(mes = str_remove(mes, "total_"),
         mes = str_to_title(mes),
         porcentaje = round(total/total_enero_mayo, digits = 2),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo"))) %>% 
  relocate(total_enero_mayo, .before = porcentaje) %>% 
  arrange(periodo)


plot_df <-   filter(tempo, str_detect(tipo_delito, "Robo"),
                    str_detect(tipo_delito, "casa|transeúnte|transporte público")) %>% 
  mutate(tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito)) 

ggplot(plot_df, aes(x = mes , y = porcentaje, fill = periodo)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge(width = 0.75), width = 1) +
  geom_text(aes(label = paste0(porcentaje * 100, "%")), 
            position = position_dodge(width = 1), 
            vjust = -0.5, hjust = 0.2,
            size = 3,
            family = "Barlow Condensed") +
  scale_fill_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_rep_wrap(~tipo_delito, repeat.tick.labels = "bottom") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribución mensual del total de carpetas de investigación",
       subtitle = "Porcentajes mensuales sobre el total de carpetas abiertas hasta mayo",
       x = "", y = "Porcentaje", 
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave(files$graf_dist1, width = 14, height = 8)


plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro"))

ggplot(plot_df, aes(x = mes , y = porcentaje, fill = periodo)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge(width = 0.75), width = 1) +
  geom_text(aes(label = paste0(porcentaje * 100, "%")), 
            position = position_dodge(width = 1), 
            vjust = -0.5, hjust = 0.2,
            size = 3,
            family = "Barlow Condensed") +
  scale_fill_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  facet_rep_wrap(~tipo_delito, repeat.tick.labels = "bottom") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribución mensual del total de carpetas de investigación",
       subtitle = "Porcentajes mensuales sobre el total de carpetas abiertas hasta mayo",
       x = "", y = "Porcentaje", 
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave(files$graf_dist2, width = 14, height = 8)

# Scatters estatales

tempo <- select(ide, year, cve_ent, nom_ent, tipo_delito, 
         pob_tot, pob_muj, total_enero_mayo) %>%
  mutate(tasa_enero_mayo = case_when(
    tipo_delito == "Feminicidio" ~ 
      round(total_enero_mayo/ pob_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ 
      round(total_enero_mayo/ pob_tot * 100000, digits = 2)),
    periodo = case_when(year == 2020 ~ "2020", T ~ "2015-2019")) %>% 
  group_by(periodo, cve_ent, nom_ent, tipo_delito) %>% 
  summarise(tasa_enero_mayo = round(mean(tasa_enero_mayo), digits = 2)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = periodo, values_from = tasa_enero_mayo,
              names_prefix = "tasa_") %>% 
  clean_names() %>% 
  mutate(diferencia = 
           round((tasa_2020 - tasa_2015_2019) / tasa_2015_2019, digits = 2),
         diferencia_grupo = case_when(
           diferencia > 0 ~ "Aumento",
           diferencia == 0 ~ "Sin variación",
           T ~ "Disminución"),
         diferencia_grupo = factor(diferencia_grupo, 
                                   levels = c("Aumento", "Sin variación",
                                              "Disminución"))) %>% 
  abreviar() %>% 
  arrange(tipo_delito, -diferencia) %>% 
  filter(!is.na(diferencia), !is.infinite(diferencia)) %>% 
  group_by(tipo_delito) %>% 
  mutate(rank = rank(-diferencia)) %>% 
  ungroup()
  

plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro"))

ggplot(plot_df, aes(x = tipo_delito, y = diferencia, color = diferencia_grupo)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), size = 1.2) +
  geom_text_repel(data = plot_df %>% filter(rank <= 5 | rank >= 28), 
                  aes(x = tipo_delito, y = diferencia, 
                                      color = diferencia_grupo,
                label = abrev), show.legend = F, vjust = 0.5,
            position = position_jitter(seed = 1, width = 0.15), 
            family = "Barlow Condensed",  size = 3) +
  scale_color_manual(values = c("#E3072A", "#E49C23", "#4E6ED2")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "¿Qué entidades han tenido variaciones más grandes en tasas de incidencia delictiva?",
       subtitle = "Tasa promedio 2015-2019 vs Tasa 2020\n(Con totales acumulados al mes de mayo)",
       y = "Variación porcentual",
       caption = "Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)") +
tema + theme(axis.title.x = element_blank(), legend.title =  element_blank(),
             plot.caption = element_text(margin = margin(20, 0, 0, 0)),
             legend.position = "top")

ggsave(files$graf5, width = 14, height = 8)

plot_df <-   filter(tempo, str_detect(tipo_delito, "Robo"),
                    str_detect(tipo_delito, "casa|transeúnte|transporte público")) %>% 
  mutate(tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito))

ggplot(plot_df, aes(x = tipo_delito, y = diferencia, color = diferencia_grupo)) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), size = 1.2) +
  geom_text_repel(data = plot_df %>% filter(rank <= 5 | rank >= 22), 
                  aes(x = tipo_delito, y = diferencia, 
                                            color = diferencia_grupo,
                                            label = abrev), show.legend = F, vjust = 0.5,
                  position = position_jitter(seed = 1, width = 0.25), 
                  family = "Barlow Condensed",  size = 3) +
  scale_color_manual(values = c("#E3072A", "#E49C23", "#4E6ED2")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "¿Qué entidades han tenido variaciones más grandes en tasas de incidencia delictiva?",
       subtitle = "Tasa promedio 2015-2019 vs Tasa 2020\n(Con totales acumulados al mes de mayo)",
       y = "Variación porcentual",
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema + theme(axis.title.x = element_blank(), legend.title =  element_blank(),
               plot.caption = element_text(margin = margin(20, 0, 0, 0)),
               legend.position = "top")

ggsave(files$graf6, width = 14, height = 8)

# Dumbells 

tempo <- select(ide, year, cve_ent, nom_ent, tipo_delito, 
                pob_tot, pob_muj, total_enero_mayo) %>%
  mutate(tasa_enero_mayo = case_when(
    tipo_delito == "Feminicidio" ~ 
      round(total_enero_mayo/ pob_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ 
      round(total_enero_mayo/ pob_tot * 100000, digits = 2)),
    periodo = case_when(year == 2020 ~ "2020", T ~ "2015-2019")) %>% 
  group_by(periodo, cve_ent, nom_ent, tipo_delito) %>% 
  summarise(tasa_enero_mayo = round(mean(tasa_enero_mayo), digits = 2)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = periodo, values_from = tasa_enero_mayo,
              names_prefix = "tasa_") %>% 
  clean_names() %>% 
  mutate(diferencia = tasa_2020 - tasa_2015_2019,
         diferencia_grupo = case_when(
           diferencia > 0 ~ "Aumento",
           diferencia == 0 ~ "Sin variación",
           T ~ "Disminución"),
         diferencia_grupo = factor(diferencia_grupo, 
                                   levels = c("Aumento", "Sin variación",
                                              "Disminución"))) %>% 
  abreviar() %>% 
  arrange(tipo_delito, -diferencia) %>% 
  group_by(tipo_delito) %>% 
  mutate(rank = rank(-diferencia)) %>% 
  ungroup()

plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro"))

plot_fun <- function(delito) {
  ggplot(plot_df %>%  filter(str_detect(tipo_delito, delito))) +
    geom_segment(aes(y = tasa_2015_2019, yend = tasa_2020, 
                   x = reorder(abrev, desc(-rank)),
                   xend = reorder(abrev, desc(-rank)),
                   color = diferencia_grupo), 
               linetype = "solid", size = 2) + 
    geom_point(aes(y = tasa_2015_2019, x = abrev), size = 2,
             color = "purple" ) +
    geom_point(aes(y = tasa_2020, x = abrev), size = 2,
             color = "#E49C23" ) +
    facet_wrap(~tipo_delito, scales = "free", nrow = 1) +
    labs(y = "", x = "", color = "") +
    tema +
    theme(axis.text.x = element_text(angle = 90))
}

plot1 <- plot_fun(delito = "Homicidio")
plot2 <- plot_fun(delito = "Feminicidio")
plot3 <- plot_fun(delito = "Secuestro")

plot <- ggpubr::ggarrange(plot1, plot2, plot3, nrow = 3,
                  common.legend = T, legend = "top")

annotate_figure(plot, top = text_grob("Tasa acumulada a mayo 2020 vs Tasa acumulada promedio a mayo 2015-2019",
                                      size = 16, family = "Barlow Condensed", 
                                      hjust = 0.5, face = "bold"),
                bottom = text_grob("Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)",
                                   size = 10, family = "Barlow Condensed", hjust = 1.5, face = "italic"))

ggsave(files$graf7, width = 14, height = 8)


plot_df <-   filter(tempo, str_detect(tipo_delito, "Robo"),
                    str_detect(tipo_delito, "casa|transeúnte|transporte público")) %>% 
  mutate(tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito))

plot1 <- plot_fun(delito = "A casa habitación")
plot2 <- plot_fun(delito = "A transeúnte en espacio abierto")
plot3 <- plot_fun(delito = "A transeúnte en vía pública")
plot4 <- plot_fun(delito = "En transporte público colectivo")
plot5 <- plot_fun(delito = "En transporte público individual")

plot <- ggpubr::ggarrange(plot1, plot2, plot3, plot4, plot5, nrow =5,
                          common.legend = T, legend = "top",
                          heights = 500)

plot

annotate_figure(plot, top = text_grob("Tasa acumulada a mayo 2020 vs Tasa acumulada promedio a mayo 2015-2019",
                                      size = 16, family = "Barlow Condensed", 
                                      hjust = 0.5, face = "bold"),
                bottom = text_grob("Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)",
                                   size = 10, family = "Barlow Condensed", hjust = 1.5, face = "italic"))


ggsave(files$graf8, width = 14, height = 8)

# 