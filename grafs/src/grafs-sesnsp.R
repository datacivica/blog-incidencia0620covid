# Author: Adrián Lara
# Maintainer(s): Adrián Lara
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# =====================================================
# blog-incidencia0620covid/grafs/src/grafs-sesnsp.R
#

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, extrafont, scales, ggridges,
               ggpubr, ggrepel, lemon, viridis, janitor)

loadfonts(quiet = T)

files <- list(ide = here("clean-data/output/clean-ide.rds"),
              graf1 = here("grafs/output/1_fiebres-robos.svg"),
              graf2 = here("grafs/output/2_fiebres-alto.svg"),
              graf_distm1 = here("grafs/output/3_distribucion-mensual.svg"),
              graf_distm2 = here("grafs/output/4_distribucion-mensual.svg"),
              dif_homicidios = here("grafs/output/5_dif-homicidios.svg"),
              dif_feminicidios = here("grafs/output/5_dif-feminicidios.svg"),
              scatter_v1 = here("grafs/output/6_scatter-variaciones.svg"),
              scatter_v2 = here("grafs/output/7_scatter-variaciones.svg"))

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 22, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 11, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 12, family = "Barlow Condensed"),
        axis.title = element_text(size = 14, family = "Barlow Condensed"),
        legend.title = element_text(size = 15, family = "Barlow Condensed", hjust = 0.5),
        legend.text = element_text(size = 13, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold", family = "Barlow Condensed"))


# Abrimos bases

ide <- readRDS(files$ide) %>% 
  mutate(periodo = case_when(
    year >= 2015 & year <= 2019 ~ "2015-2019",
    year == 2020 ~ "2020"),
    tipo_delito = case_when(
      str_detect(tipo_delito, "Robo a transeúnte en espacio") ~
        "Robo a transeúnte en espacio abierto", T ~ tipo_delito))


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
    cve_ent == "11" ~ "GTO",
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

# Fiebres

tempo <- group_by(ide, year, periodo, tipo_delito, pob_nacional, pob_nacional_muj) %>% 
  summarise(across(starts_with("total"), sum, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at(vars(starts_with("total")), list(~case_when(
    tipo_delito == "Feminicidio" ~ round(./ pob_nacional_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ round(./ pob_nacional * 100000, digits = 2)))) %>% 
  rename_at(vars(total_enero:total_anual), 
            function(x) str_replace(x, "total", "tasa")) %>% 
  pivot_longer(tasa_enero:tasa_junio, names_to = "mes", values_to = "tasa") %>% 
  group_by(periodo, tipo_delito, mes) %>% 
  summarise(mean_tasa = mean(tasa, na.rm = T), sd_tasa = sd(tasa, na.rm = T),
            min_tasa = mean_tasa - (2 * sd_tasa), 
            max_tasa = mean_tasa + (2 * sd_tasa)) %>% 
  ungroup() %>% 
  mutate(mes = str_remove(mes, "tasa_"), 
         mes = str_to_title(mes),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", 
                                      "Mayo", "Junio")),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019",
                                              "2020")))

plot_fun <- function(delitos, titulo, subtitulo, plotrows) {
  
  plot_df <- filter(tempo, str_detect(tipo_delito, delitos)) %>% 
    mutate(tipo_delito = str_squish(str_to_sentence(str_remove(tipo_delito, "Robo"))))
  
  if(delitos != "Robo"){plot_df <- mutate(plot_df, tipo_delito = fct_rev(tipo_delito))}
  
  ggplot(plot_df, aes(x = mes, y = mean_tasa, color = periodo, group = periodo)) +
    geom_ribbon(data = plot_df %>% filter(periodo == "Promedio\n2015-2019"),
                aes(ymin = mean_tasa - sd_tasa, 
                    ymax = mean_tasa + sd_tasa, x = mes), fill = "#9a9aff",
                color = "#9a9aff",
                alpha = 0.4, size = 0.5, show.legend = F) +
    geom_line(alpha = 0.8, size = 1) +
    geom_point(size = 1.25, alpha = 0.6) +
    scale_color_manual(values = c("#0A8974", "#E3072A")) +
    facet_wrap(~tipo_delito, scales = "free", nrow = plotrows) +
    labs(title = titulo, subtitle = subtitulo,
         x = "", y = "Tasa por cada 100 mil habitantes", color = "", 
         caption = "Fuente: Elaboración propia con datos del SESNSP") +
    tema +
    theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          legend.position = "top")
}

plot_fun(delitos = "Robo", plotrows = 3,
         titulo = "Tasa nacional de carpetas de investigación por robo",
         subtitulo = "Tasas mensuales por tipo de robo")
ggsave(files$graf1, width = 14, height = 8)

plot_fun(delitos = "Homicidio|Feminicidio", plotrows = 1,
         titulo = "Tasa nacional de carpetas de investigación por homicidios y feminicidios",
         subtitulo = "Tasas mensuales por tipo de delito") +
  geom_line(alpha = 0.8, size = 1.5) +
  geom_point(size = 2.2, alpha = 0.6) +
  labs(caption = "Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)") 
ggsave(files$graf2, width = 14, height = 8)

# Distribución por mes

tempo <- select(ide, periodo, year:total_enero_junio) %>% 
  group_by(periodo, year, tipo_delito) %>% 
  summarise(across(starts_with("total_"), sum)) %>% 
  pivot_longer(total_enero:total_junio, names_to = "mes", values_to = "total") %>% 
  mutate(porcentaje = round(total/total_enero_junio, digits = 2),
         mes = str_to_title(str_remove(mes, "total_")),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", 
                                      "Mayo", "Junio")),
         periodo = str_replace(periodo, "2015-2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2020"))) %>% 
  relocate(total_enero_junio, .before = porcentaje) %>% 
  arrange(periodo) %>% 
  group_by(periodo, tipo_delito, mes) %>% 
  summarise(porcentaje = round(mean(porcentaje), digits = 2)) %>% 
  ungroup() %>% 
  filter(str_detect(tipo_delito, "Robo|Homicidio|Feminicidio")) 

plot_fun <- function(delitos) {
  
  plot_df <- filter(tempo, str_detect(tipo_delito, delitos)) %>% 
    mutate(tipo_delito = case_when(str_detect(tipo_delito, "Robo") ~ 
                                   str_remove(tipo_delito, "Robo"),
                                 T ~ tipo_delito),
           tipo_delito = str_to_sentence(str_squish(tipo_delito)))
  
  if(delitos != "Robo"){plot_df <- mutate(plot_df, tipo_delito = fct_rev(tipo_delito))}
  
  ggplot() +
    geom_bar(data = plot_df %>%  filter(periodo != "2020"), 
             aes(x = mes, y = porcentaje, fill = periodo),
             stat = "identity", width = 0.85, alpha = 0.5, color = "black",
             size = 0.8) +
    geom_bar(data = plot_df %>%  filter(periodo == "2020"),
             aes(x = mes, y = porcentaje, fill = periodo),
             stat = "identity", width = 0.65, alpha = 0.5, color = "black", 
             size = 0.8) +
    scale_fill_manual(values = c("#E49C23", "#0A8974")) +
    facet_rep_wrap(~tipo_delito, repeat.tick.labels = "bottom") +
    scale_y_continuous(labels = percent_format(5L), breaks = seq(0, 0.3, 0.1)) +
    labs(title = "Del total de carpetas abiertas hasta junio, ¿Cuántas se abrieron en cada mes?",
         subtitle = "Porcentajes mensuales por tipo de delito",
         x = "", y = "Porcentaje", 
         caption = "Fuente: Elaboración propia con datos del SESNSP") +
    tema +
    theme(axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
          legend.title = element_blank(),
          legend.position = "top") +
    guides(fill = guide_legend(reverse=TRUE))
  
}


plot_fun(delitos = "Robo")
ggsave(files$graf_distm1, width = 14, height = 8)

plot_fun(delitos = "Homicidio|Feminicidio")
ggsave(files$graf_distm2, width = 14, height = 8)

# Cambios por entidad

tempo <- select(ide, year, cve_ent, nom_ent, tipo_delito, 
                pob_tot, pob_muj, total_enero_junio) %>%
  mutate(tasa_enero_junio = case_when(
    tipo_delito == "Feminicidio" ~ 
      round(total_enero_junio/ pob_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ 
      round(total_enero_junio/ pob_tot * 100000, digits = 2)),
    periodo = case_when(year == 2020 ~ "2020", T ~ "2015-2019")) %>% 
  group_by(periodo, cve_ent, nom_ent, tipo_delito) %>% 
  summarise(tasa_enero_junio = round(mean(tasa_enero_junio), digits = 2)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = periodo, values_from = tasa_enero_junio, 
              names_prefix = "x_") %>%
  clean_names() %>% 
  mutate(diferencia = round(x_2020 * 100 / x_2015_2019 - 100, digits = 2))%>% 
  pivot_longer(cols = c(x_2020, x_2015_2019), names_to = "periodo",
               values_to = "tasa_enero_junio", names_prefix = "x_") %>% 
  select(periodo, everything()) %>% 
  mutate(periodo = str_replace(periodo, "2015_2019", "Promedio\n2015-2019"),
         periodo = factor(periodo, levels = c("Promedio\n2015-2019", "2020"))) %>% 
  abreviar() %>% 
  arrange(tipo_delito, diferencia)  
  

plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio")) %>%  
  mutate(abrev = fct_reorder(abrev, -diferencia))


g1 <- ggplot(plot_df, aes(x = periodo, y = fct_rev(abrev), fill = tasa_enero_junio)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = tasa_enero_junio), family = "Barlow Condensed",
            size = 6) +
  scale_fill_gradientn(colours = c("white", "#E49C23", "#E3072A"), 
                       values = rescale(c(10,20,40))) +
  labs(title = "Diferencia en Homicidios: tasa 2020 vs tasa promedio 2015-2019",
       subtitle = "Tasa de carpetas de investigación abiertas entre enero y junio",
       x = "", y = "", caption = "Fuente: Elaboración propia con datos del SESNSP") +
  coord_equal(ratio = 0.15) +
  guides(fill = F) +
  tema +
  theme(axis.text = element_text(size = 15))

g2 <- ggplot(plot_df, aes(x = "Diferencia", y = fct_rev(abrev), fill = diferencia )) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(diferencia >= 0, paste0("+", diferencia, "%"),
                               paste0(diferencia, "%"))), family = "Barlow Condensed",
            size = 5.5) +
  scale_fill_gradient2(low = "#65B0B8", mid = "white", high = "#DF1619",
                       midpoint = 0) +
  labs(x = "", y = "") +
  coord_equal(ratio = 0.15) +
  guides(fill = F) +
  tema +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, -0.5), "cm"),
        axis.text = element_text(size = 15))

graf <- egg::ggarrange(g1, g2, ncol = 2, widths = c(55,17))

ggsave(files$dif_homicidios, graf, width = 14, height = 8)

plot_df <- filter(tempo, str_detect(tipo_delito, "Feminicidio")) %>%  
  mutate(abrev = fct_reorder(abrev, -diferencia))

g1 <- ggplot(plot_df, aes(x = periodo, y = fct_rev(abrev), fill = tasa_enero_junio)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = tasa_enero_junio), family = "Barlow Condensed",
            size = 6) +
  scale_fill_gradientn(colours = c("white", "#E49C23", "#E3072A"), 
                       values = rescale(c(10,20,40))) +
  labs(title = "Diferencia en Feminicidios: tasa 2020 vs tasa promedio 2015-2019",
       subtitle = "Tasa de carpetas de investigación abiertas entre enero y junio",
       x = "", y = "", caption = "Fuente: Elaboración propia con datos del SESNSP") +
  coord_equal(ratio = 0.15) +
  guides(fill = F) +
  tema +
  theme(axis.text = element_text(size = 15))

g2 <- ggplot(plot_df, aes(x = "Diferencia", y = fct_rev(abrev), fill = diferencia )) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(is.infinite(diferencia), "", 
                               ifelse(diferencia >= 0, paste0("+", diferencia, "%"),
                               paste0(diferencia, "%")))),
            family = "Barlow Condensed",
            size = 5.5) +
  scale_fill_gradient2(low = "#65B0B8", mid = "white", high = "#DF1619",
                       midpoint = 20) +
  labs(x = "", y = "") +
  coord_equal(ratio = 0.15) +
  guides(fill = F) +
  tema +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, -0.5), "cm"),
        axis.text = element_text(size = 15))
g2
graf <- egg::ggarrange(g1, g2, ncol = 2, widths = c(55,17))

ggsave(files$dif_feminicidios, graf, width = 14, height = 8)


rm(g1, g2, graf)

# Scatter variaciones

tempo <- select(ide, year, cve_ent, nom_ent, tipo_delito, 
                pob_tot, pob_muj, total_enero_junio) %>%
  mutate(tasa_enero_junio = case_when(
    tipo_delito == "Feminicidio" ~ 
      round(total_enero_junio/ pob_muj * 100000, digits = 2),
    tipo_delito != "Feminicidio" ~ 
      round(total_enero_junio/ pob_tot * 100000, digits = 2)),
    periodo = case_when(year == 2020 ~ "2020", T ~ "2015-2019")) %>% 
  group_by(periodo, cve_ent, nom_ent, tipo_delito) %>% 
  summarise(tasa_enero_junio = round(mean(tasa_enero_junio), digits = 2)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = periodo, values_from = tasa_enero_junio, 
              names_prefix = "tasa_") %>%
  clean_names() %>% 
  mutate(diferencia = 
           round((tasa_2020 - tasa_2015_2019) / tasa_2015_2019, digits = 2),
         diferencia_grupo = case_when(
           diferencia > 0 ~ "Aumento",
           diferencia == 0 ~ "Sin variación",
           T ~ "Disminución"),
         diferencia_grupo = factor(diferencia_grupo, 
                                   levels = c("Aumento", "Disminución",
                                              "Sin variación"))) %>% 
  abreviar() %>% 
  arrange(tipo_delito, -diferencia) %>% 
  filter(!is.na(diferencia), !is.infinite(diferencia)) %>% 
  group_by(tipo_delito) %>% 
  mutate(rank = rank(-diferencia)) %>% 
  ungroup()


plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio")) %>% 
  mutate(tipo_delito = fct_rev(tipo_delito))

ggplot(plot_df, aes(x = "", y = diferencia, color = diferencia_grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.5), size = 2) +
  geom_text(data = plot_df %>% group_by(tipo_delito) %>% slice(1:5) %>% ungroup(), 
                   aes(x = "", y = diferencia,  color = diferencia_grupo,
                       label = abrev), show.legend = F, vjust = 0, hjust = 0,
                  position = position_jitter(seed = 1, width = 0.5), 
                   family = "Barlow Condensed",  size = 3) +
  geom_text(data = plot_df %>% group_by(tipo_delito) %>% slice(28:32) %>% ungroup(), 
            aes(x = "", y = diferencia,  color = diferencia_grupo,
                label = abrev), show.legend = F, vjust = 0, hjust = 0,
            position = position_jitter(seed = 1, width = 0.5), 
            family = "Barlow Condensed",  size = 3) +
  facet_wrap(~tipo_delito, scales = "free_y") +
  scale_color_manual(values = c("#22BFAC", "#E49C23", "#E3072A")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "¿Cuánto ha variado la tasa de incidencia en 2020 respecto a años anteriores?",
       subtitle = "Tasa promedio 2015-2019 vs Tasa 2020 (Con totales acumulados al mes de junio)",
       y = "Variación porcentual",
       caption = "Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)") +
  tema +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5))

ggsave(files$scatter_v1, width = 14, height = 8)

plot_df <-   filter(tempo, str_detect(tipo_delito, "Robo"),
                    str_detect(tipo_delito, "casa|transeúnte|transporte público|negocio")) %>% 
  mutate(tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito))

ggplot(plot_df, aes(x = "", y = diferencia, color = diferencia_grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.5), size = 2) +
  geom_text(data = plot_df %>% group_by(tipo_delito) %>% slice(1:5) %>% ungroup(), 
            aes(x = "", y = diferencia,  color = diferencia_grupo,
                label = abrev), show.legend = F, vjust = 0, hjust = 0,
            position = position_jitter(seed = 1, width = 0.5), 
            family = "Barlow Condensed",  size = 3) +
  geom_text(data = plot_df %>% group_by(tipo_delito) %>% slice(28:32) %>% ungroup(), 
            aes(x = "", y = diferencia,  color = diferencia_grupo,
                label = abrev), show.legend = F, vjust = 0, hjust = 0,
            position = position_jitter(seed = 1, width = 0.5), 
            family = "Barlow Condensed",  size = 3) +
  facet_wrap(~tipo_delito, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("#22BFAC", "#E49C23", "#E3072A")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "¿Cuánto ha variado la tasa de incidencia en 2020 respecto a años anteriores?",
       subtitle = "Tasa promedio 2015-2019 vs Tasa 2020 (Con totales acumulados al mes de junio)",
       y = "Variación porcentual",
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
        strip.text.x = element_text(size = 7))

ggsave(files$scatter_v2, width = 14, height = 8)

# 
