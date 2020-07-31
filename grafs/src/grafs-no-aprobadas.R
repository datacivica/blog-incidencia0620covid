# Author: AL
# Maintainer(s): AL
#
# Copyright:   2020, Data Cívica, GPL v2 or later
# =====================================================
# blog-incidencia0620covid/grafs/src/grafs-sesnsp.R
#

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, extrafont, scales,
               ggpubr, ggrepel, lemon, viridis, janitor)

loadfonts(quiet = T)

#--- Gráficas that didn't make the cut ---#

files <- list(scatter_v1 = here("grafs/output/6_scatter-variaciones.svg"),
              scatter_v2 = here("grafs/output/7_scatter-variaciones.svg"),
              graf_distm1 = here("grafs/output/3_distribucion-mensual.svg"),
              graf_distm2 = here("grafs/output/4_distribucion-mensual.svg"),
              graf7 = here("grafs/output/7_dumbell-altoimpacto.svg"), 
              graf8 = here("grafs/output/8_dumbell-robos.svg"),
              graf_dist1 = here("grafs/output/bars_dist1.svg"),
              graf_dist2 = here("grafs/output/bars_dist2.svg"),
              grafs_acumulados1 = here("grafs/output/fiebre_acum1.svg"),
              grafs_acumulados2 = here("grafs/output/fiebre_acum2.svg"))

# Tema

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

# Histogramas

tempo <- select(ide, periodo, year:total_enero_junio) %>% 
  group_by(periodo, tipo_delito) %>% 
  summarise(across(starts_with("total_"), sum)) %>% 
  pivot_longer(total_enero:total_junio, names_to = "mes", values_to = "total") %>% 
  mutate(mes = str_remove(mes, "total_"),
         mes = str_to_title(mes),
         porcentaje = round(total/total_enero_junio, digits = 2),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", 
                                      "Mayo", "Junio"))) %>% 
  relocate(total_enero_junio, .before = porcentaje) %>% 
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
       subtitle = "Porcentajes mensuales sobre el total de carpetas abiertas hasta junio",
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
       subtitle = "Porcentajes mensuales sobre el total de carpetas abiertas hasta junio",
       x = "", y = "Porcentaje", 
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave(files$graf_dist2, width = 14, height = 8)


# Porcentajes acumulados

tempo <- select(ide, periodo, tipo_delito:total_enero_junio) %>% 
  group_by(periodo, tipo_delito) %>% 
  summarise(across(starts_with("total_"), sum)) %>% 
  pivot_longer(total_enero:total_junio, names_to = "mes", values_to = "total") %>% 
  mutate(mes = str_remove(mes, "total_"),
         mes = str_to_title(mes),
         porcentaje = round(total/total_enero_junio, digits = 2),
         mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo",
                                      "Junio"))) %>% 
  relocate(total_enero_junio, .before = porcentaje) %>% 
  arrange(periodo) %>% 
  group_by(periodo, tipo_delito) %>% 
  mutate(porcentaje_acumulado = cumsum(porcentaje)) %>%
  ungroup()

plot_df <-   filter(tempo, str_detect(tipo_delito, "Robo"),
                    str_detect(tipo_delito, "casa|transeúnte|transporte público")) %>% 
  mutate(tipo_delito = str_remove(tipo_delito, "Robo"), 
         tipo_delito = str_to_sentence(tipo_delito)) 

ggplot(plot_df, aes(x = mes, y = porcentaje_acumulado, 
                    color = periodo, group = periodo)) +
  geom_line() +
  geom_point() +
  facet_rep_wrap(~tipo_delito, repeat.tick.labels = "bottom") +
  scale_y_continuous(labels = percent_format(accuracy = 5L),
                     breaks = seq(0, 1 , 0.2)) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  labs(title = "Porcentaje acumulado de carpetas de investigación agregado por mes", 
       subtitle = "Porcentajes por tipo de delito",
       y = "Porcentaje acumulado",
       x = "",
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.title = element_blank(),
        legend.position = "top")

ggsave(files$grafs_acumulados1, width = 14, height = 8)

plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro"))

ggplot(plot_df, aes(x = mes, y = porcentaje_acumulado, 
                    color = periodo, group = periodo)) +
  geom_line() +
  geom_point() +
  facet_rep_wrap(~tipo_delito, repeat.tick.labels = "bottom") +
  scale_y_continuous(labels = percent_format(accuracy = 5L),
                     breaks = seq(0, 1 , 0.2)) +
  scale_color_manual(values = c("#E49C23", "#0A8974", "#E3072A")) +
  labs(title = "Porcentaje acumulado de carpetas de investigación agregado por mes", 
       subtitle = "Porcentajes por tipo de delito",
       y = "Porcentaje acumulado",
       x = "",
       caption = "Fuente: Elaboración propia con datos del SESNSP") +
  tema +
  theme(legend.title = element_blank(),
        legend.position = "top")

ggsave(files$grafs_acumulados2, width = 14, height = 8)


# Dumbells 

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
  mutate(rank = row_number()) %>% 
  ungroup()

plot_df <- filter(tempo, str_detect(tipo_delito, "Homicidio|Feminicidio|Secuestro"))

ggplot(plot_df) +
  geom_segment(aes(y = tasa_2015_2019, yend = tasa_2020, 
                   x = reorder(abrev, desc(-rank)),
                   xend = reorder(abrev, desc(-rank)),
                   color = diferencia_grupo), 
               linetype = "solid", size = 2) + 
  geom_point(aes(y = tasa_2015_2019, x = abrev), size = 2,
             color = "purple" ) +
  geom_point(aes(y = tasa_2020, x = abrev), size = 2,
             color = "#E49C23" ) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 2) +
  coord_flip() +
  labs(y = "", x = "", color = "") +
  tema +
  theme(axis.text.x = element_text(angle = 90))


ggplot(plot_df) +
  geom_segment(aes(x = tasa_2015_2019, xend = tasa_2020, 
                   y = rank,
                   yend = rank ,
                   color = diferencia_grupo), 
               linetype = "solid", size = 2) + 
  geom_point(aes(x = tasa_2015_2019, y = rank), size = 2,
             color = "purple" ) +
  geom_point(aes(x = tasa_2020, y = rank), size = 2,
             color = "#E49C23" ) +
  geom_text(data = plot_df %>% filter(diferencia_grupo != "Disminución"), 
            aes(x = tasa_2020, y = rank, label = abrev),
            family = "Barlow Condensed", hjust = -1, size = 3.5) +
  facet_wrap(~tipo_delito, scales = "free", nrow = 1) +
  labs(y = "", x = "", color = "") +
  tema +
  theme(axis.text.x = element_text(angle = 90))

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

annotate_figure(plot, top = text_grob("Tasa acumulada a junio 2020 vs Tasa acumulada promedio a junio 2015-2019",
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

annotate_figure(plot, top = text_grob("Tasa acumulada a junio 2020 vs Tasa acumulada promedio a junio 2015-2019",
                                      size = 16, family = "Barlow Condensed", 
                                      hjust = 0.5, face = "bold"),
                bottom = text_grob("Fuente: Elaboración propia con datos del SESNSP\n(tasa de feminicidios calculada con población total de mujeres)",
                                   size = 10, family = "Barlow Condensed", hjust = 1.5, face = "italic"))


ggsave(files$graf8, width = 14, height = 8)

#