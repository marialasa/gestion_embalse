# Librerías

library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)
library(ggthemes)
library(lubridate)
library(ggridges)

# Carga datasets

attach(Dataset)
attach(Dataset_Medias)

# Data Wrangling

Dataset_Mensual <- Dataset %>%
  separate(Fecha, into = c("Anio", "Mes", "Dia"), sep = "-", remove = FALSE)

mean(Dataset$Nivel, na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 3) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 3) %>%
  pull(Nivel) %>%
  median(na.rm = TRUE)

Dataset_10_13 <- Dataset_Mensual %>%
                 filter(Anio == 2010 & Anio == 2011 & Anio == 2012 &  Anio == 2013)

Dataset_10_13$Mes <- factor(Dataset_10_13$Mes, levels = c(1:12))

Dataset_Mensual$Mes <- factor(Dataset_Mensual$Mes, levels = c(1:12))

Dataset_Medias$Dia <- factor(Dataset_Medias$Dia, levels = c(1:31))

Dataset_Medias$Mes <- factor(Dataset_Medias$Mes, levels = c(1:12))

# Plots

Plot_Pre14 <- ggplot(Dataset_10_13, aes(Mes, Nivel)) +
  geom_hline(yintercept = 46.5, color = "red", linetype = "dashed") +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "Enero",
      "Febrero",
      "Marzo",
      "Abril",
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Septiembre",
      "Octubre",
      "Noviembre",
      "Diciembre"
    )
  ) +
  theme_fivethirtyeight() +
  ggtitle("Niveles Embalse de Río Tercero, Córdoba") +
  labs(subtitle = "Serie de tiempo 2010-2013. Segmentación mensual sobre 1.352 observaciones",
       caption="CC By-NC-SA 4.0 marialasa.com") +
  theme(plot.title = element_text(face="bold", colour="#3C3C3C", size=20)) +
  ylab("Nivel (metros)") +
  xlab("Mes") +
  theme(axis.text.x=element_text(size=9, angle = 35)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.y=element_text(size=11, face = "bold")) +
  theme(axis.title.x=element_text(size=11, face = "bold")) +
  annotate("text", x = 10, y = 46.6, label= "Cota vertedero (m)", color = "red")

ggsave(
  Plot_Pre14,
  filename = "Plot_Pre14.png",
  width = 30,
  height = 18,
  units = "cm",
  dpi = 300
)

Plot_Completo <- ggplot(Dataset_Mensual, aes(Mes, Nivel)) +
  geom_hline(yintercept = 46.5, color = "red", linetype = "dashed") +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "Enero",
      "Febrero",
      "Marzo",
      "Abril",
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Septiembre",
      "Octubre",
      "Noviembre",
      "Diciembre"
    )
  ) +
  scale_y_continuous(breaks = c(42, 43, 44, 45, 46, 47, 48)) +
  theme_fivethirtyeight() +
  ggtitle("Niveles Embalse de Río Tercero, Córdoba") +
  labs(subtitle = "Serie de tiempo 2010-2019. Segmentación mensual sobre 3.360 observaciones",
       caption="CC By-NC-SA 4.0 marialasa.com") +
  theme(plot.title = element_text(face="bold", colour="#3C3C3C", size=20)) +
  ylab("Nivel (metros)") +
  xlab("Mes") +
  theme(axis.text.x=element_text(size=9, angle = 35)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.y=element_text(size=11, face = "bold")) +
  theme(axis.title.x=element_text(size=11, face = "bold")) +
  annotate("text", x = 10, y = 46.6, label= "Cota vertedero (m)", color = "red")

ggsave(
  Plot_Completo,
  filename = "Plot_Completo.png",
  width = 30,
  height = 19,
  units = "cm",
  dpi = 300
)

Heatmap <- ggplot(Dataset_Medias, aes(Dia, Mes, fill = Nivel_Mean)) + 
           geom_tile(colour="white", size = 0.1) +
           scale_y_discrete(
             limits = rev(levels(as.factor(Dataset_Medias$Mes))),
              labels = c(
                "Diciembre",
                "Noviembre",
                "Octubre",
                "Septiembre",
                "Agosto",
                "Julio",
                "Junio",
                "Mayo",
                "Abril",
                "Marzo",
                "Febrero",
                "Enero"
              )
            ) +
           theme_fivethirtyeight() +
            scale_fill_viridis(begin = 0, end = 1, direction = -1,
                               discrete = FALSE, option = "D") +
            ggtitle("Niveles Embalse de Río Tercero, Córdoba") +
            labs(subtitle = "Serie de tiempo 2010-2019. Promedios diarios sobre 3.360 observaciones",
                 caption="CC By-NC-SA 4.0 marialasa.com",
                 fill = "Cota (m)") +
            theme(plot.title = element_text(face="bold", colour="#3C3C3C", size=20)) +
            ylab("Mes") +
            xlab("Día") +
            theme(axis.text.x=element_text(size = 9)) +
            theme(axis.text.y=element_text(size = 9)) +
            theme(axis.title.y=element_text(size = 11, face = "bold")) +
            theme(axis.title.x=element_text(size = 11, face = "bold")) +
            theme(legend.position = "right", legend.direction = "vertical")

ggsave(
  Heatmap,
  filename = "Heatmap.png",
  width = 30,
  height = 13,
  units = "cm",
  dpi = 300
)

# Cálculo medias para heatmap. Cambiar meses y días

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 1) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 2) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 3) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 4) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 5) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 6) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 7) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 8) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 9) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 10) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 11) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 12) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 13) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 14) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 15) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 16) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 17) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 18) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 19) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 20) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 21) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 22) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 23) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 24) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 25) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 26) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 27) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 28) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 29) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 30) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)

Dataset_Mensual %>%
  filter(Mes == 2 & Dia == 31) %>%
  pull(Nivel) %>%
  mean(na.rm = TRUE)
=
