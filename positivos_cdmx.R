# Titulo:   Análisis de casos positivos y fallecimientos por COVID en la CDMX
# Datos:     
# Fecha:    14-12-2020
# Autores:  Gatitos Contra la Desigualdad


# --- Directory and packages
rm(list = ls())

library(pacman)

p_load(survival, data.table, tidyverse, reshape2, gghighlight, RColorBrewer, wesanderson, 
       ggpubr, gridExtra, magrittr,ggrepel, lubridate, zoo, scales)

# ---  Importar los datos
datitos <- read.csv("~/Google Drive/Escuela/2020A/Covid y Dani/Bases/base-covid-sinave.csv", 
                    header = T, )

glimpse(datitos)

table(datitos$resultado_definitivo)
table(datitos$semana_defuncion)
table(datitos$evolucion_caso)


#Positivos----


#fecha_inicio_sintomas
datitos$fecha_inicio_sintomas <- as_datetime(datitos$fecha_inicio_sintomas)



positivos_diarios <-
  datitos %>% 
  filter( resultado_definitivo == "SARS-CoV-2") %>% 
  mutate (dia = date(fecha_inicio_sintomas),
          positivo = 1) %>% 
  group_by(dia) %>% 
  summarise( total_diario = sum(positivo)) 

glimpse(positivos_diarios)

write_csv(positivos_diarios, "www/positivos_diarios.csv")

positivos_diarios <- positivos_diarios %>% 
  mutate( promedio_5 = rollapplyr (total_diario, 5,mean, partial=TRUE))
                              
positivos_diarios$dia <- as_datetime(positivos_diarios$dia)
# assignmeent pipe %<>%




colores <- read.csv("www/color_semaforo_cdmx.csv")
glimpse(colores)
names(colores)
colores$dia  <- as_datetime(colores$dia)


colores %<>% 
  rename(color = var4) %>% 
  select(dia, color)

positivos_diarios %<>%
 left_join(colores, by="dia") 


positivos_diarios <- positivos_diarios %>% 
filter(!(row_number() >= (n() - 4)))
    

semaforo = data.frame(x1=c("2020-02-22", "2020-06-01", "2020-06-29"),
                   x2=c("2020-05-31", "2020-06-28", "2020-12-04"),
                   col=c("sin semáforo", "rojo", "naranja"))
semaforo$x1 <- as_datetime(semaforo$x1)
semaforo$x2 <- as_datetime(semaforo$x2)


#Función para determinar theme de las gráficas
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      #axis.line = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(1, .5, .5, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(0.2, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9,hjust = 1,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}



positivos_diarios %>% 
  ggplot()+
  geom_line( aes(x=dia,
                 y=promedio_5)) +
  geom_rect(data = semaforo, 
            aes(xmin = x1, xmax = x2, 
                ymin = -Inf, ymax = Inf, 
                fill = col), 
                alpha = 0.2)+
  scale_fill_manual(values=c("#ff931f","#ff1b0a","#f5f5f3"))+
  scale_x_datetime(breaks = date_breaks("10 days")) +
  scale_y_continuous(breaks = seq(0,2500,500), limits = c(0,2500)) +
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         plot.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  labs ( x = "Fecha (inicio síntomas)",
         y = "Total positivos (prom. mov. 5 días)",
         title = "Casos positivos por COVID19 en CDMX",
         fill = "Semáforo",
         subtitle = "Con corte al 11 de diciembre 2020",
         caption = "Nota: Los casos positivos identificados dependen en gran medida de número de pruebas.
         A mediados de año las pruebas eran insuficientes, cerca de 3 mil diarias en promedio. 
         El Gob. de la CDMX ha aumentando las pruebas de manera importante en últimos meses, hasta 11 mil diarias.
         Fuente: Elaborado por @gatitosvsdesig, con datos de 'Covid-19 SINAVE Ciudad de México de ADIP-CDMX'")
  








#Fallecimientos----

fallecidos_diarios <-
  datitos %>% 
  filter( resultado_definitivo == "SARS-CoV-2" & evolucion_caso== "DEFUNCION") %>% 
  mutate (dia = date(fecha_defuncion),
          fallecido = 1) %>% 
  group_by(dia) %>% 
  summarise( total_diario = sum(fallecido)) 

table(fallecidos_diarios$total_diario)


fallecidos_diarios <- fallecidos_diarios %>% 
  mutate( promedio_5 = rollapplyr (total_diario, 5,mean, partial=TRUE))

fallecidos_diarios$dia <- as_datetime(fallecidos_diarios$dia)

fallecidos_diarios %<>%
  left_join(colores, by="dia") 

fallecidos_diarios  %<>%
  filter(!(row_number() >= (n() - 8)))

summary(fallecidos_diarios$promedio_5)


fallecidos_diarios %>% 
  ggplot()+
  geom_line( aes(x=dia,
                 y=promedio_5)) +
  geom_rect(data = semaforo, 
            aes(xmin = x1, xmax = x2, 
                ymin = -Inf, ymax = Inf, 
                fill = col), 
            alpha = 0.2)+
  scale_fill_manual(values=c("#ff931f","#ff1b0a","#f5f5f3"))+
  scale_x_datetime(breaks = date_breaks("10 days")) +
  scale_y_continuous(breaks = seq(0,140,10), limits = c(0,140)) +
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         plot.background = element_rect(fill = "#F5F5F3", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  labs ( x = "Fecha (defunción)",
         y = "Total fallecimientos (prom. mov. 5 días)",
         title = "Fallecimientos por COVID19 en CDMX",
         fill = "Semáforo",
         subtitle = "(sólo identificados con prueba). Con corte al 11 de diciembre 2020, se incluyen sólo hasta 30 de nov.",
         caption = "Nota: Los fallecimientos identificados por COVID19 dependen en gran medida de número de pruebas.
         El dato más confiable para conocer impacto de crisis son exceso de muertes, 
         pero ese dato toma entre uno y dos meses en hacerse público.
         Fuente: Elaborado por @gatitosvsdesig, con datos de 'Covid-19 SINAVE Ciudad de México de ADIP-CDMX'")

