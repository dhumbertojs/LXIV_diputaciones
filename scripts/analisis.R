library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

inp <- "./output"

reel <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT_aHplhF1Mm8OcnrVg3y9PI0O-TihXb3Ca4pEhZ825a7nfSkA-a7l7lv3JGkmz8aXZAyc8fbuHgZPm/pub?gid=0&single=true&output=csv", 
                 fileEncoding = "UTF-8") %>% 
  rename(primero = paterno, segundo = materno) %>% 
  mutate(
    nombre_completo = paste(nombre, primero, segundo, sep = " "),
    nombre_completo = toupper(nombre_completo),
    nombre_completo = str_replace_all(nombre_completo,
                                      c("Á" = "A", "É" = "E",
                                        "Í" = "I", "Ó" = "O",
                                        "Ú" = "U")),
    identificador = 1:n()
    ) %>% 
  janitor::clean_names()

asis <- read.csv(paste(inp, "asistencias.csv", sep = "/")) %>% 
  mutate(
    nombre_completo = toupper(nombre_completo),
    nombre_completo = str_replace_all(nombre_completo,
                                      c("Á" = "A", "É" = "E",
                                        "Í" = "I", "Ó" = "O",
                                        "Ú" = "U"))
  )

asistencias <- left_join(reel, asis, by = "nombre_completo") %>% 
  select(
    ejercicio,
    numero_de_legislatura,
    nombre_completo, 
    nombre, 
    primero, 
    
    segundo,
    gp,
    entidad, 
    distrito_circunscripcion,
    tipo,
    
    grupo_o_representacion_parlamentaria,
    tipo_de_registro,
    tipo_de_sesion_o_reunion_celebrada_catalogo,
    hipervinculo_a_la_lista_de_asistencia,
    asistencia,
    id
  ) %>% 
  mutate(
    numero_de_legislatura = ifelse(numero_de_legislatura == "64", "LXIV", numero_de_legislatura),
    grupo_o_representacion_parlamentaria = str_replace_all(grupo_o_representacion_parlamentaria, 
                                                           "PARTIDO DE LA REVOLUCIÓN DEMOCRÁTICA", "PRD")
  )

comi <- read.csv(paste(inp, "comisiones.csv", sep = "/"))

plen <- read.csv(paste(inp, "pleno.csv", sep = "/"))

inic <- read.csv(paste(inp, "iniciativas.csv", sep = "/"))
