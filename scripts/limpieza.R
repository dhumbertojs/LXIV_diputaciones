library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)
library(tidyr)
library(lubridate)

datos <- "./datos"
out <- "./output"

a <- list.dirs(datos)
a

# Asistencias -------------------------------------------------------------
inp <- a[2]
list.files(inp)

asistencias <- map(list.files(inp),
                   ~ read_excel(paste(inp, .x, sep = "/"), 
                                sheet = 1,
                                skip = 5) %>% 
                     mutate(id = .x) %>% 
                     clean_names(.) %>% 
                     select(-1))
nombres <- c(
  "ejercicio",
  "fecha_de_inicio_del_periodo_que_se_informa",
  "fecha_de_termino_del_periodo_que_se_informa",
  "numero_de_legislatura",
  "duracion_de_la_legislatura",
  
  "ano_legislativo_catalogo",
  "periodo_de_sesiones_catalogo",
  "fecha_de_inicio_del_periodo_de_sesiones",
  "fecha_de_termino_del_periodo_de_sesiones",
  "numero_de_sesion",
  
  "tipo_de_sesion_o_reunion_celebrada_catalogo",
  "fecha_de_la_sesion_o_reunion_celebrada",
  "numero_de_gaceta_parlamentaria_o_equivalente",
  "fecha_de_la_gaceta_parlamentaria_o_equivalente",
  "organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo",
  
  "legisladores_asistentes_cargo_grupo_y_registro_tabla_335527",
  "denominacion_de_la_normatividad_que_obliga_a_la_publicacion_de_la_lista_de_asistencia",
  "fundamento_legal_que_obliga_a_la_publicacion_de_la_lista_de_asistencia",
  "hipervinculo_a_la_lista_de_asistencia",
  "area_s_responsable_s_que_genera_n_posee_n_publica_n_y_actualizan_la_informacion",
  
  "fecha_de_validacion",
  "fecha_de_actualizacion",
  "nota",
  "id"     
)

#1:14 son iguales

first <- asistencias[1:14]
first <- lapply(first, function(x) x[-1,])
first <- lapply(first, setNames, nombres)
first <- bind_rows(first)

sec <- asistencias[15:19]
sec <- bind_rows(sec) %>% 
  rename(id = id_2)

asist <- bind_rows(first, sec) %>% 
  rename(identificador = legisladores_asistentes_cargo_grupo_y_registro_tabla_335527)

asistencias <- map(list.files(inp),
                   ~ read_excel(paste(inp, .x, sep = "/"), 
                                sheet = 6, 
                                skip = 2) %>% 
                     mutate(id = .x) %>% 
                     clean_names(.) %>% 
                     mutate_all(as.character))

lista <- bind_rows(asistencias) %>% 
  rename(identificador = id, id = id_2) %>% 
  mutate(
    nombre_completo = paste(nombre, primer_apellido, segundo_apellido, sep = " ")
    ) %>% 
  select(-c(nombre, primer_apellido, segundo_apellido))

final <- left_join(lista, asist) %>% 
  group_by(
    ejercicio, 
    id,
    numero_de_sesion
  ) %>% 
  mutate(
    asistencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    total_sesiones = 110,
    tipo_de_registro = tolower(tipo_de_registro),
    tipo_de_registro = str_replace_all(tipo_de_registro, 
                                       c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u")),
    tipo_de_registro = str_trim(tipo_de_registro, "both"),
    
    grupo_o_representacion_parlamentaria = toupper(grupo_o_representacion_parlamentaria),
    grupo_o_representacion_parlamentaria = str_remove_all(grupo_o_representacion_parlamentaria, 
                                                          "GRUPO PARLAMENTARIO DE "),
    grupo_o_representacion_parlamentaria = str_remove_all(grupo_o_representacion_parlamentaria, 
                                                          "GRUPO PARLAMENTARIO DEL "),
    grupo_o_representacion_parlamentaria = case_when(
      str_detect(grupo_o_representacion_parlamentaria, "ACCIÓN") ~ "PAN",
      str_detect(grupo_o_representacion_parlamentaria, "REVOLUCION") ~ "PRD",
      str_detect(grupo_o_representacion_parlamentaria, "TRABAJO") ~ "PT",
      str_detect(grupo_o_representacion_parlamentaria, "SOCIAL") ~ "PES",
      str_detect(grupo_o_representacion_parlamentaria, "REGENERACIÓN") ~ "MORENA",
      str_detect(grupo_o_representacion_parlamentaria, "INSTITUCIONAL") ~ "PRI",
      str_detect(grupo_o_representacion_parlamentaria, "VERDE") ~ "PVEM",
      str_detect(grupo_o_representacion_parlamentaria, "SIN") ~ "SP",
      str_detect(grupo_o_representacion_parlamentaria, "CIUDADANO") ~ "MC",
      T ~ grupo_o_representacion_parlamentaria
    )
  )
#entre 2018 y 2020 ha habido 110 sesiones registradas (1 sola de 2020)

levels(as.factor(final$numero_de_legislatura))
#todo es de la legislatura actual

stat <- final %>% 
  group_by(ejercicio, 
           nombre_completo, 
           cargo, 
           grupo_o_representacion_parlamentaria, 
           tipo_de_registro, 
           organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo) %>% 
  count()
  
write.csv(final, paste(out, "asistencias.csv", sep = "/"), row.names = F)
# Comisiones -----------------------------------------------------------------

inp <- a[3]

comisiones <- map(list.files(inp),
                  ~ read_excel(paste(inp, .x, sep = "/"), 
                               skip = 6) %>% 
                    mutate(archivo = .x) %>% 
                    clean_names(.) %>% 
                    mutate_all(as.character)
                  )

first <- comisiones[sapply(comisiones, ncol)==24]
sec <- comisiones[sapply(comisiones, ncol)==25]
thi <- comisiones[sapply(comisiones, ncol)==26]
fo <- comisiones[sapply(comisiones, ncol)==27]

first <- bind_rows(first)
first <- first %>% 
  select(
    
    numero_de_legislatura,
    numero_de_sesion_o_reunion,
    sesion_celebrada_y_el_tipo_de_la_misma,
    organismo_que_llevo_a_cabo_la_sesion_o_reunion,
    tipo_de_votacion,
    tipo_de_asunto_que_se_vota,
    titulo_del_asunto,
    legisladores_asistentes_tabla_14475,
    sesion_celebrada_y_el_tipo_de_la_misma,
    hipervinculo_al_acta,
    archivo,
    nota
         ) %>% 
  rename(id = legisladores_asistentes_tabla_14475,
         organismo = organismo_que_llevo_a_cabo_la_sesion_o_reunion)

sec <- bind_rows(sec)
sec <- sec %>% 
  select(
    numero_de_legislatura,
    numero_de_sesion_o_reunion,
    sesion_celebrada_y_el_tipo_de_la_misma,
    organismo_que_llevo_a_cabo_la_sesion_o_reunion_en_su_caso_catalogo,
    #tipo_de_votacion,
    #tipo_de_asunto_que_se_vota,
    #titulo_del_asunto,
    legisladores_as_que_presenten_un_voto_y_su_tipo_tabla_335295,
    sesion_celebrada_y_el_tipo_de_la_misma,
    hipervinculo_al_dictamen,
    hipervinculo_al_documento,
    archivo,
    nota
  ) %>% 
  rename(id = legisladores_as_que_presenten_un_voto_y_su_tipo_tabla_335295,
         organismo = organismo_que_llevo_a_cabo_la_sesion_o_reunion_en_su_caso_catalogo)

thi <- bind_rows(thi)
thi <- thi %>% 
  select(
    numero_de_legislatura,
    numero_de_sesion_o_reunion,
    sesion_celebrada_y_el_tipo_de_la_misma,
    organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo,
    tipo_de_votacion_catalogo,
    tipo_de_asunto_que_se_vota,
    titulo_del_asunto,
    nombre_completo_de_los_legisladores_as_asistentes_y_el_sentido_del_voto_tabla_335271,
    sesion_celebrada_y_el_tipo_de_la_misma,
    hipervinculo_al_acta_de_votacion,
    archivo,
    nota
  ) %>% 
  rename(id = nombre_completo_de_los_legisladores_as_asistentes_y_el_sentido_del_voto_tabla_335271, 
         organismo = organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo,
         tipo_de_votacion = tipo_de_votacion_catalogo)

fo <- bind_rows(fo)
fo <- fo %>% 
  select(
    numero_de_legislatura,
    numero_de_sesion_o_reunion,
    sesion_celebrada_y_el_tipo_de_la_misma,
    organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo,
    tipo_de_votacion_catalogo,
    tipo_de_asunto_que_se_vota,
    titulo_del_asunto,
    nombre_completo_de_los_legisladores_as_asistentes_y_el_sentido_del_voto_tabla_335271,
    sesion_celebrada_y_el_tipo_de_la_misma,
    hipervinculo_al_acta_de_votacion,
    archivo,
    nota
  ) %>% 
  rename(id = nombre_completo_de_los_legisladores_as_asistentes_y_el_sentido_del_voto_tabla_335271,
    organismo = organismo_que_llevo_a_cabo_la_sesion_o_reunion_catalogo,
    tipo_de_votacion = tipo_de_votacion_catalogo
  )

#compare_df_cols(first, thi)

com <- bind_rows(
  first, sec, thi, fo
) %>% 
  filter(numero_de_legislatura == "LXIV") %>% 
  mutate(
    reunion0 = ifelse(str_detect(nota, "o se genero"), 1, 0),
    reunion1 = ifelse(str_detect(nota, "o hubo"), 1, 0),
    reunion2 = ifelse(str_detect(nota, "se gene"), 1, 0),
    reunion3 = ifelse(str_detect(nota, "no gene"), 1, 0),
    reunion4 = ifelse(str_detect(nota, "no real"), 1, 0),
    reunion5 = ifelse(str_detect(nota, "no se real"), 1, 0),
    reunion6 = ifelse(str_detect(nota, "NO HAB"), 1, 0),
    reunion7 = ifelse(str_detect(nota, "no se votan"), 1, 0),
    reunion8 = ifelse(str_detect(nota, "NO gene"), 1, 0),
    reunion9 = ifelse(str_detect(nota, "no se lleva"), 1, 0),
    reunion10 = ifelse(str_detect(nota, "o se"), 1, 0),
    reunion = rowSums(cbind(reunion0, reunion1, reunion2, 
                            reunion3, reunion4, reunion5, 
                            reunion6, reunion7, reunion8,
                            reunion9, reunion10), na.rm = T)
    ) %>% 
  select(-c(reunion0, reunion1, reunion2, 
            reunion3, reunion4, reunion5, 
            reunion6, reunion7, reunion8,
            reunion9, reunion10)) %>% 
  filter(
    reunion == 0
  )

archivos <- com %>% 
  select(archivo) %>% 
  distinct(.)
archivos <- archivos$archivo

c1 <- map(archivos, 
           ~ read_excel(paste(inp, .x, sep = "/"), 
                        sheet = 5, skip = 2) %>% 
            mutate(archivo = .x) %>% 
            mutate_all(as.character) %>% 
            clean_names())
c1 <- c1[sapply(c1, ncol)==6]

c1 <- bind_rows(c1)

c2 <- map(archivos, 
          ~ read_excel(paste(inp, .x, sep = "/"), 
                       sheet = 6, skip = 2) %>% 
            mutate(archivo = .x) %>% 
            mutate_all(as.character) %>% 
            clean_names())
c2 <- c2[sapply(c2, ncol)==6]
c2 <- bind_rows(c2)

c_vot <- bind_rows(c1, c2)

comisiones <- left_join(c_vot, com, by = c("id", "archivo")) %>% 
  tidyr::unite(
    tipo_de_voto, tipo_de_voto_catalogo, tipo_de_votacion, remove = T
  ) %>% 
  mutate(
    tipo_de_voto = str_remove_all(tipo_de_voto, "NA"),
    tipo_de_voto = str_remove_all(tipo_de_voto, "_")
  )

write.csv(comisiones, paste(out, "comisiones.csv", sep = "/"), row.names = F)

# Pleno -------------------------------------------------------------------

inp <- a[6]
list.files(inp)

pleno <- map(list.files(inp),
             ~ read_excel(paste(inp, .x, sep = "/"),
                          skip = 6) %>% 
               mutate(archivo = .x) %>% 
               mutate_all(as.character) %>% 
               clean_names)
pleno <- bind_rows(pleno) %>% 
  rename(
    id = nombre_completo_de_los_legisladores_as_asistentes_y_el_sentido_del_voto_tabla_335271
  )

p_lista <- map(list.files(inp),
               ~ read_excel(paste(inp, .x, sep = "/"),
                            sheet = 6,
                            skip = 2) %>% 
                 mutate_all(as.character) %>% 
                 mutate(archivo = .x) %>% 
                 clean_names
)

p_lista <- bind_rows(p_lista)

v_pleno <- left_join(p_lista, pleno, by = c("id", "archivo"))

write.csv(v_pleno, paste(out, "pleno.csv", sep = "/"), row.names = F)

# iniciativas -------------------------------------------------------------

inp <- a[5]

ini <- map(list.files(inp),
           ~ read_excel(paste(inp, .x, sep = "/"),
                        skip = 5)
           )

n <- c(
  "ejercicio",
  "fecha_de_inicio_del_periodo_que_se_informa",
  "fecha_de_termino_del_periodo_que_se_informa",
  "numero_de_legislatura",
  "duracion_de_la_legislatura",
  
  "ano_legislativo_catalogo",
  "periodo_de_sesiones_catalogo",
  "fecha_de_inicio_del_periodo_de_sesiones",
  "fecha_de_termino_del_periodo_de_sesiones",
  "numero_de_sesion_o_reunion",
  
  "numero_de_gaceta_parlamentaria_o_equivalente",
  "fecha_en_la_que_se_recibio_la_iniciativa_de_ley_o_decreto_dia_mes_ano",
  "tipo_de_documento_catalogo",
  "titulo_de_la_iniciativa_de_ley_decreto_o_acuerdo",
  "denominacion_del_organo_legislativo",
  
  "cargo_del_presentador_de_la_iniciativa",
  "hipervinculo_al_documento",
  "denominacion_de_la_comision_a_la_que_se_turno",
  "periodo_de_prorroga",
  "sentido_del_dictamen_en_su_caso",
  
  "fecha_del_dictamen",
  "hipervinculo_al_dictamen",
  "denominacion_de_la_normatividad_que_obliga_a_la_publicacion_de_las_iniciativas",
  "fundamento_legal_que_obliga_a_la_publicacion_de_la_lista_de_asistencia",
  "area_s_responsable_s_que_genera_n_posee_n_publica_n_y_actualizan_la_informacion",
  
  "fecha_de_validacion",
  "fecha_de_actualizacion",
  "nota")

first <- ini[1:19]
first <- lapply(first, function(x) x[-1,])
fa <- first[sapply(first, ncol)==28]
fa <- lapply(fa, setNames, n)
fa <- bind_rows(fa)

fb <- first[sapply(first, ncol)==29]
fb <- lapply(fb, function(x) x[,-1])
fb <- lapply(fb, setNames, n)
fb <- bind_rows(fb)

sec <- ini[20:22]
sec <- lapply(sec, function(x) x[,-1])
sec <- lapply(sec, setNames, n)
sec <- bind_rows(sec)

inic <- bind_rows(fa, fb, sec)

inic <- inic %>% 
  mutate(
    dummy = cargo_del_presentador_de_la_iniciativa,
    
    nombre = str_remove_all(cargo_del_presentador_de_la_iniciativa, "Suscrita por las"),
    nombre = str_remove_all(nombre, "Suscrita por los"),
    nombre = str_remove_all(nombre, "Suscrita por la"),
    nombre = str_remove_all(nombre, "Suscrita por el"),
    nombre = str_remove_all(nombre, "Suscrita por"),
    nombre = str_remove_all(nombre, "PAN"),
    nombre = str_remove_all(nombre, "PRD"),
    nombre = str_remove_all(nombre, "PRI"),
    nombre = str_remove_all(nombre, "PVEM"),
    nombre = str_remove_all(nombre, "PT"),
    nombre = str_remove_all(nombre, "MC"),
    nombre = str_remove_all(nombre, "PES"),
    nombre = str_remove_all(nombre, "Morena"),
    nombre = str_remove_all(nombre, "Moviemiento Ciudadano"),
    nombre = str_remove_all(nombre, "MORENA"),
    nombre = toupper(nombre),
    
    filtro = ifelse(str_detect(nombre, "DIP"),1, 0),
    
    nombre = str_replace_all(nombre,
                             c("Á" = "A", "É" = "E",
                               "Í" = "I", "Ó" = "O",
                               "Ú" = "U")),
    
    nombre = str_remove_all(nombre, "DIPUTADOS"),
    nombre = str_remove_all(nombre, "DIPUTADAS"),
    nombre = str_remove_all(nombre, "DIPUTADO"),
    nombre = str_remove_all(nombre, "DIPUTADA"),
    nombre = str_remove_all(nombre, "INTEGRANTES DE LOS GRUPOS PARLAMENTARIOS DEL PVEM"),
    nombre = str_remove_all(nombre, "INTEGRANTES DEL GRUPO PARLAMENTARIO DEL"),
    nombre = str_remove_all(nombre, "SUSCRITO POR EL"),
    nombre = str_remove_all(nombre, ", PAN"),
    nombre = str_remove_all(nombre, ", PRI"),
    nombre = str_remove_all(nombre, ", PES"),
    nombre = str_remove_all(nombre, ", MC"),
    nombre = str_replace_all(nombre, "\\,", " Y"),
    nombre = str_replace_all(nombre, " E ", " Y "),
    nombre = str_remove_all(nombre, "PARTIDO"),
    nombre = str_remove_all(nombre, "SUSCRITA POR LA"),
    nombre = str_remove_all(nombre, "SUSCRITA POR"),
    nombre = str_remove_all(nombre, "DIPS."),
    nombre = str_remove_all(nombre, "SUSCRITA  POR LA"),
    nombre = str_remove_all(nombre, "SUSCRTIA POR LOS "),
    nombre = str_remove_all(nombre, "SUSCRITGA POR LOS "),
    nombre = str_remove_all(nombre, "SUSCRITAS POR LA "),
    nombre = str_remove_all(nombre, "SUSCRITA PO LA "),
    nombre = str_remove_all(nombre, "SUSCRITA  POR EL "),
    nombre = str_remove_all(nombre, "SUCRITA POR LA "),
    nombre = str_remove_all(nombre, "SUCRITA POR EL "),
    nombre = str_remove_all(nombre, "A NOMBRE DE LA "),
    nombre = str_remove_all(nombre, "DIP."),
    nombre = str_remove_all(nombre, "COORDINADORES")
  ) %>% 
  filter(filtro == 1) %>% 
  separate(
    col = nombre, 
    c("nombre_1", "nombre_2", "nombre_3", 
      "nombre_4", "nombre_5", "nombre_6",
      "nombre_7", "nombre_8", "nombre_9"), 
    sep = " Y ", remove = T
  ) %>% 
  filter(
    nchar(nombre_1) >3
  ) %>% 
  mutate(
    nombre_1 = str_trim(nombre_1, side = "both"),
    nombre_1 = ifelse(nombre_1 == "ARTURO ESCOBAR", 
                      "ARTURO ESCOBAR Y VEGA", nombre_1),
    nombre_1 = ifelse(nombre_1 == "MARIANA RODRIGUEZ MIER", 
                      "MARIANA RODRIGUEZ MIER Y TERAN", nombre_1),
    nombre_1 = ifelse(nombre_1 == "CLAUDIA VALERIA YAÑEZ CENTENO", 
                      "CLAUDIA VALERIA YAÑEZ CENTENO Y CABRERA", nombre_1),
    nombre_1 = ifelse(nombre_1 == "MIGUEL ALVA", "MIGUEL ALVA Y ALVA", nombre_1),
    nombre_1 = ifelse(nombre_1 == "EL  JAVIER ARIEL HIDALGO PONCE", 
                      "JAVIER ARIEL HIDALGO PONCE", nombre_1),
    nombre_1 = ifelse(str_detect(nombre_1, "GRUPO"), NA, nombre_1),
    
    nombre_2 = str_remove_all(nombre_2, "Y  EL  "),
    nombre_2 = str_remove_all(nombre_2, "Y SUSCRITO POR LA  "),
    nombre_2 = str_remove_all(nombre_2, "Y  LOS  "),
    nombre_2 = str_remove_all(nombre_2, "MOVIMIENTO CIUDADANO"),
    nombre_2 = str_remove_all(nombre_2, "Y "),
    nombre_2 = str_remove_all(nombre_2, "VEGA"),
    nombre_2 = str_remove_all(nombre_2, "TERAN"),
    nombre_2 = ifelse(nombre_2 == "ALVA", NA, nombre_2),
    nombre_2 = str_remove_all(nombre_2, "CABRERA"),
    nombre_2 = str_remove_all(nombre_2, "SUSCRITO POR LOS "),
    nombre_2 = str_trim(nombre_2, "both"),
    nombre_2 = ifelse(nombre_2 == "SP", NA, nombre_2),
    nombre_2 = ifelse(nombre_2 == "NA", NA, nombre_2),
    nombre_2 = ifelse(nchar(nombre_2)<=3, NA, nombre_2),
    nombre_2 = str_remove_all(nombre_2, "NUEVA ALIANZA"),
    
    nombre_1 = case_when(
      nombre_1 == "HECTOR JIMENEZ" ~ "HECTOR GUILLERMO DE JESUS JIMENEZ Y MENESES",
      nombre_1 == "HECTOR GUILLERMO DE JESUS JIMENEZ" ~ "HECTOR GUILLERMO DE JESUS JIMENEZ Y MENESES",
      T ~ nombre_1
    ),
    
    nombre_2 = ifelse(nombre_2 == "MENESES", NA, nombre_2),
    
    nombre_1 = str_remove_all(nombre_1, "LOS  "),
    nombre_1 = ifelse(str_detect(nombre_1, "INTEGRA"), NA, nombre_1),
    
    nombre_2 = ifelse(str_detect(nombre_2, "INTEGRA"), NA, nombre_2),
    nombre_2 = ifelse(str_detect(nombre_2, "NOMBRE"), NA, nombre_2),
    nombre_2 = str_remove_all(nombre_2, "EL  "),
    nombre_2 = ifelse(nombre_2 == "ARTURO ESCOBAR", "ARTURO ESCOBAR Y VEGA", nombre_2),
    
    nombre_3 = str_remove_all(nombre_3, "VEGA"),
    
    nombre_2 = ifelse(str_detect(nombre_2, "GRUPO"), NA, nombre_2),
    nombre_2 = ifelse(nombre_2 == "", NA, nombre_2),
    nombre_2 = ifelse(nombre_2 == ". ECONOMIA", NA, nombre_2),
    nombre_2 = str_remove_all(nombre_2, "DE LAS  "),
    nombre_2 = ifelse(str_detect(nombre_2, "LEGIS"), NA, nombre_2),
    nombre_2 = str_remove_all(nombre_2, "LOS  "),
    nombre_2 = case_when(
      nombre_2 == "S  LETICIA MARIANA GOMEZ ORDAZ" ~ "LETICIA MARIANA GOMEZ ORDAZ",
      nombre_2 == "S  MAIELLA MARTHA GABRIELA GOMEZ MALDONADO" ~ "MAIELLA MARTHA GABRIELA GOMEZ MALDONADO",
      nombre_2 == "S  MARIBEL MARTINEZ RUIZ" ~ "MARIBEL MARTINEZ RUIZ",
      nombre_2 == "E ISMAEL ALFREDO HERNANDEZ DERAS" ~ "ISMAEL ALFREDO HERNANDEZ DERAS",
      
      nombre_2 == "MARIANA RODRIGUEZ MIER" ~ 
        "MARIANA RODRIGUEZ MIER Y TERAN",
      
      T ~ nombre_2
    ),
    
    nombre_2 = ifelse(str_detect(nombre_2, "CONSERVACION"), NA, nombre_2),
    nombre_2 = ifelse(str_detect(nombre_2, "FEDERAL"), NA, nombre_2),
    
    nombre_3 = str_remove_all(nombre_3, "Y "),
    nombre_3 = str_trim(nombre_3, "both"),
    nombre_3 = str_remove_all(nombre_3, "MOVIMIENTO CIUDADANO"), 
    nombre_3 = str_remove_all(nombre_3, "TERAN"),
    nombre_3 = ifelse(str_detect(nombre_3, "INTEGRA"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "LEGIS"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "GRUPO"), NA, nombre_3),
    nombre_3 = ifelse(nombre_3 == "", NA, nombre_3),
    nombre_3 = ifelse(nchar(nombre_3)<=3, NA, nombre_3),
    
    nombre_1 = ifelse(nombre_1 == "CLAUDIA VALERIA YAÑEZ CENTENO", 
                      "CLAUDIA VALERIA YAÑEZ CENTENO CABRERA", nombre_1),
    
    nombre_2 = ifelse(nombre_2 == "CABRERA", NA, nombre_2),
    nombre_2 = ifelse(nombre_2 == "CLAUDIA VALERIA YAÑEZ CENTENO", 
                      "CLAUDIA VALERIA YAÑEZ CENTENO CABRERA", nombre_2),
    
    nombre_3 = ifelse(nombre_3 == "CABRERA", NA, nombre_3),
    nombre_3 = ifelse(nombre_3 == "DE", NA, nombre_3),
    nombre_3 = str_remove_all(nombre_3, "DE LA"),
    nombre_3 = str_remove_all(nombre_3, "DE LOS"),
    nombre_3 = ifelse(nombre_3 == "DEL", NA, nombre_3),
    nombre_3 = str_trim(nombre_3, "both"),
    nombre_3 = ifelse(str_detect(nombre_3, "CINEMAT"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "COMER"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "COORD"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "INDEP"), NA, nombre_3),
    nombre_3 = case_when(
      str_detect(nombre_3, "ARTURO ESCOBAR") ~ "ARTURO ESCOBAR Y VEGA",
      nombre_3 == "EL  MACEDONIO SALOMON TAMEZ GUAJARDO" ~ "MACEDONIO SALOMON TAMEZ GUAJARDO",
      nombre_3 == "EL  ULISES GARCIA SOTO" ~ "ULISES GARCIA SOTO",
      nombre_3 == "LA  JUANITA GUERRA MENA" ~ "JUANITA GUERRA MENA",
      nombre_3 == "LOS  ANTONIO ORTEGA MARTINEZ" ~ "ANTONIO ORTEGA MARTINEZ",
      nombre_3 == "LOS  ARMANDO LUNA CANALES" ~ "ARMANDO LUNA CANALES",
      nombre_3 == "DEL  ALFREDO BASURTO ROMAN" ~ "ALFREDO BASURTO ROMAN",
      nombre_3 == "DEL  JOSE ALFREDO FERREIRO VELAZCO" ~ "JOSE ALFREDO FERREIRO VELAZCO",
      nombre_3 == "DEL  JOSE MARIA ARROYO JUAREZ" ~ "JOSE MARIA ARROYO JUAREZ",
      nombre_3 == "DEL  VIRGILIO DANTE CABALLERO PEDRAZA" ~ "VIRGILIO DANTE CABALLERO PEDRAZA",
      
      T ~ nombre_3
    ),
    
    nombre_3 = ifelse(str_detect(nombre_3, "AGRICO"), NA, nombre_3),
    nombre_3 = ifelse(str_detect(nombre_3, "MUNICI"), NA, nombre_3),
    
    nombre_3 = ifelse(str_detect(nombre_3, "SEN"), NA, nombre_3),
    nombre_4 = str_trim(nombre_4, "both"),
    nombre_4 = ifelse(nombre_4 == "", NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "INTEGRA"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "GRUPO"), NA, nombre_4),
    
    nombre_4 = ifelse(str_detect(nombre_4, "VEGA"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "NA"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "Y"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "DE"), NA, nombre_4),
    
    nombre_4 = ifelse(nombre_4 == "EL  JOSE GUADALUPE AMBROCIO GACHUZ", 
                      "JOSE GUADALUPE AMBROCIO GACHUZ", nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "CINE"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "COMPET"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "RESPECT"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "SUSTEN"), NA, nombre_4),
    nombre_4 = ifelse(str_detect(nombre_4, "MOVIMIENTO"), NA, nombre_4),
    
    nombre_5 = str_trim(nombre_5, "both"),
    nombre_5 = ifelse(nombre_5 == "", NA, nombre_5),
    nombre_5 = ifelse(nombre_5 == "DE", NA, nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "GRUPO"), NA, nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "INTEGRA"), NA, nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "DE D"), NA, nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "NA"), NA, nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "Y"), NA, nombre_5),
    
    nombre_5 = ifelse(nombre_5 == "TERAN", 
                      "MARIANA RODRIGUEZ MIER Y TERAN", nombre_5),
    nombre_5 = ifelse(str_detect(nombre_5, "CLIMA"), NA, nombre_5),
    
    nombre_6 = str_trim(nombre_6, "both"),
    nombre_6 = ifelse(nombre_6 == "", NA, nombre_6),
    nombre_6 = ifelse(str_detect(nombre_6, "MOVIMI"), NA, nombre_6),
    nombre_6 = ifelse(str_detect(nombre_6, "RECURS"), NA, nombre_6),
    
    nombre_7 = str_trim(nombre_7, "both"),
    nombre_7 = ifelse(nombre_7 == "", NA, nombre_7),
    nombre_7 = ifelse(str_detect(nombre_7, "GRUPO"), NA, nombre_7),
    
    nombre_8 = str_trim(nombre_8, "both"),
    nombre_8 = ifelse(nombre_8 == "", NA, nombre_8),
    nombre_8 = str_remove_all(nombre_8, "POR LA  "),
    
    nombre_9 = ifelse(str_detect(nombre_9, "GARCIA"), nombre_9, NA),
    nombre_9 = str_trim(nombre_9, "both")
    
  ) %>% 
  unite(
    nombre_completo,
    c("nombre_1", "nombre_2", "nombre_3", 
      "nombre_4", "nombre_5", "nombre_6",
      "nombre_7", "nombre_8", "nombre_9"),
    sep = ";"
  ) %>% 
  separate(
    nombre_completo,
    c("nombre_1", "nombre_2", "nombre_3", 
      "nombre_4", "nombre_5", "nombre_6",
      "nombre_7", "nombre_8", "nombre_9"),
    sep = ";"
  )

inic <- inic %>% 
  mutate(
    nombre_1 = ifelse(nombre_1 == "NA", NA, nombre_1),
    nombre_2 = ifelse(nombre_2 == "NA", NA, nombre_2),
    nombre_3 = ifelse(nombre_3 == "NA", NA, nombre_3),
    nombre_4 = ifelse(nombre_4 == "NA", NA, nombre_4),
    nombre_5 = ifelse(nombre_5 == "NA", NA, nombre_5),
    nombre_6 = ifelse(nombre_6 == "NA", NA, nombre_6),
    nombre_7 = ifelse(nombre_7 == "NA", NA, nombre_7),
    nombre_8 = ifelse(nombre_8 == "NA", NA, nombre_8),
    nombre_9 = ifelse(nombre_9 == "NA", NA, nombre_9)
  ) %>% 
  pivot_longer(
    cols = c("nombre_1", "nombre_2", "nombre_3", 
             "nombre_4", "nombre_5", "nombre_6",
             "nombre_7", "nombre_8", "nombre_9")
  ) %>% 
  mutate(
    value = str_trim(value, "both"),
    value = ifelse(str_detect(value, "LEGISLA"), NA, value),
    value = ifelse(str_detect(value, "MOVIMIENTO"), NA, value),
    value = ifelse(str_detect(value, "POBLACION"), NA, value),
    value = ifelse(str_detect(value, "SEN"), NA, value),
    value = ifelse(str_detect(value, "SIN"), NA, value),
    value = ifelse(value == "SP", NA, value),
    value = ifelse(value == "SUSCRITA", NA, value),
    value = ifelse(value == "MARIANA RODRIGUEZ MIER", "MARIANA RODRIGUEZ MIER Y TERAN", value),
    value = ifelse(value == "TERAN", NA, value),
    
    value = case_when(
      value == "ALFREDO MIGUEL HERRERA" ~ "ALFREDO MIGUEL HERRERA DERAS", 
      value == "CARMEN JULIA PRUDENCIA GONZALEZ" ~ "CARMEN JULIA PRUDENCIO GONZALEZ", 
      value == "CARMINA YADIRA RESGALADO MARDUEÑO" ~ "CARMINA YADIRA REGALADO MARDUEÑO", 
      value == "CESAR AGUSTIN HERNANDEZ PERERZ" ~ "CESAR AGUSTIN HERNANDEZ PEREZ", 
      value == "CESAR CAMACHO" ~ "CESAR CAMACHO QUIROZ", 
      value == "CLAUDIA LOPEZ RAMON" ~ "CLAUDIA LOPEZ RAYON", 
      value == "CLAUDIA VALERIA YAÑEZ CENTENO CABRERA" ~ 
        "CLAUDIA VALERIA YAÑEZ CENTENO Y CABRERA", 
      value == "CYNTHIA ILIANA LOPEZ CASTO" ~ "CYNTHIA ILIANA LOPEZ CASTRO", 
      value == "CYNTHIA LOPEZ CASTRO" ~ "CYNTHIA ILIANA LOPEZ CASTRO", 
      value == "DALIA MARIA ROCHA LADRON" ~ "DALIA MARIA ROCHA LADRON DE GUEVARA", 
      value == "DANIELLA JUDITH HERNANDEZ FLORES" ~ "DANIELA JUDITH HERNANDEZ FLORES", 
      value == "DAVID BAUTIS RIVERA" ~ "DAVID BAUTISTA RIVERA", 
      value == "DIEGO DEL BOSQUE VILLARREAL" ~ "DIEGO EDUARDO DEL BOSQUE VILLARREAL",
      value == "DIEGO EDUARDO DEL BOPSQUE VILLARREAL" ~ "DIEGO EDUARDO DEL BOSQUE VILLARREAL",
      value == "DIEGO EDUARDO DEL BOSQUE VILLAREAL" ~ "DIEGO EDUARDO DEL BOSQUE VILLARREAL",
      value == "DULCE MARIA MENDEZ DE LA LUZ DUAZON" ~ "DULCE MARIA MENDEZ DE LA LUZ DAUZON",
      value == "EDELMIRO SANTIAGO DIAZ" ~ "EDELMIRO SANTIAGO SANTOS DIAZ",
      value == "ELBA LORENA TORRES DIAS" ~ "ELBA LORENA TORRES DIAZ",  
      value == "ELBA LORENA TORREZ DIAZ" ~ "ELBA LORENA TORRES DIAZ",
      value == "ERNESTINA GODORAMOS" ~ "ERNESTINA GODOY RAMOS",
      value == "ESMERALDA DE LOS ANGELES  MEDINA" ~ "ESMERALDA DE LOS ANGELES MORENO MEDINA",
      value == "ESMERALDA DE LOS ANGELES MORENO MEDIAN" ~ "ESMERALDA DE LOS ANGELES MORENO MEDINA",
      value == "FLOR ESTELA RENDERIA MEDINA" ~ "FLOR ESTELA RENTERIA MEDINA",
      value == "FRINNE AZUARA YAZABAL" ~ "FRINNE AZUARA YARZABAL",
      value == "HECTOR RENE CRUZ APARARICIO" ~ "HECTOR RENE CRUZ APARICIO",
      value == "HORTENSIA MARIA LUIS NOROÑA QUEZADA" ~ "HORTENSIA MARIA LUISA NOROÑA QUEZADA",
      value == "HORTENSIA MARIA LUISA NOROÑA" ~ "HORTENSIA MARIA LUISA NOROÑA QUEZADA",
      value == "IRASEMA BUENFIL DIAZ" ~ "IRASEMA DEL CARMEN BUENFIL DIAZ",
      value == "ISAIAS GONZALES CUEVAS" ~ "ISAIAS GONZALEZ CUEVA",
      value == "ITZCOATL TONATIUTH BRAVO PADILLA" ~ "ITZCOATL TONATIUH BRAVO PADILLA",
      value == "JANET MELANIE MURILLO CAVEZ" ~ "JANET MELANIE MURILLO CHAVEZ",
      value == "JANNET TELLEZ INFRANTE" ~ "JANNET TELLEZ INFANTE",
      value == "JARTURO ESCOBAR" ~ "ARTURO ESCOBAR Y VEGA",
      value == "JAVIER JULIAN CASTAÑEDA PROMPOSO" ~ "JAVIER JULIAN CASTAÑEDA POMPOSO",
      value == "JESUS FERNANDO GARCIA" ~ "JESUS FERNANDO GARCIA HERNANDEZ",
      value == "JORGE ALBICIADES GARCIA LARA" ~ "JORGE ALCIBIADES GARCIA LARA",
      value == "JORGE ARTURO AGÜELLES VICTORERO" ~ "JORGE ARTURO ARGÜELLES VICTORERO",
      value == "JORGE ARTURO ESPADA GALVAN" ~ "JORGE ARTURO ESPADAS GALVAN",
      value == "JOSE ALBERTO COUTOLENC BUENTELLO" ~ "JOSE ALBERTO COUTTOLENC BUENTELLO",
      value == "JOSEFINA GONALEZ LUNA" ~ "JOSEFINA GONZALEZ LUNA",
      value == "JUAN ALBERTO BLANCO SALDIVAR" ~ "JUAN ALBERTO BLANCO ZALDIVAR",
      value == "JUAN CARLOS VILLAREAL SALAZAR" ~ "JUAN CARLOS VILLARREAL SALAZAR",
      value == "JUAN ERNIQUE FARRERA ESPONDA" ~ "JUAN ENRIQUE FARRERA ESPONDA",
      value == "JUAN ROMERO TERNORIO" ~ "JUAN ROMERO TENORIO",
      value == "JULIETA MACIAS RABAGO YMOVIMIENTO CIUDADANO" ~ "JULIETA MACIAS RABAGO",
      value == "KARINA  PADILLA AVILA" ~ "KARINA PADILLA AVILA",
      value == "JUAN ROMERO TERNORIO" ~ "JUAN ROMERO TENORIO",
      value == "ULIETA MACIAS RABAGO YMOVIMIENTO CIUDADANO" ~ "ULIETA MACIAS RABAGO",
      value == "KARINA  PADILLA AVILA" ~ "KARINA PADILLA AVILA",
      value == "KATIA ALEJANDRAR CASTILLO LOZANO" ~ "KATIA ALEJANDRA CASTILLO LOZANO",
      value == "LAURA BARRERA FOTOUL" ~ "LAURA BARRERA FORTOUL",
      value == "LAURA IMELDA PEREZ" ~ "LAURA IMELDA PEREZ SEGURA",
      value == "LETRICIA MARIANA GOMEZ ORDAS" ~ "LETICIA MARIANA GOMEZ ORDAZ",
      value == "LIA LIMAON GARCIA" ~ "LIA LIMON GARCIA",
      value == "LORENA DEL SOCORRO JIMENEZ ANDRANE" ~ "LORENA DEL SOCORRO JIMENEZ ANDRADE",
      value == "MA. EUGENIA LETICIA ESPINOZA RIVAS" ~ "MA. EUGENIA LETICIA ESPINOSA RIVAS",
      value == "MARCO ANTONIO ADAME CASTRILLO" ~  "MARCO ANTONIO ADAME CASTILLO",
      value == "MARCO ANTONIO GOMEZ ALCATAR" ~ "MARCO ANTONIO GOMEZ ALCANTAR",
      value == "MARIA DE LOS DOLRES PADIERNA LUNA" ~ "MARIA DE LOS DOLORES PADIERNA LUNA",
      value == "MARIA DE LOURDES MONTES" ~ "MARIA DE LOURDES MONTES HERNANDEZ",
      value == "MARIA ESTHER ALONZO MORALES" ~  "MARIA ESTER ALONZO MORALES",
      value == "MARIA LIBIER GOZALEZ ANAYA" ~ "MARIA LIBIER GONZALEZ ANAYA",
      value == "MARIA LUIS BELTRAN REYES" ~ "MARIA LUISA BELTRAN REYES",
      value == "MARIA ROSETE" ~  "MARIA ROSETE SANCHEZ",
      value == "MARIA TERESA LOPEZ" ~ "MARIA TERESA LOPEZ PEREZ",
      value == "MARIA WENDBRICEÑO ZULOAGA" ~  "MARIA WENDY BRICEÑO ZULOAGA",
      value == "MARIANA RODRIGUEZ MIER" ~ "MARIANA RODRIGUEZ MIER Y TERAN",
      value == "MARICRUZ ROBLERO GORDILLO" ~ "MARICRUZ ROBLEDO GORDILLO",
      value == "MARTHA HORTENCIA GARACADENA" ~  "MARTHA HORTENCIA GARAY CADENA",
      value == "MARTHA HORTENSIA GARAY CADENA" ~ "MARTHA HORTENCIA GARAY CADENA",
      value == "MAXIMINO ALEJANDRO CANDARIA" ~ "MAXIMINO ALEJANDRO CANDELARIA",
      value == "MIGUEL ANGEL CHICHO HERRERA" ~ "MIGUEL ANGEL CHICO HERRERA",
      value == "NANCLOPEZ RUIZ" ~ "NANCY LOPEZ RUIZ",
      value == "NANCY CLAUDIA RESENDIZ" ~ "NANCY CLAUDIA RESENDIZ HERNANDEZ",
      value == "PRESENTADA POR LA  RUTH NOEMI TISCAREÑO AGOITIA" ~ "RUTH NOEMI TISCAREÑO AGOITIA",
      value == "RAUL GRACIA GUZMAN ACUERDO" ~ "RAUL GRACIA GUZMAN",
      value == "RAUL GARCIA GUZMAN" ~ "RAUL GRACIA GUZMAN",
      value == "REFUGIO TRINIDAD GAZON CANCHOLA" ~ "REFUGIO TRINIDAD GARZON CANCHOLA",
      value == "ROCIO DEL PILAR VILLARAUZ" ~  "ROCIO DEL PILAR VILLARAUZ MARTINEZ",
      value == "S  MARIA GUADALUPE ROMAN AVILA" ~ "MARIA GUADALUPE ROMAN AVILA",
      value == "SUSANA BEATRIZ CUAXIOLA SERRANO" ~ "SUSANA BEATRIZ CUAXILOA SERRANO",
      value == "VIRGILIO DANTE CABALLERO" ~ "VIRGILIO DANTE CABALLERO PEDRAZA",
      value == "XOCHITL NASHIELLZAGAL RAMIREZ" ~ "XOCHITL NASHIELLY ZAGAL RAMIREZ",
      value == "ZULMA ESPINOZA MAYA" ~ "ZULMA ESPINOZA MATA",
      
      T ~ value
    ),
    
    titular = ifelse(name == "nombre_1", 1, 0)
  ) %>% 
  filter(!is.na(value)) %>% 
  rename(nombre_completo = value) %>% 
  select(-c(name, dummy)) %>% 
  mutate(
    fecha_de_inicio_del_periodo_que_se_informa1 = dmy(fecha_de_inicio_del_periodo_que_se_informa),
    fecha_de_inicio_del_periodo_que_se_informa2 = as.Date(as.numeric(fecha_de_inicio_del_periodo_que_se_informa), 
                                                          origin = "1899-12-30"),
    fecha_de_inicio_del_periodo_que_se_informa = paste(fecha_de_inicio_del_periodo_que_se_informa1,
                                                        fecha_de_inicio_del_periodo_que_se_informa2, 
                                                        sep = ";"),
    fecha_de_inicio_del_periodo_que_se_informa = str_remove_all(fecha_de_inicio_del_periodo_que_se_informa, ";"),
    fecha_de_inicio_del_periodo_que_se_informa = str_remove_all(fecha_de_inicio_del_periodo_que_se_informa, "NA"),
    
    
    fecha_de_termino_del_periodo_que_se_informa1 = dmy(fecha_de_termino_del_periodo_que_se_informa),
    fecha_de_termino_del_periodo_que_se_informa2 = as.Date(as.numeric(fecha_de_termino_del_periodo_que_se_informa), 
                                                          origin = "1899-12-30"),
    fecha_de_termino_del_periodo_que_se_informa = paste(fecha_de_termino_del_periodo_que_se_informa1,
                                                       fecha_de_termino_del_periodo_que_se_informa2, 
                                                       sep = ";"),
    fecha_de_termino_del_periodo_que_se_informa = str_remove_all(fecha_de_termino_del_periodo_que_se_informa, ";"),
    fecha_de_termino_del_periodo_que_se_informa = str_remove_all(fecha_de_termino_del_periodo_que_se_informa, "NA"),
    
    
    fecha_de_inicio_del_periodo_de_sesiones1 = dmy(fecha_de_inicio_del_periodo_de_sesiones),
    fecha_de_inicio_del_periodo_de_sesiones2 = as.Date(as.numeric(fecha_de_inicio_del_periodo_de_sesiones), 
                                                          origin = "1899-12-30"),
    fecha_de_inicio_del_periodo_de_sesiones = paste(fecha_de_inicio_del_periodo_de_sesiones1,
                                                       fecha_de_inicio_del_periodo_de_sesiones2, 
                                                       sep = ";"),
    fecha_de_inicio_del_periodo_de_sesiones = str_remove_all(fecha_de_inicio_del_periodo_de_sesiones, ";"),
    fecha_de_inicio_del_periodo_de_sesiones = str_remove_all(fecha_de_inicio_del_periodo_de_sesiones, "NA"),
    
    
    fecha_de_termino_del_periodo_de_sesiones1 = dmy(fecha_de_termino_del_periodo_de_sesiones),
    fecha_de_termino_del_periodo_de_sesiones2 = as.Date(as.numeric(fecha_de_termino_del_periodo_de_sesiones), 
                                                          origin = "1899-12-30"),
    fecha_de_termino_del_periodo_de_sesiones = paste(fecha_de_termino_del_periodo_de_sesiones1,
                                                       fecha_de_termino_del_periodo_de_sesiones2, 
                                                       sep = ";"),
    fecha_de_termino_del_periodo_de_sesiones = str_remove_all(fecha_de_termino_del_periodo_de_sesiones, ";"),
    fecha_de_termino_del_periodo_de_sesiones = str_remove_all(fecha_de_termino_del_periodo_de_sesiones, "NA"),
    
    
    fecha_del_dictamen1 = dmy(fecha_del_dictamen),
    fecha_del_dictamen2 = as.Date(as.numeric(fecha_del_dictamen), 
                                                          origin = "1899-12-30"),
    fecha_del_dictamen = paste(fecha_del_dictamen1,
                                                       fecha_del_dictamen2, 
                                                       sep = ";"),
    fecha_del_dictamen = str_remove_all(fecha_del_dictamen, ";"),
    fecha_del_dictamen = str_remove_all(fecha_del_dictamen, "NA"),
    
    
    fecha_de_validacion1 = dmy(fecha_de_validacion),
    fecha_de_validacion2 = as.Date(as.numeric(fecha_de_validacion), 
                                                          origin = "1899-12-30"),
    fecha_de_validacion = paste(fecha_de_validacion1,
                                                       fecha_de_validacion2, 
                                                       sep = ";"),
    fecha_de_validacion = str_remove_all(fecha_de_validacion, ";"),
    fecha_de_validacion = str_remove_all(fecha_de_validacion, "NA"),
    
    
    fecha_de_actualizacion1 = dmy(fecha_de_actualizacion),
    fecha_de_actualizacion2 = as.Date(as.numeric(fecha_de_actualizacion), 
                                                          origin = "1899-12-30"),
    fecha_de_actualizacion = paste(fecha_de_actualizacion1,
                                                       fecha_de_actualizacion2, 
                                                       sep = ";"),
    fecha_de_actualizacion = str_remove_all(fecha_de_actualizacion, ";"),
    fecha_de_actualizacion = str_remove_all(fecha_de_actualizacion, "NA")
  ) %>% 
  select(-c("fecha_de_inicio_del_periodo_que_se_informa1",
            "fecha_de_inicio_del_periodo_que_se_informa2",
            "fecha_de_termino_del_periodo_que_se_informa1",
            "fecha_de_termino_del_periodo_que_se_informa2",
            "fecha_de_inicio_del_periodo_de_sesiones1",
            "fecha_de_inicio_del_periodo_de_sesiones2",
            "fecha_de_termino_del_periodo_de_sesiones1",
            "fecha_de_termino_del_periodo_de_sesiones2",
            "fecha_del_dictamen1",
            "fecha_del_dictamen2",
            "fecha_de_validacion1",
            "fecha_de_validacion2",
            "fecha_de_actualizacion1",
            "fecha_de_actualizacion2"))

write.csv(inic, paste(out, "iniciativas.csv", sep = "/"), row.names = F)