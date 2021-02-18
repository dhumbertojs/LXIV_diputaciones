library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)

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

iniciativas <- bind_rows(fa, fb, sec)

write.csv(iniciativas, paste(out, "iniciativas.csv", sep = "/"), row.names = F)