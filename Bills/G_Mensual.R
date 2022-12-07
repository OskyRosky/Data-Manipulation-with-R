########################################################################################
#                                                                                      #
#                                                                                      #
#       Extracción + transformación + creacción de tablas (datamart)  de los Gastos    #
#                                                                                      #
#                                                                                      #
########################################################################################

start.time <- Sys.time()

###############
#   Mensual   #
###############

#############################
#   Estructura del código   #  
#############################

# 0. Establecimiento de la configuración general del espacio de trabajo.
#
# 1. Importación de los archivos y otros referentes a los gastos
#
# 2. Ciertas modificaciones previar generales.
#
# 3. Unión + Transformación de ingresos mensuales + anuales
#
# 4. Creación de las principales tablas.
#
# 5. Exportación de las tablas con información.


#####
# 0 #  
##################################################################################################
##################################################################################################
#                         Establecimiento de la  configuración general                           #
##################################################################################################
##################################################################################################

######################
# Opciones generales #
###################### 

options(encoding="utf-8")
options(scipen=999)

################
#  Directorio  #
################ 

setwd("C:/Users/oscar/Desktop/Gastos - 2021/Insumos/data")

###############
#  Librerías  #
###############

suppressMessages(if(!require(readxl)){ install.packages("readxl")}) 
suppressMessages(if(!require(dplyr)){ install.packages("dplyr")})
suppressMessages(if(!require(DT)){ install.packages("DT")})
suppressMessages(if(!require(plyr)){ install.packages("plyr")})
suppressMessages(if(!require(readr)){ install.packages("readr")})
suppressMessages(if(!require(tidyr)){ install.packages("tidyr")})
suppressMessages(if(!require(stringr)){ install.packages("stringr")}) 
suppressMessages(if(!require(openxlsx)){ install.packages("openxlsx")}) 


suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(openxlsx))



#####################################################
#####################################################
#         Parámetros: datos + dashboard             #
#####################################################
#####################################################

#############
#  Millones #
#############

Millones <- 1000000

#####################################################
#####################################################
#         Parámetros: datos + dashboard             #
#####################################################
#####################################################

#############
#  Millones #
#############

Millones <- 1000000

################################
#  Delimitación años análisis  #
################################

Anos_analisis <- 2007

####################
#  Años referencia #
####################

Ano_actual    <- as.numeric(substr(Sys.Date(),1,4))
Ano_pasado_1  <- as.numeric(substr(Sys.Date(),1,4))-1  
Ano_pasado_2  <- as.numeric(substr(Sys.Date(),1,4))-2

####################
#  Mes referencia  #
####################

Mes_actual <- substr(Sys.Date(),6,7)

####################
#  Mes referencia  #
####################

Mes_actual <- substr(Sys.Date(),6,7)


#####
# 1 #  
##################################################################################################
##################################################################################################
##################################################################################################
#                              Importación de los gastos acumulados                              #
##################################################################################################
##################################################################################################
##################################################################################################


############################
#  2007 - 30 Abril 2021    #
############################


gastos_m_2007_Abril_2021 <- suppressWarnings(readxl::read_excel("Total_Egresos_mensuales_30abril2021.xlsx"))
#names(gastos_m_2007_Abril_2021)


############################
#       Actualizados       #
############################


sigaf_gasto_actual_mensual <- suppressWarnings(readxl::read_excel("Mensual_SIGAF_Gastos_actual.xlsx"))
names(sigaf_gasto_actual_mensual)

###########################################
#       Clasificador del destinatario     #
###########################################

Destinatario_2021_mensual <- suppressWarnings(readxl::read_excel("Destinatario_2021_mensual.xlsx"))
names(Destinatario_2021_mensual)


Destinatario_2021_mensual <- Destinatario_2021_mensual %>% distinct() 

# Destinatario_2021_mensual  %>% group_by(Concatdestin)   %>% 
#                                dplyr::summarise('Conteo' = n())



####################
#  Transformación  # 
####################

# Actualizada #

# Renombrar #


sigaf_gasto_actual_mensual  <- sigaf_gasto_actual_mensual %>% 
                                              dplyr::rename(
                                                TIPO = Concepto,
                                                Año = Ejercicio,
                                                `Centro Gestor`  = `Centro Gestor`, 
                                                `POS PRE` = `Posicion presupuestaria`,
                                                `Fuente cod`  =  `Fondos`,
                                                `CE_cod` = `Clasif.Economica`,
                                                `CF_cod` = `Clasif.Funcional`,
                                                `IP` = `Identificador Partida`,
                                                Partida_cod  = `Partida`,
                                                `Subpartida cod`  = `Grupo Obj`,
                                                ObjetoGasto_cod  = `Obj_Gasto`,
                                                `Enero` = `M01`,
                                                `Febrero` = `M02`, 
                                                `Marzo` = `M03`,
                                                `Abril` = `M04`,
                                                `Mayo` = `M05`,
                                                `Junio` = `M06`,
                                                `Julio` = `M07`,
                                                `Agosto` = `M08`,
                                                `Septiembre` = `M09`,
                                                `Octubre` = `M10`,
                                                `Noviembre` = `M11`,
                                                `Diciembre` = `M12`,
                                                `Total` = `TOTAL`
                                               )

# Modificación de la variable IP

sigaf_gasto_actual_mensual <- mutate(sigaf_gasto_actual_mensual,
                                            IP = if_else(is.na(IP),"000",IP)                            
                                      )


# Creación de la variable Concatdestin

sigaf_gasto_actual_mensual <- mutate(sigaf_gasto_actual_mensual,
                                                 Concatdestin = paste(`Centro Gestor`,ObjetoGasto_cod, `Fuente cod`,`CE_cod`,`CF_cod`,`IP`,sep ="")
                                       )


# Creación de las otras variables #

sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,

                   ##############################################################
                   # Creación de las variables: Título, programa y subprobrama  #
                   ##############################################################
                   
                   `Tit_cod` = substr(sigaf_gasto_actual_mensual$`Centro Gestor`, 1, 3),
                   `Título`  = case_when(
                     `Tit_cod` == "101" ~ "Asamblea Legislativa",
                     `Tit_cod` == "102" ~ "Contraloría General de la República",
                     `Tit_cod` == "103" ~ "Defensoría de los Habitantes de la República",
                     `Tit_cod` == "201" ~ "Presidencia de la República",
                     `Tit_cod` == "202" ~ "Ministerio de la Presidencia",
                     `Tit_cod` == "203" ~ "Ministerio de Gobernación y Policía",
                     `Tit_cod` == "204" ~ "Ministerio de Relaciones Exteriores y Culto",
                     `Tit_cod` == "205" ~ "Ministerio de Seguridad Pública",
                     `Tit_cod` == "206" ~ "Ministerio de Hacienda",
                     `Tit_cod` == "207" ~ "Ministerio de Agricultura y Ganadería",
                     `Tit_cod` == "208" ~ "Ministerio de Economía, Industria y Comercio",
                     `Tit_cod` == "209" ~ "Ministerio de Obras Públicas y Transportes",
                     `Tit_cod` == "210" ~ "Ministerio de Educación Pública",
                     `Tit_cod` == "211" ~ "Ministerio de Salud",
                     `Tit_cod` == "212" ~ "Ministerio de Trabajo y Seguridad Social",
                     `Tit_cod` == "213" ~ "Ministerio de Cultura y Juventud",
                     `Tit_cod` == "214" ~ "Ministerio de Justicia y Paz",
                     `Tit_cod` == "215" ~ "Ministerio de Vivienda y Asentamientos Humanos",
                     `Tit_cod` == "216" ~ "Ministerio de Comercio Exterior",
                     `Tit_cod` == "217" ~ "Ministerio de Planificación Nacional y Política Económica",
                     `Tit_cod` == "218" ~ "Ministerio de Ciencia y Tecnología",
                     `Tit_cod` == "219" ~ "Ministerio del Ambiente y Energía",
                     `Tit_cod` == "230" ~ "Servicio de la Deuda Pública",
                     `Tit_cod` == "231" ~ "Regímenes de Pensiones",
                     `Tit_cod` == "232" ~ "Partidas Específicas",
                     `Tit_cod` == "301" ~ "Poder Judicial",
                     `Tit_cod` == "401" ~ "Tribunal Supremo de Elecciones"
                   ),
                   `Programa cod` = substr(sigaf_gasto_actual_mensual$`Centro Gestor`, 4, 6),
                   `Prog des` = case_when(
                     `Tit_cod` == "101" & `Programa cod` == "002" ~ "Asamblea Legislativa",
                     `Tit_cod` == "102" & `Programa cod` == "009" ~ "Dirección Superior y Apoyo Administrativo",
                     `Tit_cod` == "102" & `Programa cod` == "012" ~ "Fiscalización Superior de la Hacienda Pública",
                     `Tit_cod` == "103" & `Programa cod` == "806" ~ "Donación PRODERE",
                     `Tit_cod` == "103" & `Programa cod` == "807" ~ "Defensoría de los Habitantes de la República",
                     `Tit_cod` == "103" & `Programa cod` == "808" ~ "Defensoría de los Habitantes",
                     `Tit_cod` == "201" & `Programa cod` == "021" ~ "Administración Superior",
                     `Tit_cod` == "201" & `Programa cod` == "024" ~ "Administración de Recursos Humanos",
                     `Tit_cod` == "201" & `Programa cod` == "027" ~ "Información y comunicación",
                     `Tit_cod` == "202" & `Programa cod` == "034" ~ "Administración Superior",
                     `Tit_cod` == "202" & `Programa cod` == "035" ~ "Delegados Presidenciales",
                     `Tit_cod` == "202" & `Programa cod` == "041" ~ "Dirección de Inteligencia y Seguridad Nacional",
                     `Tit_cod` == "202" & `Programa cod` == "042" ~ "Unidad Especial de Intervención",
                     `Tit_cod` == "203" & `Programa cod` == "044" ~ "Actividad Central",
                     `Tit_cod` == "203" & `Programa cod` == "048" ~ "Tribunal Administrativo Migratorio",
                     `Tit_cod` == "203" & `Programa cod` == "049" ~ "Desarrollo de la Comunidad",
                     `Tit_cod` == "203" & `Programa cod` == "051" ~ "Programación Publicitaria",
                     `Tit_cod` == "203" & `Programa cod` == "054" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "204" & `Programa cod` == "079" ~ "Actividad Central",
                     `Tit_cod` == "204" & `Programa cod` == "081" ~ "Servicio Exterior",
                     `Tit_cod` == "204" & `Programa cod` == "082" ~ "Política Exterior",
                     `Tit_cod` == "204" & `Programa cod` == "083" ~ "Cooperación Internacional",
                     `Tit_cod` == "204" & `Programa cod` == "084" ~ "Dirección General de Protocolo y Ceremonial del Estado",
                     `Tit_cod` == "204" & `Programa cod` == "085" ~ "Promoción Externa",
                     `Tit_cod` == "204" & `Programa cod` == "088" ~ "Cuotas a Organismos Internacionales",
                     `Tit_cod` == "205" & `Programa cod` == "089" ~ "Gestión Administrativa de los Cuerpos Policiales",
                     `Tit_cod` == "205" & `Programa cod` == "090" ~ "Gestión Operativa de los Cuerpos Policiales",
                     `Tit_cod` == "205" & `Programa cod` == "091" ~ "Actividades Centrales",
                     `Tit_cod` == "205" & `Programa cod` == "092" ~ "Actividades comunes al servicio de seguridad ciudadana, servicio de seguridad fronteriza, servicio de seguridad aérea, y servicios de investigación y represión del narcotráfico",
                     `Tit_cod` == "205" & `Programa cod` == "093" ~ "Servicio de Seguridad Ciudadana",
                     `Tit_cod` == "205" & `Programa cod` == "094" ~ "Servicio de Seguridad Fronteriza",
                     `Tit_cod` == "205" & `Programa cod` == "095" ~ "Servicio de Seguridad Aérea",
                     `Tit_cod` == "205" & `Programa cod` == "096" ~ "Servicio de Seguridad Marítima",
                     `Tit_cod` == "205" & `Programa cod` == "097" ~ "Servicios de Investigación y Represión al Narcotráfico ",
                     `Tit_cod` == "205" & `Programa cod` == "098" ~ "Policía Antidrogas",
                     `Tit_cod` == "205" & `Programa cod` == "107" ~ "Seguridad Ciudadana",
                     `Tit_cod` == "205" & `Programa cod` == "109" ~ "Vigilancia Áerea ||||| Servicio Nacional de Guardacostas",
                     `Tit_cod` == "206" & `Programa cod` == "131" ~ "Unidad Ejecutora del Catastro y Registro",
                     `Tit_cod` == "206" & `Programa cod` == "132" ~ "Administración Superior",
                     `Tit_cod` == "206" & `Programa cod` == "134" ~ "Administración de Ingresos",
                     `Tit_cod` == "206" & `Programa cod` == "135" ~ "Tribunales Fiscal y Aduanero",
                     `Tit_cod` == "206" & `Programa cod` == "136" ~ "Administración Financiera",
                     `Tit_cod` == "206" & `Programa cod` == "138" ~ "Servicios Hacendarios",
                     `Tit_cod` == "207" & `Programa cod` == "169" ~ "Actividades Centrales",
                     `Tit_cod` == "207" & `Programa cod` == "170" ~ "Secretaria de Planificación del Sector Agropecuario",
                     `Tit_cod` == "207" & `Programa cod` == "171" ~ "Servicio Fitosanitario",
                     `Tit_cod` == "207" & `Programa cod` == "172" ~ "Instituto Nacional de Innovación Tecnológica Agropecuaria",
                     `Tit_cod` == "207" & `Programa cod` == "173" ~ "Salud Animal ",
                     `Tit_cod` == "207" & `Programa cod` == "175" ~ "Dirección Superior de Operaciones Regionales y Extensión Agropecuaria",
                     `Tit_cod` == "207" & `Programa cod` == "176" ~ "Desarrollo Rural",
                     `Tit_cod` == "207" & `Programa cod` == "177" ~ "Apoyo a instituciones y organizaciones del sector agropecuario",
                     `Tit_cod` == "207" & `Programa cod` == "180" ~ "Proyecto Desarrollo Agrícola de la Península de Nicoya FIDA-BCIE",
                     `Tit_cod` == "207" & `Programa cod` == "182" ~ "Fomento de la Producción Agropecuaria Sostenible",
                     `Tit_cod` == "207" & `Programa cod` == "185" ~ "Desarrollo Sostenible Cuenca Binacional Río Sixaola",
                     `Tit_cod` == "208" & `Programa cod` == "215" ~ "Actividades Centrales",
                     `Tit_cod` == "208" & `Programa cod` == "217" ~ "Mejora Regulatoria",
                     `Tit_cod` == "208" & `Programa cod` == "218" ~ "Gestión de Reglamentación Técnica",
                     `Tit_cod` == "208" & `Programa cod` == "219" ~ "Dirección General Pequeña y Mediana Empresa",
                     `Tit_cod` == "208" & `Programa cod` == "220" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "208" & `Programa cod` == "223" ~ "Protección del Consumidor",
                     `Tit_cod` == "208" & `Programa cod` == "224" ~ "Promoción de la Competencia",
                     `Tit_cod` == "208" & `Programa cod` == "229" ~ "Dirección de Estudios Económicos",
                     `Tit_cod` == "209" & `Programa cod` == "326" ~ "Administración Superior",
                     `Tit_cod` == "209" & `Programa cod` == "327" ~ "Mejoramiento y Conservación de la Red Vial",
                     `Tit_cod` == "209" & `Programa cod` == "328" ~ "Puertos y Regulación Marítima",
                     `Tit_cod` == "209" & `Programa cod` == "329" ~ "Edificaciones Nacionales",
                     `Tit_cod` == "209" & `Programa cod` == "330" ~ "Instituto Geográfico Nacional",
                     `Tit_cod` == "209" & `Programa cod` == "331" ~ "Transporte Terrestre",
                     `Tit_cod` == "209" & `Programa cod` == "332" ~ "Proyectos y Transferencias Sectoriales",
                     `Tit_cod` == "209" & `Programa cod` == "333" ~ "Conservación Vial Participativa de la Red Rural",
                     `Tit_cod` == "209" & `Programa cod` == "334" ~ "Complejo Vial Costanera Sur- BCIE-1605",
                     `Tit_cod` == "209" & `Programa cod` == "386" ~ "Construcción Vías Contrapartida BIRF 2764-CR",
                     `Tit_cod` == "209" & `Programa cod` == "393" ~ "Construcción Vías Contrato BIRF 2764-CR",
                     `Tit_cod` == "210" & `Programa cod` == "501" ~ "Administración Central",
                     `Tit_cod` == "210" & `Programa cod` == "550" ~ "Definición y Planificación de la Política Educativa",
                     `Tit_cod` == "210" & `Programa cod` == "551" ~ "Servicios de Apoyo a la Gestión",
                     `Tit_cod` == "210" & `Programa cod` == "552" ~ "Capacitación y desarrollo profesional",
                     `Tit_cod` == "210" & `Programa cod` == "553" ~ "Desarrollo curricular y vinculo al trabajo",
                     `Tit_cod` == "210" & `Programa cod` == "554" ~ "Infraestructura y equipamiento del sistema educativo",
                     `Tit_cod` == "210" & `Programa cod` == "555" ~ "Aplicación de la Tecnología a la Educación",
                     `Tit_cod` == "210" & `Programa cod` == "556" ~ "Gestión y Evaluación de la Calidad",
                     `Tit_cod` == "210" & `Programa cod` == "557" ~ "Desarrollo y Coordinación Regional",
                     `Tit_cod` == "210" & `Programa cod` == "558" ~ "Programas de Equidad",
                     `Tit_cod` == "210" & `Programa cod` == "570" ~ "Definición de la Política Educativa",
                     `Tit_cod` == "210" & `Programa cod` == "571" ~ "Desarrollo y Seguimiento del Sistema Educativo",
                     `Tit_cod` == "210" & `Programa cod` == "572" ~ "Administración del Sistema Educativo",
                     `Tit_cod` == "210" & `Programa cod` == "573" ~ "Implementación de la Política Educativa",
                     `Tit_cod` == "210" & `Programa cod` == "574" ~ "Transferencias a la Educación Superior",
                     `Tit_cod` == "210" & `Programa cod` == "580" ~ "Dirección y Administración",
                     `Tit_cod` == "211" & `Programa cod` == "621" ~ "Dirección Superior y Administración General",
                     `Tit_cod` == "211" & `Programa cod` == "622" ~ "Planificación en Salud ------ OTROS",
                     `Tit_cod` == "211" & `Programa cod` == "623" ~ "Transferencias Instituciones del Sector Salud",
                     `Tit_cod` == "211" & `Programa cod` == "625" ~ "Nutrición y Desarrollo Infantil",
                     `Tit_cod` == "211" & `Programa cod` == "627" ~ "Desarrollo del Sector Salud",
                     `Tit_cod` == "211" & `Programa cod` == "630" ~ "Gestión Intrainstitucional",
                     `Tit_cod` == "211" & `Programa cod` == "631" ~ "Rectoría de la Producción Social de la Salud",
                     `Tit_cod` == "211" & `Programa cod` == "632" ~ "Provisión de Servicios de Salud",
                     `Tit_cod` == "211" & `Programa cod` == "633" ~ "Desarrollo Social y Lucha contra la Pobreza",
                     `Tit_cod` == "212" & `Programa cod` == "635" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "212" & `Programa cod` == "729" ~ "Actividades Centrales",
                     `Tit_cod` == "212" & `Programa cod` == "731" ~ "Asuntos del Trabajo",
                     `Tit_cod` == "212" & `Programa cod` == "732" ~ "Desarrollo y Seguridad Social",
                     `Tit_cod` == "212" & `Programa cod` == "733" ~ "Tribunal Administrativo de la Seguridad Social",
                     `Tit_cod` == "212" & `Programa cod` == "734" ~ "Pensiones y Jubilaciones",
                     `Tit_cod` == "212" & `Programa cod` == "735" ~ "Transferencias y aportes varios",
                     `Tit_cod` == "213" & `Programa cod` == "749" ~ "Actividades Centrales",
                     `Tit_cod` == "213" & `Programa cod` == "751" ~ "Conservación del Patrimonio Cultural",
                     `Tit_cod` == "213" & `Programa cod` == "753" ~ "Gestión y Desarrollo Cultural",
                     `Tit_cod` == "213" & `Programa cod` == "755" ~ "Sistema Nacional de Bibliotecas",
                     `Tit_cod` == "213" & `Programa cod` == "758" ~ "Desarrollo Artístico y Extensión Musical",
                     `Tit_cod` == "213" & `Programa cod` == "760" ~ "Transferencias varias",
                     `Tit_cod` == "214" & `Programa cod` == "779" ~ "Actividad Central",
                     `Tit_cod` == "214" & `Programa cod` == "780" ~ "Promoción de la Paz y la Convivencia Ciudadana",
                     `Tit_cod` == "214" & `Programa cod` == "781" ~ "Procuraduría General de la República",
                     `Tit_cod` == "214" & `Programa cod` == "783"~ "Administración Penitenciaria",
                     `Tit_cod` == "214" & `Programa cod` == "784" ~ "Registro Nacional",
                     `Tit_cod` == "214" & `Programa cod` == "786" ~ "Actividades Centrales",
                     `Tit_cod` == "214" & `Programa cod` == "787" ~ "Actividades comunes a la atención de personas adscritas al sistema penitenciario nacional y prevención de la violencia y promoción de la paz social",
                     `Tit_cod` == "214" & `Programa cod` == "788" ~ "Actividades comunes a la defensa del estado asistencia jurídica y prevención, detección y combate de la corrupción",
                     `Tit_cod` == "214" & `Programa cod` == "789" ~ "Atención de personas adscritas al sistema penitenciario nacional",
                     `Tit_cod` == "214" & `Programa cod` == "790" ~ "Prevención de la violencia y promoción de la paz social",
                     `Tit_cod` == "214" & `Programa cod` == "791" ~ "Defensa del estado y asistencia jurídica al sector público",
                     `Tit_cod` == "214" & `Programa cod` == "793" ~ "Prevención, detección y combate de la corrupción",
                     `Tit_cod` == "214" & `Programa cod` == "794" ~ "Registro Nacional",
                     `Tit_cod` == "215" & `Programa cod` == "811" ~ "Proyección de la Comunidad",
                     `Tit_cod` == "215" & `Programa cod` == "812" ~ "Desarrollo Social y Lucha contra la Pobreza",
                     `Tit_cod` == "215" & `Programa cod` == "814" ~ "Actividades Centrales",
                     `Tit_cod` == "215" & `Programa cod` == "815" ~ "Ordenamiento Territorial",
                     `Tit_cod` == "216" & `Programa cod` == "792" ~ "Actividades Centrales",
                     `Tit_cod` == "216" & `Programa cod` == "796" ~ "Política Comercial Externa",
                     `Tit_cod` == "216" & `Programa cod` == "797" ~ "Programa de Integración Fronteriza",
                     `Tit_cod` == "216" & `Programa cod` == "798" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "217" & `Programa cod` == "863" ~ "Actividades Centrales",
                     `Tit_cod` == "217" & `Programa cod` == "865" ~ "Transferencias Varias",
                     `Tit_cod` == "217" & `Programa cod` == "866" ~ "Fondo Especial PL 480",
                     `Tit_cod` == "217" & `Programa cod` == "870" ~ "Transferencias varias",
                     `Tit_cod` == "217" & `Programa cod` == "874" ~ "Planificación y Coordinación Económica, Social e Institucional",
                     `Tit_cod` == "218" & `Programa cod` == "893" ~ "Coordinación del Desarrollo Científico y Tecnológico",
                     `Tit_cod` == "218" & `Programa cod` == "894" ~ "Innovación y Capital Humano para la Competitividad",
                     `Tit_cod` == "218" & `Programa cod` == "895" ~ "Transferencias varias",
                     `Tit_cod` == "218" & `Programa cod` == "899" ~ "Rectoría del Sector Telecomunicaciones",
                     `Tit_cod` == "219" & `Programa cod` == "879" ~ "Actividades Centrales",
                     `Tit_cod` == "219" & `Programa cod` == "883" ~ "Tribunal Ambiental Administrativo",
                     `Tit_cod` == "219" & `Programa cod` == "887" ~ "Dirección de Agua",
                     `Tit_cod` == "219" & `Programa cod` == "888" ~ "Instituto Meteorológico Nacional ",
                     `Tit_cod` == "219" & `Programa cod` == "889" ~ "Secretaría Técnica Nacional Ambiental",
                     `Tit_cod` == "219" & `Programa cod` == "890" ~ "Hidrocarburos, Transporte y Comercio de Combustibles",
                     `Tit_cod` == "219" & `Programa cod` == "897" ~ "Planificación Enérgetica Nacional",
                     `Tit_cod` == "219" & `Programa cod` == "898" ~ "Geología y Minas",
                     `Tit_cod` == "219" & `Programa cod` == "899" ~ "Rectoría del Sector Telecomunicaciones",
                     `Tit_cod` == "219" & `Programa cod` == "936" ~ "Transferencias del Sector Energía y Minas",
                     `Tit_cod` == "230" & `Programa cod` == "825" ~ "Servicio de la Deuda Pública",
                     `Tit_cod` == "231" & `Programa cod` == "743" ~ "Regímenes de Pensiones",
                     `Tit_cod` == "232" & `Programa cod` == "900" ~ "Partidas Específicas San José",
                     `Tit_cod` == "232" & `Programa cod` == "901" ~ "Partidas Específicas Alajuela",
                     `Tit_cod` == "232" & `Programa cod` == "902" ~ "Partidas Específicas Cartago",
                     `Tit_cod` == "232" & `Programa cod` == "903" ~ "Partidas Específicas Heredia",
                     `Tit_cod` == "232" & `Programa cod` == "904" ~ "Partidas Específicas Guanacaste",
                     `Tit_cod` == "232" & `Programa cod` == "905" ~ "Partidas Específicas Puntarenas",
                     `Tit_cod` == "232" & `Programa cod` == "906" ~ "Partidas Específicas Limón",
                     `Tit_cod` == "301" & `Programa cod` == "802" ~ "Poder Judicial",
                     `Tit_cod` == "301" & `Programa cod` == "926" ~ "Dirección y Administración",
                     `Tit_cod` == "301" & `Programa cod` == "927" ~ "Servicio Jurisdiccional",
                     `Tit_cod` == "301" & `Programa cod` == "928" ~ "Servicio de Investigación Judicial",
                     `Tit_cod` == "301" & `Programa cod` == "929" ~ "Servicio Ejercicio de la Acción Penal Pública",
                     `Tit_cod` == "301" & `Programa cod` == "930" ~ "Servicio de Defensa Pública",
                     `Tit_cod` == "301" & `Programa cod` == "931" ~ "Servicio de Notariado",
                     `Tit_cod` == "301" & `Programa cod` == "932" ~ "Servicio de Justicia de Tránsito",
                     `Tit_cod` == "301" & `Programa cod` == "942" ~ "Aporte Local Préstamo 1377/OC-CR",
                     `Tit_cod` == "301" & `Programa cod` == "943" ~ "Segunda Etapa Proyecto de Modernización de la Justicia",
                     `Tit_cod` == "301" & `Programa cod` == "950" ~ "Servicio de Atención y Protección de Víctimas y Testigos",
                     `Tit_cod` == "401" & `Programa cod` == "850" ~ "Tribunal Supremo de Elecciones"
                     
                   ),
                   `Subprograma cod` = substr(sigaf_gasto_actual_mensual$`Centro Gestor`, 7, 8),
                   `Subprograma des` = case_when(
                     `Tit_cod` == "101" & `Programa cod` == "002" & `Subprograma cod` == "00"  ~ "Asamblea Legislativa",
                     `Tit_cod` == "102" & `Programa cod` == "009" & `Subprograma cod` == "00"  ~ "Dirección Superior y Apoyo Administrativo",
                     `Tit_cod` == "102" & `Programa cod` == "012" & `Subprograma cod` == "00" ~ "Fiscalización Superior de la Hacienda Pública",
                     `Tit_cod` == "103" & `Programa cod` == "806" & `Subprograma cod` == "00" ~ "Donación PRODERE",
                     `Tit_cod` == "103" & `Programa cod` == "807" & `Subprograma cod` == "00" ~ "Defensoría de los Habitantes de la República",
                     `Tit_cod` == "103" & `Programa cod` == "808" & `Subprograma cod` == "00" ~ "Defensoría de los Habitantes",
                     `Tit_cod` == "201" & `Programa cod` == "021" & `Subprograma cod` == "00" ~ "Administración Superior",
                     `Tit_cod` == "201" & `Programa cod` == "024" & `Subprograma cod` == "01" ~ "Dirección General de Servicio Civil",
                     `Tit_cod` == "201" & `Programa cod` == "024" & `Subprograma cod` == "02" ~ "Tribunal de Servicio Civil",
                     `Tit_cod` == "201" & `Programa cod` == "027" & `Subprograma cod` == "00" ~ "Información y comunicación",
                     `Tit_cod` == "202" & `Programa cod` == "034" & `Subprograma cod` == "00" ~ "Administración Superior",
                     `Tit_cod` == "202" & `Programa cod` == "035" & `Subprograma cod` == "00" ~ "Delegados Presidenciales",
                     `Tit_cod` == "202" & `Programa cod` == "041" & `Subprograma cod` == "00" ~ "Dirección de Inteligencia y Seguridad Nacional",
                     `Tit_cod` == "202" & `Programa cod` == "042" & `Subprograma cod` == "00" ~ "Unidad Especial de Intervención",
                     `Tit_cod` == "203" & `Programa cod` == "044" & `Subprograma cod` == "00" ~ "Actividad Central",
                     `Tit_cod` == "203" & `Programa cod` == "048" & `Subprograma cod` == "00" ~ "Tribunal Administrativo Migratorio",
                     `Tit_cod` == "203" & `Programa cod` == "049" & `Subprograma cod` == "00" ~ "Desarrollo de la Comunidad",
                     `Tit_cod` == "203" & `Programa cod` == "051" & `Subprograma cod` == "00" ~ "Programación Publicitaria",
                     `Tit_cod` == "203" & `Programa cod` == "054" & `Subprograma cod` == "01" ~ "Control de Migración y Extranjería en el País",
                     `Tit_cod` == "203" & `Programa cod` == "054" & `Subprograma cod` == "02" ~ "Transferencias",
                     `Tit_cod` == "203" & `Programa cod` == "054" & `Subprograma cod` == "03" ~ "Imprenta Nacional",
                     `Tit_cod` == "204" & `Programa cod` == "079" & `Subprograma cod` == "00" ~ "Actividad Central",
                     `Tit_cod` == "204" & `Programa cod` == "081" & `Subprograma cod` == "00" ~ "Servicio Exterior",
                     `Tit_cod` == "204" & `Programa cod` == "082" & `Subprograma cod` == "00" ~ "Política Exterior",
                     `Tit_cod` == "204" & `Programa cod` == "083" & `Subprograma cod` == "00" ~ "Cooperación Internacional",
                     `Tit_cod` == "204" & `Programa cod` == "084" & `Subprograma cod` == "00" ~ "Dirección General de Protocolo y Ceremonial del Estado",
                     `Tit_cod` == "204" & `Programa cod` == "085" & `Subprograma cod` == "00" ~ "Promoción Externa",
                     `Tit_cod` == "204" & `Programa cod` == "088" & `Subprograma cod` == "00" ~ "Cuotas a Organismos Internacionales",
                     `Tit_cod` == "205" & `Programa cod` == "089" & `Subprograma cod` == "00" ~ "Gestión Administrativa de los Cuerpos Policiales",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "01" ~ "Policía de Control de Drogas",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "02" ~ "Escuela Nacional de Policía",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "03" ~ "Seguridad Ciudadana",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "04" ~ "Servicio Nacional de Guardacostas",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "05" ~ "Servicio de Vigilancia Áerea",
                     `Tit_cod` == "205" & `Programa cod` == "090" & `Subprograma cod` == "06" ~ "Recursos policiales impuesto al banano",
                     `Tit_cod` == "205" & `Programa cod` == "091" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "205" & `Programa cod` == "092" & `Subprograma cod` == "00" ~ "Actividades comunes al servicio de seguridad ciudadana, servicio de seguridad fronteriza, servicio de seguridad aérea, y servicios de investigación y represión del narcotráfico",
                     `Tit_cod` == "205" & `Programa cod` == "093" & `Subprograma cod` == "00" ~ "Servicio de Seguridad Ciudadana",
                     `Tit_cod` == "205" & `Programa cod` == "094" & `Subprograma cod` == "00" ~ "Servicio de Seguridad Fronteriza",
                     `Tit_cod` == "205" & `Programa cod` == "095" & `Subprograma cod` == "00" ~ "Servicio de Seguridad Aérea",
                     `Tit_cod` == "205" & `Programa cod` == "096" & `Subprograma cod` == "00" ~ "Servicio de Seguridad Marítima",
                     `Tit_cod` == "205" & `Programa cod` == "097" & `Subprograma cod` == "00" ~ "Servicios de Investigación y Represión al Narcotráfico ",
                     `Tit_cod` == "205" & `Programa cod` == "098" & `Subprograma cod` == "00" ~ "Policía Antidrogas",
                     `Tit_cod` == "205" & `Programa cod` == "107" & `Subprograma cod` == "00" ~ "Seguridad Ciudadana",
                     `Tit_cod` == "205" & `Programa cod` == "109" & `Subprograma cod` == "02" ~ "Vigilancia Áerea",
                     `Tit_cod` == "205" & `Programa cod` == "109" & `Subprograma cod` == "03" ~ "Servicio Nacional de Guardacostas",
                     `Tit_cod` == "206" & `Programa cod` == "131" & `Subprograma cod` == "00" ~ "Unidad Ejecutora del Catastro y Registro",
                     `Tit_cod` == "206" & `Programa cod` == "132" & `Subprograma cod` == "00" ~ "Administración Superior",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "01" ~ "Dirección y Coordinación General",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "02" ~ "Gestión de Ingresos Internos",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "03" ~ "Gestión Aduanera",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "04" ~ "Asesoría Hacendaria",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "05" ~ "Investigaciones Fiscales",
                     `Tit_cod` == "206" & `Programa cod` == "134" & `Subprograma cod` == "06" ~ "Transparencia Hacendaria",
                     `Tit_cod` == "206" & `Programa cod` == "135" & `Subprograma cod` == "01" ~ "Tribunal Fiscal Administrativo",
                     `Tit_cod` == "206" & `Programa cod` == "135" & `Subprograma cod` == "02" ~ "Tribunal Aduanero",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "01" ~ "Dirección y Coordinación General",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "02" ~ "Dirección y coordinación del proceso presupuestario del sector público",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "03" ~ "Dirección de Administración de Bienes y Contratación",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "04" ~ "Gestión de Caja del Gobierno Central",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "05" ~ "Regulación y Registro Contable de la Hacienda Pública",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "06" ~ "Dirección General de Crédito Público",
                     `Tit_cod` == "206" & `Programa cod` == "136" & `Subprograma cod` == "07" ~ "Secretaria Técnica de la Autoridad Presupuestaria",
                     `Tit_cod` == "206" & `Programa cod` == "138" & `Subprograma cod` == "00" ~ "Administración Tecnológica",
                     `Tit_cod` == "206" & `Programa cod` == "138" & `Subprograma cod` == "01" ~ "Administración Tecnológica",
                     `Tit_cod` == "206" & `Programa cod` == "138" & `Subprograma cod` == "02" ~ "Centro de Investigación y Formación Hacendaria",
                     `Tit_cod` == "207" & `Programa cod` == "169" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "207" & `Programa cod` == "170" & `Subprograma cod` == "00" ~ "Secretaria de Planificación del Sector Agropecuario",
                     `Tit_cod` == "207" & `Programa cod` == "171" & `Subprograma cod` == "00" ~ "Servicio Fitosanitario",
                     `Tit_cod` == "207" & `Programa cod` == "172" & `Subprograma cod` == "00" ~ "Instituto Nacional de Innovación Tecnológica Agropecuaria",
                     `Tit_cod` == "207" & `Programa cod` == "173" & `Subprograma cod` == "00" ~ "Salud Animal",
                     `Tit_cod` == "207" & `Programa cod` == "175" & `Subprograma cod` == "00" ~ "Dirección Superior de Operaciones Regionales y Extensión Agropecuaria",
                     `Tit_cod` == "207" & `Programa cod` == "176" & `Subprograma cod` == "00" ~ "Desarrollo Rural",
                     `Tit_cod` == "207" & `Programa cod` == "177" & `Subprograma cod` == "00" ~ "Apoyo a instituciones y organizaciones del sector agropecuario",
                     `Tit_cod` == "207" & `Programa cod` == "180" & `Subprograma cod` == "00" ~ "Proyecto Desarrollo Agrícola de la Península de Nicoya FIDA-BCIE",
                     `Tit_cod` == "207" & `Programa cod` == "182" & `Subprograma cod` == "00" ~ "Fomento de la Producción Agropecuaria Sostenible",
                     `Tit_cod` == "207" & `Programa cod` == "185" & `Subprograma cod` == "00" ~ "Desarrollo Sostenible Cuenca Binacional Río Sixaola",
                     `Tit_cod` == "208" & `Programa cod` == "215" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "208" & `Programa cod` == "217" & `Subprograma cod` == "00" ~ "Mejora Regulatoria",
                     `Tit_cod` == "208" & `Programa cod` == "218" & `Subprograma cod` == "00" ~ "Gestión de Reglamentación Técnica",
                     `Tit_cod` == "208" & `Programa cod` == "219" & `Subprograma cod` == "00" ~ "Dirección General Pequeña y Mediana Empresa",
                     `Tit_cod` == "208" & `Programa cod` == "220" & `Subprograma cod` == "00" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "208" & `Programa cod` == "223" & `Subprograma cod` == "00" ~ "Protección del Consumidor",
                     `Tit_cod` == "208" & `Programa cod` == "224" & `Subprograma cod` == "00" ~ "Promoción de la Competencia",
                     `Tit_cod` == "208" & `Programa cod` == "229" & `Subprograma cod` == "00" ~ "Dirección de Estudios Económicos",
                     `Tit_cod` == "209" & `Programa cod` == "326" & `Subprograma cod` == "00" ~ "Administración Superior",
                     `Tit_cod` == "209" & `Programa cod` == "327" & `Subprograma cod` == "00" ~ "Mejoramiento y Conservación de la Red Vial",
                     `Tit_cod` == "209" & `Programa cod` == "327" & `Subprograma cod` == "01" ~ "Atención de Infraestructura Vial y Fluvial",
                     `Tit_cod` == "209" & `Programa cod` == "327" & `Subprograma cod` == "02" ~ "Proyecto Bajos del Chilamate-Vuelta a Kooper",
                     `Tit_cod` == "209" & `Programa cod` == "327" & `Subprograma cod` == "03" ~ "Red Vial Cantonal I (MOPT-BID)",
                     `Tit_cod` == "209" & `Programa cod` == "328" & `Subprograma cod` == "00" ~ "Puertos y Regulación Marítima",
                     `Tit_cod` == "209" & `Programa cod` == "329" & `Subprograma cod` == "00" ~ "Edificaciones Nacionales",
                     `Tit_cod` == "209" & `Programa cod` == "330" & `Subprograma cod` == "00" ~ "Instituto Geográfico Nacional",
                     
                     `Tit_cod` == "209" & `Programa cod` == "331" & `Subprograma cod` == "01" ~ "Administración Vial y Transporte Terrestre",
                     `Tit_cod` == "209" & `Programa cod` == "331" & `Subprograma cod` == "02" ~ "Tribunal Administrativo de Transporte",
                     `Tit_cod` == "209" & `Programa cod` == "332" & `Subprograma cod` == "00" ~ "Proyectos y Transferencias Sectoriales",
                     `Tit_cod` == "209" & `Programa cod` == "333" & `Subprograma cod` == "00" ~ "Proyecto MOPT-KFW",
                     `Tit_cod` == "209" & `Programa cod` == "334" & `Subprograma cod` == "00" ~ "Complejo Vial Costanera Sur- BCIE-1605",
                     `Tit_cod` == "209" & `Programa cod` == "386" & `Subprograma cod` == "00" ~ "Construcción Vías Contrapartida BIRF 2764-CR",
                     `Tit_cod` == "209" & `Programa cod` == "393" & `Subprograma cod` == "00" ~ "Construcción Vías Contrato BIRF 2764-CR",
                     `Tit_cod` == "210" & `Programa cod` == "501" & `Subprograma cod` == "00" ~ "Administración Central",
                     `Tit_cod` == "210" & `Programa cod` == "550" & `Subprograma cod` == "00" ~ "Definición y Planificación de la Política Educativa",
                     `Tit_cod` == "210" & `Programa cod` == "551" & `Subprograma cod` == "00" ~ "Servicios de Apoyo a la Gestión",
                     `Tit_cod` == "210" & `Programa cod` == "552" & `Subprograma cod` == "00" ~ "Capacitación y desarrollo profesional",
                     `Tit_cod` == "210" & `Programa cod` == "553" & `Subprograma cod` == "00" ~ "Desarrollo curricular y vinculo al trabajo",
                     `Tit_cod` == "210" & `Programa cod` == "554" & `Subprograma cod` == "00" ~ "Infraestructura y equipamiento del sistema educativo",
                     `Tit_cod` == "210" & `Programa cod` == "555" & `Subprograma cod` == "00" ~ "Aplicación de la Tecnología a la Educación",
                     `Tit_cod` == "210" & `Programa cod` == "556" & `Subprograma cod` == "00" ~ "Gestión y Evaluación de la Calidad",
                     `Tit_cod` == "210" & `Programa cod` == "557" & `Subprograma cod` == "00" ~ "Desarrollo y Coordinación Regional",
                     `Tit_cod` == "210" & `Programa cod` == "558" & `Subprograma cod` == "00" ~ "Programas de Equidad",
                     `Tit_cod` == "210" & `Programa cod` == "570" & `Subprograma cod` == "00" ~ "Definición de la Política Educativa",
                     `Tit_cod` == "210" & `Programa cod` == "571" & `Subprograma cod` == "00" ~ "Desarrollo y Seguimiento del Sistema Educativo",
                     `Tit_cod` == "210" & `Programa cod` == "572" & `Subprograma cod` == "00" ~ "Administración del Sistema Educativo",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "01" ~ "Enseñanza Preescolar, I y II Ciclos",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "02" ~ "III Ciclo y Educación Diversificada Académica",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "03" ~ "III Ciclo y Educación Diversificada Técnica",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "04" ~ "Enseñanza Especial",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "05" ~ "Educación para Jóvenes y Adultos",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "06" ~ "Administración Regional del Sistema Educativo",
                     `Tit_cod` == "210" & `Programa cod` == "573" & `Subprograma cod` == "07" ~ "Programa de Equidad Social",
                     `Tit_cod` == "210" & `Programa cod` == "574" & `Subprograma cod` == "00" ~ "Transferencias a la Educación Superior",
                     `Tit_cod` == "210" & `Programa cod` == "580" & `Subprograma cod` == "00" ~ "Dirección y Administración",
                     `Tit_cod` == "211" & `Programa cod` == "621" & `Subprograma cod` == "00" ~ "Dirección Superior y Administración General",
                     
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "01" ~ "Planificación en Salud",
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "02" ~ "Normalización y Acreditación",
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "03" ~ "Registros y Controles",
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "04" ~ "Vigilancia y Control del Ambiente Humano",
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "06" ~ "Vigilancia de la Salud",
                     `Tit_cod` == "211" & `Programa cod` == "622" & `Subprograma cod` == "07" ~ "Nivel Regional",
                     
                     `Tit_cod` == "211" & `Programa cod` == "623" & `Subprograma cod` == "00" ~ "Transferencias Instituciones del Sector Salud",
                     `Tit_cod` == "211" & `Programa cod` == "625" & `Subprograma cod` == "00" ~ "Nutrición y Desarrollo Infantil",
                     `Tit_cod` == "211" & `Programa cod` == "627" & `Subprograma cod` == "00" ~ "Desarrollo del Sector Salud",
                     `Tit_cod` == "211" & `Programa cod` == "630" & `Subprograma cod` == "00" ~ "Gestión Intrainstitucional",
                     
                     `Tit_cod` == "211" & `Programa cod` == "631" & `Subprograma cod` == "00" ~ "Rectoría de la Producción Social de la Salud",
                     `Tit_cod` == "211" & `Programa cod` == "631" & `Subprograma cod` == "01" ~ "Rectoría de la Producción Social de la Salud",
                     `Tit_cod` == "211" & `Programa cod` == "631" & `Subprograma cod` == "02" ~ "Control del tabaco y sus efectos nocivos en la salud",
                     
                     `Tit_cod` == "211" & `Programa cod` == "632" & `Subprograma cod` == "00" ~ "Provisión de Servicios de Salud",
                     `Tit_cod` == "211" & `Programa cod` == "633" & `Subprograma cod` == "00" ~ "Desarrollo Social y Lucha contra la Pobreza",
                     `Tit_cod` == "212" & `Programa cod` == "635" & `Subprograma cod` == "00" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "212" & `Programa cod` == "729" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "212" & `Programa cod` == "731" & `Subprograma cod` == "00" ~ "Asuntos del Trabajo",
                     
                     
                     `Tit_cod` == "212" & `Programa cod` == "732" & `Subprograma cod` == "00" ~ "Desarrollo y Seguridad Social",
                     `Tit_cod` == "212" & `Programa cod` == "732" & `Subprograma cod` == "01" ~ "Gestión y Administración del FODESAF",
                     `Tit_cod` == "212" & `Programa cod` == "732" & `Subprograma cod` == "02" ~ "Empleo y Seguridad Social",
                     
                     `Tit_cod` == "212" & `Programa cod` == "733" & `Subprograma cod` == "00" ~ "Tribunal Administrativo de la Seguridad Social",
                     `Tit_cod` == "212" & `Programa cod` == "734" & `Subprograma cod` == "00" ~ "Pensiones y Jubilaciones",
                     `Tit_cod` == "212" & `Programa cod` == "735" & `Subprograma cod` == "00" ~ "Transferencias y aportes varios",
                     `Tit_cod` == "213" & `Programa cod` == "749" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "213" & `Programa cod` == "751" & `Subprograma cod` == "00" ~ "Conservación del Patrimonio Cultural",
                     `Tit_cod` == "213" & `Programa cod` == "753" & `Subprograma cod` == "00" ~ "Gestión y Desarrollo Cultural",
                     `Tit_cod` == "213" & `Programa cod` == "755" & `Subprograma cod` == "00" ~ "Sistema Nacional de Bibliotecas",
                     `Tit_cod` == "213" & `Programa cod` == "758" & `Subprograma cod` == "00" ~ "Desarrollo Artístico y Extensión Musical",
                     `Tit_cod` == "213" & `Programa cod` == "760" & `Subprograma cod` == "00" ~ "Transferencias varias",
                     `Tit_cod` == "214" & `Programa cod` == "779" & `Subprograma cod` == "00" ~ "Actividad Central",
                     `Tit_cod` == "214" & `Programa cod` == "780" & `Subprograma cod` == "00" ~ "Promoción de la Paz y la Convivencia Ciudadana",
                     `Tit_cod` == "214" & `Programa cod` == "781" & `Subprograma cod` == "00" ~ "Procuraduría General de la República",
                     `Tit_cod` == "214" & `Programa cod` == "783" & `Subprograma cod` == "00" ~ "Administración Penitenciaria",
                     `Tit_cod` == "214" & `Programa cod` == "784" & `Subprograma cod` == "00" ~ "Registro Nacional",
                     `Tit_cod` == "214" & `Programa cod` == "786" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "214" & `Programa cod` == "787" & `Subprograma cod` == "00" ~ "Actividades comunes a la atención de personas adscritas al sistema penitenciario nacional y prevención de la violencia y promoción de la paz social",
                     `Tit_cod` == "214" & `Programa cod` == "788" & `Subprograma cod` == "00" ~ "Actividades comunes a la defensa del estado asistencia jurídica y prevención, detección y combate de la corrupción",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "00" ~ "Atención de personas adscritas al sistema penitenciario nacional",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "01" ~ "Atención de hombres adultos en centros institucionales",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "02" ~ "Atención de mujeres sujetas a medidas privativas de libertad",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "03" ~ "Atención a población penal juvenil",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "04" ~ "Atención de población en centros semi institucionales",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "05" ~ "Atención de población en comunidad ",
                     `Tit_cod` == "214" & `Programa cod` == "789" & `Subprograma cod` == "06" ~ "Atención de población sujeta a dispositivos electrónicos",
                     
                     `Tit_cod` == "214" & `Programa cod` == "790" & `Subprograma cod` == "00" ~ "Prevención de la violencia y promoción de la paz social",
                     `Tit_cod` == "214" & `Programa cod` == "791" & `Subprograma cod` == "00" ~ "Defensa del estado y asistencia jurídica al sector público",
                     `Tit_cod` == "214" & `Programa cod` == "793" & `Subprograma cod` == "00" ~ "Prevención, detección y combate de la corrupción",
                     `Tit_cod` == "214" & `Programa cod` == "794" & `Subprograma cod` == "00" ~ "Registro Nacional",
                     `Tit_cod` == "215" & `Programa cod` == "811" & `Subprograma cod` == "00" ~ "Proyección de la Comunidad",
                     `Tit_cod` == "215" & `Programa cod` == "812" & `Subprograma cod` == "00" ~ "Desarrollo Social y Lucha contra la Pobreza",
                     `Tit_cod` == "215" & `Programa cod` == "814" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "215" & `Programa cod` == "815" & `Subprograma cod` == "00" ~ "Ordenamiento Territorial",
                     `Tit_cod` == "216" & `Programa cod` == "792" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "216" & `Programa cod` == "796" & `Subprograma cod` == "00" ~ "Política Comercial Externa",
                     `Tit_cod` == "216" & `Programa cod` == "797" & `Subprograma cod` == "00" ~ "Programa de Integración Fronteriza",
                     `Tit_cod` == "216" & `Programa cod` == "798" & `Subprograma cod` == "00" ~ "Partidas No Asignables a Programas",
                     `Tit_cod` == "217" & `Programa cod` == "863" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "217" & `Programa cod` == "865" & `Subprograma cod` == "00" ~ "Transferencias Varias",
                     `Tit_cod` == "217" & `Programa cod` == "866" & `Subprograma cod` == "00" ~ "Fondo Especial PL 480",
                     `Tit_cod` == "217" & `Programa cod` == "870" & `Subprograma cod` == "00" ~ "Transferencias varias",
                     `Tit_cod` == "217" & `Programa cod` == "874" & `Subprograma cod` == "00" ~ "Planificación y Coordinación Económica, Social e Institucional",
                     `Tit_cod` == "218" & `Programa cod` == "893" & `Subprograma cod` == "00" ~ "Coordinación del Desarrollo Científico y Tecnológico",
                     `Tit_cod` == "218" & `Programa cod` == "894" & `Subprograma cod` == "00" ~ "Innovación y Capital Humano para la Competitividad",
                     `Tit_cod` == "218" & `Programa cod` == "895" & `Subprograma cod` == "00" ~ "Transferencias varias",
                     `Tit_cod` == "218" & `Programa cod` == "899" & `Subprograma cod` == "00" ~ "Rectoría del Sector Telecomunicaciones",
                     `Tit_cod` == "219" & `Programa cod` == "879" & `Subprograma cod` == "00" ~ "Actividades Centrales",
                     `Tit_cod` == "219" & `Programa cod` == "883" & `Subprograma cod` == "00" ~ "Tribunal Ambiental Administrativo",
                     `Tit_cod` == "219" & `Programa cod` == "887" & `Subprograma cod` == "00" ~ "Conservación y Manejo de la Biodiversidad",
                     `Tit_cod` == "219" & `Programa cod` == "888" & `Subprograma cod` == "00" ~ "Hidrometeorología Aplicada",
                     `Tit_cod` == "219" & `Programa cod` == "889" & `Subprograma cod` == "00" ~ "Evaluación y Control Ambiental",
                     `Tit_cod` == "219" & `Programa cod` == "890" & `Subprograma cod` == "00" ~ "Hidrocarburos, Transporte y Comercio de Combustibles",
                     `Tit_cod` == "219" & `Programa cod` == "897" & `Subprograma cod` == "00" ~ "Conservación y Uso Racional de la Energía",
                     `Tit_cod` == "219" & `Programa cod` == "898" & `Subprograma cod` == "00" ~ "Geología y Minas",
                     `Tit_cod` == "219" & `Programa cod` == "899" & `Subprograma cod` == "00" ~ "Rectoría del Sector Telecomunicaciones",
                     `Tit_cod` == "219" & `Programa cod` == "936" & `Subprograma cod` == "00" ~ "Transferencias del Sector Energía y Minas",
                     `Tit_cod` == "230" & `Programa cod` == "825" & `Subprograma cod` == "00" ~ "Servicio de la Deuda Pública",
                     `Tit_cod` == "231" & `Programa cod` == "743" & `Subprograma cod` == "00" ~ "Regímenes de Pensiones",
                     `Tit_cod` == "232" & `Programa cod` == "900" & `Subprograma cod` == "00" ~ "Partidas Específicas San José",
                     `Tit_cod` == "232" & `Programa cod` == "901" & `Subprograma cod` == "00" ~ "Partidas Específicas Alajuela",
                     `Tit_cod` == "232" & `Programa cod` == "902" & `Subprograma cod` == "00" ~ "Partidas Específicas Cartago",
                     `Tit_cod` == "232" & `Programa cod` == "903" & `Subprograma cod` == "00" ~ "Partidas Específicas Heredia",
                     `Tit_cod` == "232" & `Programa cod` == "904" & `Subprograma cod` == "00" ~ "Partidas Específicas Guanacaste",
                     `Tit_cod` == "232" & `Programa cod` == "905" & `Subprograma cod` == "00" ~ "Partidas Específicas Puntarenas",
                     `Tit_cod` == "232" & `Programa cod` == "906" & `Subprograma cod` == "00" ~ "Partidas Específicas Limón",
                     `Tit_cod` == "301" & `Programa cod` == "802" & `Subprograma cod` == "00" ~ "Poder Judicial",
                     `Tit_cod` == "301" & `Programa cod` == "926" & `Subprograma cod` == "00" ~ "Dirección y Administración",
                     `Tit_cod` == "301" & `Programa cod` == "927" & `Subprograma cod` == "00" ~ "Servicio Jurisdiccional",
                     `Tit_cod` == "301" & `Programa cod` == "928" & `Subprograma cod` == "00" ~ "Servicio de Investigación Judicial",
                     `Tit_cod` == "301" & `Programa cod` == "929" & `Subprograma cod` == "00" ~ "Servicio Ejercicio de la Acción Penal Pública",
                     `Tit_cod` == "301" & `Programa cod` == "930" & `Subprograma cod` == "00" ~ "Servicio de Defensa Pública",
                     `Tit_cod` == "301" & `Programa cod` == "931" & `Subprograma cod` == "00" ~ "Servicio de Notariado",
                     `Tit_cod` == "301" & `Programa cod` == "932" & `Subprograma cod` == "00" ~ "Servicio de Justicia de Tránsito",
                     `Tit_cod` == "301" & `Programa cod` == "942" & `Subprograma cod` == "00" ~ "Aporte Local Préstamo 1377/OC-CR",
                     `Tit_cod` == "301" & `Programa cod` == "943" & `Subprograma cod` == "00" ~ "Segunda Etapa Proyecto de Modernización de la Justicia",
                     `Tit_cod` == "301" & `Programa cod` == "950" & `Subprograma cod` == "00" ~ "Servicio de Atención y Protección de Víctimas y Testigos",
                     `Tit_cod` == "401" & `Programa cod` == "850" & `Subprograma cod` == "01" ~ "Tribunal Supremo de Elecciones",
                     `Tit_cod` == "401" & `Programa cod` == "850" & `Subprograma cod` == "02" ~ "Organización de Elecciones"
                   )
) 


                   
sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,
                   
                   ##############################################################
                   # Creación de la variable:  Fuente                           #
                   ##############################################################
                   
                  # `Fuente cod` = as.numeric(sigaf_gasto_actual_mensual$`Fuente cod`),
                   
                   Fuente= case_when(
                     `Fuente cod` == '001' ~ "Ingresos corrientes",
                     `Fuente cod` == '002' ~ "Venta de terrenos",
                     `Fuente cod` == '030' ~ "Recuperación de otras inversiones",
                     `Fuente cod` == '060' ~ "Transferencias de capital de Órganos Desconcentrados",
                     `Fuente cod` == '063' ~ "Donación de las Naciones Unidas para el Poder Judicial (Depto. de Ciencias Forenses del Organismo de Investigación Judicial)",
                     `Fuente cod` == '064' ~ "Donación Comunidad Económica Europea",
                     `Fuente cod` == '065' ~ "Donac. BIRF Fondo p/ Medio Ambiente Mund",
                     `Fuente cod` == '066' ~ "Donación del Ministerio de Relaciones Exteriores de Finlandia",
                     `Fuente cod` == '067' ~ "Donación Gobierno EE UU (OIJ)",
                     `Fuente cod` == '068' ~ "Cancelación operaciones cuasifiscales al Banco Central de Costa Rica (BCCR) . Acuerdo de Cooperación Energética de Caracas, Ley N° 8116",
                     `Fuente cod` == '069' ~ "Donación de la República de China",
                     `Fuente cod` == '070' ~ "Donación de la República de China",
                     `Fuente cod` == '121' ~ "Reintegro e imtereses del crédito externo PL-480",
                     `Fuente cod` == '280' ~ "Títulos valores de deuda interna",
                     `Fuente cod` == '281' ~ "Títulos valores de deuda interna (Deuda Política)",
                     `Fuente cod` == '282' ~ "Títulos valores de deuda interna (Caja Única)",
                     `Fuente cod` == '283' ~ "Títulos valores de deuda interna (Fideicomiso agropecuario)",
                     `Fuente cod` == '290' ~ "Bonos Cont.Part.Pol.",
                     `Fuente cod` == '450' ~ "Crédito BCIE-CR-26 FDS, Ley N° 7639 Programa de Infraestructura Universidad Nacional",
                     `Fuente cod` == '451' ~ "Crédito BCIE-CR-1129, Ley N° 7659 Proyecto de Desarrollo Agrícola Península de Nicoya",
                     `Fuente cod` == '452' ~ "Crédito BCIE-1605, Ley N° 8359 Programa para completar el complejo vial Costanera Sur",
                     `Fuente cod` == '453' ~ "Crédito BCIE 1709, Ley N° 8685 Programa de Gestión Integrada de Recursos Hídricos",
                     `Fuente cod` == '454' ~ "Crédito BCIE 2198 Programa de Alcantarillado y Con trol de Inundaciones para Limón Ley N° 9690",
                     `Fuente cod` == '490' ~ "Crédito BID CR569, Ley N° 7376 Proyecto para Financiar IV Etapa del Programa de Preinversión",
                     `Fuente cod` == '491' ~ "Crédito BID N° 636, Ley N° 7296 Programa de Suministro Agua Potable Centros Urbanos",
                     `Fuente cod` == '492' ~ "Crédito BID N° 637, Ley N° 7296 Rehabilitación Infraestructura Sanitaria Limón",
                     `Fuente cod` == '493' ~ "Crédito BID 711/OC-CR, Ley N° 7374 Programa de Mejoramiento Servicios de Salud",
                     `Fuente cod` == '494' ~ "Crédito BID N° 712/OC-CR, Ley N° 7374  Programa de Mejoramiento de Servicios de Salud",
                     `Fuente cod` == '495' ~ "Crédito BID 859-OC, Programa Modernización de la Administración de la Justicia Ley N° 7496",
                     `Fuente cod` == '496' ~ "Crédito BID 1010/OC-CR Proyecto de Educación Preescolar y Tercer Ciclo",
                     `Fuente cod` == '497' ~ "Crédito BID 1030/OC-CR, Ley N° 7760 Proyecto de apoyo a la profundización de la reforma del Estado y la apertura de los sectores financieros y de infraestructura al sector privado",
                     `Fuente cod` == '498' ~ "Crédito BID 1284/OC-CR, Ley N° 8154 Programa de Regularización del Catastro y Registro",
                     `Fuente cod` == '499' ~ "Crédito BID 1377/OC-CR, Ley N° 8273 Segunda Etapa del Programa de Modernización de la Administración de Justicia",
                     `Fuente cod` == '500' ~ "Crédito BID 1451/OC-CR, Ley N° 8403 Programa de Desarrollo del Sector Salud",
                     `Fuente cod` == '501' ~ "Crédito BID N° 667, Ley N° 7315 Proyecto Mejoramiento de la Calidad de la Educación General Básica",
                     `Fuente cod` == '502' ~ "Crédito BID 1436 OC-CR, Ley N° 8408 Programa de Fomento de la Producción Agropecuaria Sostenible",
                     `Fuente cod` == '503' ~ "Crédito BID 1556-OC-CR. Programa de Desarrollo Sostenible de la Cuenca del Río Sixaola",
                     `Fuente cod` == '504' ~ "Préstamo BID N° 2007/-OC-CR-Programa de Infraestructura Vial (PIVI) Ley N° 8845",
                     `Fuente cod` == '505'  ~ "Crédito BID N° 2098/-OC-CR-Programa Red Vial Cantonal (PIV1) Ley N° 8982",
                     `Fuente cod` == '506' ~ "Credito BID N° 1824/OC-CR-Programa de turismo en Áreas Silvestres Protegidas y su Contrato Modificatorio N°LEG/SGO/CID/DBDOC #35218709 Ley N° 8967",
                     `Fuente cod` == '507'  ~ "Crédito BID N° 2526/OC-CR-Programa para la Prevención de la Violencia y Promoción de la Inclusión Social Ley N° 9025",
                     `Fuente cod` == '508' ~ "Crédito BID N° 2852/OC-CR-Programa de Innovación y Capital Humano para la Competitividad, Ley N° 9218",
                     `Fuente cod` == '509'  ~ "Crédito BID N° 3071/OC-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                     `Fuente cod` == '510' ~ "Crédito BID N° 3072/CH-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                     `Fuente cod` == '511'  ~ "Crédito EXIMBANK Ley N° 9293 Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32 Tramo: Ruta N° 4 - Limón",
                     `Fuente cod` == '512' ~ "Crédito EXIMBANK Ley N° 9293 Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32, Tramo: Ruta N° 4 - Limón",
                     `Fuente cod` == '513'  ~ "Crédito BCIE-2157 Ley N° 9327 Proyecto de Mercado Regional Mayorista de la Región Chorotega",
                     `Fuente cod` == '514' ~ "Crédito BIRF 8593-CR Ley N° 9396 Programa por Resultados para el Fortalecimiento del Seguro Universal de Salud en Costa Rica",
                     `Fuente cod` == '515'   ~ "Crédito BID N° 3488/OC-CR-Programa de Integración Fronteriza (PIF) Ley N° 9451",
                     `Fuente cod` == '516' ~ "Crédito BID N° 4433/OC-CR Programa de Emergencias Tormenta Tropical Nate Ley N° 9595",
                     `Fuente cod` == '517'   ~ "Crédito BID N° 4507/OC-CR Programa Red Vial Cantonal II (PIV2) Ley N° 8982",
                     `Fuente cod` == '518' ~ "Crédito BID N° 4819/OC-CR Programa de Apoyo a la Sostenibilidad Fiscal Ley N° 9754",
                     `Fuente cod` == '530'   ~ "Préstamo BIRF N° 3654-CR, Ley N° 7441 Proyecto para Financiar Reforma del Sector Salud",
                     `Fuente cod` == '531' ~ "Crédito BIRF N° 4557-CR, Ley N° 8058 Programa de Pagos de Servicios Ambientales",
                     `Fuente cod` == '532'  ~ "Crédito BIRF 70-68-CR, Ley N° 8269 Proyecto de Fortalecimiento y Modernización del Sector Salud",
                     `Fuente cod` == '533' ~ "Crédito BIRF N° 7284-CR Proyecto Equidad y Eficiencia de la Educación",
                     `Fuente cod` == '534'  ~ "Crédito BIRF N° 7388-CR Proyecto Introducción Instrumentos Financieros para Gestión Ambiental",
                     `Fuente cod` == '535' ~ "Crédito BIRF N° 7594-CR Opción de Desembolsos Diferido Ante el Riesgo de Catástrofes (CAT-DDO)",
                     `Fuente cod` == '536'  ~ "Crédito BIRF 7498-CR Ley N° 8725 Proyecto de Limón Cuidad-Puerto",
                     `Fuente cod` == '537' ~ "Crédito BIRF N° 7686-CR Ley B° 8843 Apoyo de Políticas de Desarrollo de las Finanzas Públicas y la Competitividad",
                     `Fuente cod` == '538'   ~ "Crédito BIRF N° 8194-CR Ley N° 9144 Proyecto de Mejoramiento de la Educación Superior",
                     `Fuente cod` == '610' ~ "Préstamo FIDA 371-CR, Ley N° 7659 Proyecto de Desarrollo Agrícola Península de Nicoya",
                     `Fuente cod` == '650'  ~ "Préstamo Banco Japonés de Cooperación Internacional CR-P4, Ley N° 8559 Proyecto de Mejoramiento del Medio Ambiente del Area Metropolitana de San José",
                     `Fuente cod` == '660' ~ "Préstamo Corporación Andina De Fomento, Ley N° 8844 Proyecto Bajos de Chilamate -Vuelta Kooper",
                     `Fuente cod` == '690'   ~ "Crédito del Gobierno de los EE UU (Convenio PL-480 Leyes N° 6945, 6978, 7019, 7059, 7098, 7203 y 7307)",
                     `Fuente cod` == '692' ~ "Crédito KFW 2002-65-066 Programa de Rehabilitación y Mantenimiento de la Red Vial Cantonal, Leyes N° 6979, 7132 y 7109",
                     `Fuente cod` == '693'  ~ "Préstamos KFW, Ley N° 7132 Programa de Agua Potable y Saneamiento Básico Rural II",
                     `Fuente cod` == '694' ~ "Crédito KFW N° 28568 Programa de Saneamiento en Zonas Prioritarias, Ley N° 9723",
                     `Fuente cod` == '730'  ~ "Préstamo Export - Import Bank de la República de China Ley N° 7624 Proyecto Construcción Carretera Florencia - Naranjo",
                     `Fuente cod` == '890' ~ "Títulos valores de deuda externa",
                     `Fuente cod` == '900'  ~ "Superávit",
                     `Fuente cod` == '905' ~ "Superávit Contraloría",
                     `Fuente cod` == '906'  ~ "Superávit Asamblea Legislativa",
                     `Fuente cod` == '922' ~ "Superávit específico Colocación Títulos en el Exterior",
                     `Fuente cod` == '923'  ~ "Superávit específico Donación UE",
                     `Fuente cod` == '924' ~ "Superávit específico de la donación República de Corea-KOLFACI",
                     `Fuente cod` == '925'  ~ "Superávit específico préstamo CAF Bajos de Chilamate - Vuelta Kooper",
                     `Fuente cod` == '980' ~ "Superávi",
                     `Fuente cod` == '661'  ~ "Otro"
                   )
                   
       )

sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,
                   
                                       
                                      
                   ##############################################################
                   # Creación del clasificador económico ECO                    #
                   ##############################################################   
                   
                   CE1_cod = substr(sigaf_gasto_actual_mensual$`CE_cod`,1, 1),
                   `CE1` =   case_when(
                     CE1_cod == "1" ~ "Gastos corrientes",
                     CE1_cod == "2" ~ "Gastos de capital",
                     CE1_cod == "3" ~ "Transacciones financieras",
                     CE1_cod == "4" ~ "Sumas sin asignación"
                   ),
                   CE2_cod = substr(sigaf_gasto_actual_mensual$`CE_cod`, 1, 2),
                   CE2 =   case_when(
                     CE2_cod == "11" ~ "Gastos de consumo",
                     CE2_cod == "12" ~ "Intereses",
                     CE2_cod == "13" ~ "Transferencias corrientes",
                     CE2_cod == "21" ~ "Formación de capital",
                     CE2_cod == "22" ~ "Adquisición de activos",
                     CE2_cod == "23" ~ "Transferencias de capital",
                     CE2_cod == "31" ~ "Concesión de préstamos",
                     CE2_cod == "32" ~ "Adquisición de valores",
                     CE2_cod == "33" ~ "Amortización ",
                     CE2_cod == "34" ~ "Otros activos financieros",
                     CE2_cod == "40" ~ "Sumas sin asignación"
                   ),
                   CE3_cod = substr(sigaf_gasto_actual_mensual$`CE_cod`, 1, 3),
                   `CE3` =   case_when(
                     CE3_cod == "111" ~ "Remuneraciones",
                     CE3_cod == "112" ~ "Adquisición de bienes y servicios",
                     CE3_cod == "121" ~ "Intereses internos",
                     CE3_cod == "122" ~ "Intereses externos",
                     CE3_cod == "131" ~ "Transferencias corrientes al sector público",
                     CE3_cod == "132" ~ "Transferencias corrientes al sector privado",
                     CE3_cod == "133" ~ "Transferencias corrientes al sector externo",
                     CE3_cod == "211" ~ "Edificaciones",
                     CE3_cod == "212" ~ "Vías de comunicación",
                     CE3_cod == "213" ~ "Obras urbanísticas",
                     CE3_cod == "214" ~ "Instalaciones",
                     CE3_cod == "215" ~ "Otras obras",
                     CE3_cod == "221" ~ "Maquinaria y equipo",
                     CE3_cod == "222" ~ "Terrenos",
                     CE3_cod == "223" ~ "Edificios",
                     CE3_cod == "224" ~ "Intangibles",
                     CE3_cod == "225" ~ "Activos de valor",
                     CE3_cod == "231" ~ "Transferencias de capital al sector público",
                     CE3_cod == "232" ~ "Transferencias de capital al sector privado",
                     CE3_cod == "233" ~ "Transferencias de capital al sector externo",
                     CE3_cod == "320" ~ "Adquisición de valores",
                     CE3_cod == "331" ~ "Amortización interna",
                     CE3_cod == "332" ~ "Amortización externa",
                     CE3_cod == "340" ~ "Otros activos financieros",
                     CE3_cod == "400" ~ "Sumas sin asignación"
                   ),
                   CE =   case_when(
                     `CE_cod` == "1111" ~ "Sueldos y salarios",
                     `CE_cod` == "1112" ~ "Contribuciones sociales",
                     `CE_cod` == "1120" ~ "Adquisición de bienes y servicios",
                     `CE_cod` == "1210" ~ "Intereses internos",
                     `CE_cod` == "1220" ~ "Intereses externos",
                     `CE_cod` == "1310" ~ "Transferencias corrientes al sector público",
                     `CE_cod` == "1320" ~ "Transferencias corrientes al sector privado",
                     `CE_cod` == "1330" ~ "Transferencias corrientes al sector externo",
                     `CE_cod` == "2110" ~ "Edificaciones",
                     `CE_cod` == "2120" ~ "Vías de comunicación",
                     `CE_cod` == "2130" ~ "Obras urbanísticas",
                     `CE_cod` == "2140" ~ "Instalaciones",
                     `CE_cod` == "2150" ~ "Otras obras",
                     `CE_cod` == "2210" ~ "Maquinaria y equipo",
                     `CE_cod` == "2220" ~ "Terrenos",
                     `CE_cod` == "2230" ~ "Edificios",
                     `CE_cod` == "2240" ~ "Intangibles",
                     `CE_cod` == "2250" ~ "Activos de valor",
                     `CE_cod` == "2310" ~ "Transferencias de capital al sector público",
                     `CE_cod` == "2320" ~ "Transferencias de capital al sector privado",
                     `CE_cod` == "2330" ~ "Transferenciasde capital al sector externo",
                     `CE_cod` == "3100" ~ "Concesión de préstamos",
                     `CE_cod` == "3200" ~ "Adquisición de valores",
                     `CE_cod` == "3310" ~ "Amortización interna",
                     `CE_cod` == "3320" ~ "Amortización externa",
                     `CE_cod` == "3400" ~ "Otros activos financieros",
                     `CE_cod` == "4000" ~ "Sumas sin asignación",
                     `CE_cod` == "4110" ~ "Sumas sin asignación"
                   )
)           
                   
sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,     
                                      
                   ##############################################################
                   #   Creación del clasificador funcional                      #
                   ##############################################################
                   `CF1_cod` = substr(sigaf_gasto_actual_mensual$`CF_cod`, 1, 1),
                   CF1 =   case_when(
                     `CF1_cod` == "1" ~ "Funciones de servicios públicos generales",
                     `CF1_cod` == "2" ~ "Funciones de servicios económicos",
                     `CF1_cod` == "3" ~ "Funciones de servicios sociales",
                     `CF1_cod` == "4" ~ "Transacciones no asociadas a funciones"
                   ),
                   CF2_cod = substr(sigaf_gasto_actual_mensual$`CF_cod`, 1, 2),
                   CF2 =   case_when(
                     CF2_cod == "11" ~ "Servicios públicos generales",
                     CF2_cod == "12" ~ "Defensa",
                     CF2_cod == "13" ~ "Orden público y seguridad",
                     CF2_cod == "21" ~ "Asuntos económicos",
                     CF2_cod == "22" ~ "Protección del medio ambiente",
                     CF2_cod == "31" ~ "Vivienda y otros servicios comunitarios",
                     CF2_cod == "32" ~ "Salud",
                     CF2_cod == "33" ~ "Servicios recreativos, deportivos, de cultura y religión",
                     CF2_cod == "34" ~ "Educación",
                     CF2_cod == "35" ~ "Protección social",
                     CF2_cod == "40" ~ "Transacciones no asociadas a funciones"
                   ),
                   CF3_cod = substr(sigaf_gasto_actual_mensual$`CF_cod`, 1, 3),
                   
                   CF3 =   case_when(
                     CF3_cod == "111" ~ "Asuntos ejecutivos, financieros, fiscales y exteriores",
                     CF3_cod == "112" ~ "Asuntos legislativos",
                     CF3_cod == "113" ~ "Ayuda económica exterior",
                     CF3_cod == "114" ~ "Servicios generales",
                     CF3_cod == "115" ~ "Investigación básica",
                     CF3_cod == "116" ~ "Investigación y desarrollo relacionados con los servicios públicos generales",
                     CF3_cod == "117" ~ "Transacciones de la deuda pública",
                     CF3_cod == "118" ~ "Transferencias de carácter general entre diferentes niveles del sector público",
                     CF3_cod == "119" ~ "Servicios electorales y otros servicios públicos generales no especificados",
                     
                     CF3_cod == "121" ~ "Defensa civil",
                     CF3_cod == "122" ~ "Investigación y desarrollo relacionados con la defensa ",
                     CF3_cod == "123" ~ "Defensa No Especificada",
                     
                     CF3_cod == "131" ~ "Servicios de policía",
                     CF3_cod == "132" ~ "Justicia",
                     CF3_cod == "133" ~ "Centros de reclusión",
                     CF3_cod == "134" ~ "Investigación y desarrollo relacionados con el orden público y la seguridad",
                     CF3_cod == "135" ~ "Protección contra incendios y otros eventos",
                     CF3_cod == "136" ~ "Orden público y la seguridad no especificada",
                     
                     CF3_cod == "211" ~ "Asuntos económicos, comerciales y laborales en general",
                     CF3_cod == "212" ~ "Agricultura, ganadería, silvicultura, pesca y caza",
                     CF3_cod == "213" ~ "Combustibles y energía",
                     CF3_cod == "214" ~ "Minería, manufacturas y construcción",
                     CF3_cod == "215" ~ "Transporte",
                     CF3_cod == "216" ~ "Comunicaciones",
                     CF3_cod == "217" ~ "Turismo y otras industrias",
                     CF3_cod == "218" ~ "Investigación y desarrollo relacionados con asuntos económicos",
                     CF3_cod == "219" ~ "Asuntos económicos no especificados",
                     
                     
                     CF3_cod == "221" ~ "Disposición de desechos",
                     CF3_cod == "222" ~ "Disposición de aguas residuales",
                     CF3_cod == "223" ~ "Reducción de la contaminación",
                     CF3_cod == "224" ~ "Protección de la diversidad biológica y del paisaje",
                     CF3_cod == "225" ~ "Investigación y desarrollo relacionados con la protección del medio ambiente",
                     CF3_cod == "226" ~ "Protección del medio ambiente no especificados",
                     
                     CF3_cod == "311" ~ "Urbanización",
                     CF3_cod == "312" ~ "Desarrollo comunitario",
                     CF3_cod == "313" ~ "Abastecimiento de Agua",
                     CF3_cod == "314" ~ "Alumbrado público",
                     CF3_cod == "315" ~ "Investigación y desarrollo relacionados con la vivienda y los servicios comunitarios",
                     CF3_cod == "316" ~ "Vivienda y servicios comunitarios no especificados",
                     
                     CF3_cod == "321" ~ "Servicios para pacientes externos",
                     CF3_cod == "322" ~ "Servicios hospitalarios",
                     CF3_cod == "323" ~ "Servicios de salud pública",
                     CF3_cod == "324" ~ "Investigación y desarrollo relacionados con la salud",
                     CF3_cod == "325" ~ "Servicios de salud no especificados",
                     
                     CF3_cod == "331" ~ "Servicios recreativos y deportivos",
                     CF3_cod == "332" ~ "Servicios culturales",
                     CF3_cod == "333" ~ "Servicios editoriales, de radio y televisión",
                     CF3_cod == "334" ~ "Servicios religiosos y otros servicios comunitarios",
                     CF3_cod == "335" ~ "Investigación y desarrollo relacionados con el esparcimiento, el deporte, la cultura y la religión",
                     CF3_cod == "336" ~ "Servicios recreativos, deportivos, de cultura y religión no especificados",
                     
                     CF3_cod == "341" ~ "Enseñanza materno infantil, preescolar y primaria",
                     CF3_cod == "342" ~ "Enseñanza secundaria",
                     CF3_cod == "343" ~ "Enseñanza post secundaria no terciaria o parauniversitaria",
                     CF3_cod == "344" ~ "Enseñanza terciaria o universitaria",
                     CF3_cod == "345" ~ "Enseñanza no atribuible a ningún nivel",
                     CF3_cod == "346" ~ "Servicios auxiliares de la educación",
                     CF3_cod == "347" ~ "Investigación y desarrollo relacionados con la educación",
                     CF3_cod == "348" ~ "Enseñanza no especificada",
                     
                     CF3_cod == "351" ~ "Beneficios por enfermedad y maternidad ",
                     CF3_cod == "352" ~ "Pensiones",
                     CF3_cod == "353" ~ "Ayuda a familias",
                     CF3_cod == "354" ~ "Prestaciones laborales",
                     CF3_cod == "355" ~ "Exclusión social no especificada",
                     CF3_cod == "356" ~ "Investigación y desarrollo relacionados con la protección social",
                     CF3_cod == "357" ~ "Protección social no especificada",
                     
                     CF3_cod == "400" ~ "Transacciones no asociadas a funciones"
                   ),
                   `CF` =   case_when(
                     `CF_cod` == "1111" ~ "Asuntos ejecutivos",
                     `CF_cod` == "1112" ~ "Asuntos financieros y fiscales",
                     `CF_cod` == "1113" ~ "Asuntos exteriores",
                     
                     `CF_cod` == "1120" ~ "Asuntos legislativos",
                     `CF_cod` == "1130" ~ "Ayuda económica exterior",
                     `CF_cod` == "1132" ~ "Ayuda económica exterior",
                     `CF_cod` == "1141" ~ "Servicios generales de personal",
                     `CF_cod` == "1142" ~ "Servicios generales de planificación y estadística",
                     `CF_cod` == "1143" ~ "Otros servicios generales",
                     `CF_cod` == "1150" ~ "Investigación básica",
                     `CF_cod` == "1160" ~ "Investigación y desarrollo relacionados con los servicios públicos generales",
                     `CF_cod` == "1170" ~ "Transacciones de la deuda pública",
                     `CF_cod` == "1180" ~ "Transferencias de carácter general entre diferentes niveles del sector público",
                     `CF_cod` == "1190" ~ "Servicios electorales y otros servicios públicos generales no especificados",
                     
                     `CF_cod` == "1210" ~ "Defensa civil",
                     `CF_cod` == "1220" ~ "Investigación y desarrollo relacionados con la defensa",
                     `CF_cod` == "1230" ~ "Defensa no especificada",
                     
                     `CF_cod` == "1310" ~ "Servicios de policía",
                     `CF_cod` == "1320" ~ "Justicia",
                     `CF_cod` == "1330" ~ "Centros de reclusión",
                     `CF_cod` == "1340" ~ "Investigación y desarrollo relacionados con el orden público y la seguridad",
                     `CF_cod` == "1350" ~ "Protección contra incendios y otros eventos",
                     `CF_cod` == "1360" ~ "Orden público y seguridad no especificada",
                     
                     `CF_cod` == "2111" ~ "Asuntos económicos y comerciales en general",
                     `CF_cod` == "2112" ~ "Asuntos laborales en general",
                     
                     `CF_cod` == "2121" ~ "Agricultura y ganadería",
                     `CF_cod` == "2122" ~ "Silvicultura",
                     `CF_cod` == "2123" ~ "Pesca y caza",
                     
                     `CF_cod` == "2131" ~ "Energía eléctrica",
                     `CF_cod` == "2132" ~ "Petróleo y gas natural",
                     `CF_cod` == "2133" ~ "Energía para otros usos",
                     `CF_cod` == "2134" ~ "Otros combustibles",
                     
                     `CF_cod` == "2141" ~ "Extracción de recursos minerales excepto los combustibles minerales",
                     `CF_cod` == "2142" ~ "Manufacturas",
                     `CF_cod` == "2143" ~ "Construcción",
                     
                     `CF_cod` == "2151" ~ "Transporte por carretera",
                     `CF_cod` == "2152" ~ "Transporte aéreo ",
                     `CF_cod` == "2153" ~ "Transporte marítimo y fluvial",
                     `CF_cod` == "2154" ~ "Transporte ferroviario",
                     `CF_cod` == "2155" ~ "Otros sistemas de transporte",
                     `CF_cod` == "2156" ~ "Otros asuntos de transporte no especificados",
                     
                     `CF_cod` == "2161" ~ "Comunicaciones",
                     
                     `CF_cod` == "2171" ~ "Turismo",
                     `CF_cod` == "2172" ~ "Otras industrias",
                     
                     `CF_cod` == "2181" ~ "Investigación y desarrollo relacionados con asuntos económicos, comerciales y laborales en general",
                     `CF_cod` == "2182" ~ "Investigación y desarrollo relacionados con agricultura, ganadería, silvicultura, pesca y caza",
                     `CF_cod` == "2183" ~ "Investigación y desarrollo relacionados con combustibles y energía",
                     `CF_cod` == "2184" ~ "Investigación y desarrollo relacionados con minería, manufacturas y construcción",
                     `CF_cod` == "2185" ~ "Investigación y desarrollo relacionados con el transporte",
                     `CF_cod` == "2186" ~ "Investigación y desarrollo relacionados con las comunicaciones",
                     `CF_cod` == "2187" ~ "Investigación y desarrollo relacionados con otras industrias",
                     
                     `CF_cod` == "2190" ~ "Asuntos económicos no especificados",
                     
                     `CF_cod` == "2210" ~ "Disposición de desechos",
                     `CF_cod` == "2220" ~ "Disposición de aguas residuales",
                     `CF_cod` == "2230" ~ "Reducción de la contaminación",
                     `CF_cod` == "2240" ~ "Protección de la diversidad biológica y el paisaje",
                     `CF_cod` == "2250" ~ "Investigación y desarrollo relacionados con la protección del medio ambiente",
                     `CF_cod` == "2260" ~ "Protección del medio ambiente no especificados",
                     
                     `CF_cod` == "3110" ~ "Urbanización",
                     `CF_cod` == "3120" ~ "Desarrollo comunitario",
                     `CF_cod` == "3130" ~ "Abastecimiento de agua",
                     `CF_cod` == "3140" ~ "Alumbrado público",
                     `CF_cod` == "3150" ~ "Investigación y desarrollo relacionados con la vivienda y los servicios comunitarios ",
                     `CF_cod` == "3160" ~ "Vivienda y servicios comunitarios no especificados",
                     
                     `CF_cod` == "3210" ~ "Servicios para pacientes externos",
                     `CF_cod` == "3220" ~ "Servicios hospitalarios",
                     `CF_cod` == "3230" ~ "Servicios de salud pública",
                     `CF_cod` == "3240" ~ "Investigación y desarrollo relacionados con la salud",
                     `CF_cod` == "3250" ~ "Servicios de salud no especificados",
                     
                     `CF_cod` == "3310" ~ "Servicios recreativos y deportivos",
                     `CF_cod` == "3320" ~"Servicios culturales", 
                     `CF_cod` == "3330" ~"Servicios editoriales, de radio y televisión",
                     `CF_cod` == "3340" ~"Servicios religiosos y otros servicios comunitarios",
                     `CF_cod` == "3350" ~"Investigación y desarrollo relacionados con el esparcimiento, el deporte, la cultura y la religión",
                     `CF_cod` == "3360" ~"Servicios recreativos, deportivos, de cultura y religión no especificados",
                     
                     `CF_cod` == "3410" ~ "Enseñanza materno infantil, preescolar y primaria",
                     `CF_cod` == "3411" ~ "Enseñanza materna infantil y preescolar",
                     `CF_cod` == "3412" ~ "Enseñanza primaria",
                     
                     `CF_cod` == "3420" ~ "Enseñanza secundaria",
                     `CF_cod` == "3421" ~ "Enseñanza secundaria básica",
                     `CF_cod` == "3422" ~ "Enseñanza secundaria avanzada",
                     
                     `CF_cod` == "3430" ~ "Enseñanza postsecundaria no terciaria o parauniversitaria",
                     `CF_cod` == "3440" ~ "Enseñanza terciaria o universitaria",
                     `CF_cod` == "3441" ~ "Enseñanza terciaria o universitaria",
                     
                     `CF_cod` == "3450" ~ "Enseñanza no atribuible a ningún nivel",
                     `CF_cod` == "3460" ~ "Servicios auxiliares de la educación",
                     `CF_cod` == "3470" ~ "Investigación y desarrollo relacionados con la educación",
                     `CF_cod` == "3480" ~ "Enseñanza no especificada",
                     
                     
                     `CF_cod` == "3510" ~ "Beneficios por enfermedad y maternidad",
                     `CF_cod` == "3520" ~ "Pensiones",
                     `CF_cod` == "3521" ~ "Pensión por invalidez",
                     `CF_cod` == "3522" ~ "Pensión por vejez",
                     `CF_cod` == "3523" ~ "Pensión por muerte",
                     `CF_cod` == "3524" ~ "Otras pensiones",
                     `CF_cod` == "3530" ~ "Ayuda a familias",
                     `CF_cod` == "3540" ~ "Prestaciones laborales",
                     `CF_cod` == "3550" ~ "Exclusión social no especificada",
                     `CF_cod` == "3560" ~ "Investigación y desarrollo relacionados con la protección social",
                     `CF_cod` == "3570" ~ "Protección social no especificada",
                     
                     `CF_cod` == "4000" ~ "Transacciones no asociadas a funciones"
                     
                   )
)

sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,

                   #############################################################################
                   #  Creación del clasificador COG: Partida, Subpartida y ObjetoGasto    #
                   #############################################################################
                   
                  Partida_cod = as.numeric(sigaf_gasto_actual_mensual$Partida_cod),
                   
                   Partida = case_when(
                     Partida_cod == 0 ~ "Remuneraciones", 
                     Partida_cod == 1  ~ "Servicios",
                     Partida_cod == 2  ~ "Materiales y suministros",
                     Partida_cod == 3  ~ "Intereses y comisiones",
                     Partida_cod == 4  ~ "Activos financieros",
                     Partida_cod == 5  ~ "Bienes duraderos",
                     Partida_cod == 6  ~ "Transferencias corrientes",
                     Partida_cod == 7  ~ "Transferencias de capital",
                     Partida_cod == 8  ~ "Amortización",
                     Partida_cod == 9  ~ "Cuentas especiales"
                   ),
                   
                   `Subpartida cod` = as.numeric(sigaf_gasto_actual_mensual$`Subpartida cod`) ,
                   
                   Subpartida = case_when(
                     `Subpartida cod` == 1   ~ "Remuneraciones básicas",
                     `Subpartida cod` == 2   ~ "Remuneraciones eventuales",
                     `Subpartida cod` == 3   ~ "Incentivos salariales",
                     `Subpartida cod` == 4   ~ "Contribuciones patronales al desarrollo y seguridad social",
                     `Subpartida cod` == 5   ~ "Contribuciones patronales a fondos de pensiones  y otros fondos de capitalización",
                     `Subpartida cod` == 99  ~ "Remuneraciones diversas",
                     `Subpartida cod` == 101  ~ "Alquileres",
                     `Subpartida cod` == 102  ~ "Servicios básicos",
                     `Subpartida cod` == 103  ~ "Servicios comerciales y financieros",
                     `Subpartida cod` == 104   ~ "Servicios de gestión y apoyo",
                     `Subpartida cod` == 105   ~ "Gastos de viaje y transporte",
                     `Subpartida cod` == 106   ~ "Seguros, reaseguros y otras obligaciones",
                     `Subpartida cod` == 107   ~ "Capacitación y protocolo",
                     `Subpartida cod` == 108   ~ "Mantenimiento y reparación",
                     `Subpartida cod` == 109   ~ "Impuestos",
                     `Subpartida cod` == 199   ~ "Servicios diversos",
                     `Subpartida cod` == 201   ~ "Productos químicos y conexos",
                     `Subpartida cod` == 202   ~ "Alimentos y productos agropecuarios",
                     `Subpartida cod` == 203   ~ "Materiales y productos de uso en la construcción y mantenimiento",
                     `Subpartida cod` == 204   ~ "Herramientas, repuestos y accesorios",
                     `Subpartida cod` == 205   ~ "Bienes para la producción y comercialización",
                     `Subpartida cod` == 299   ~ "Útiles, materiales y suministros diversos",
                     `Subpartida cod` == 301   ~ "Intereses sobre títulos valores",
                     `Subpartida cod` == 302   ~ "Intereses sobre préstamos",
                     `Subpartida cod` == 303   ~ "Intereses sobre otras obligaciones",
                     `Subpartida cod` == 304   ~ "Comisiones y otros gastos",
                     `Subpartida cod` == 401   ~ "Préstamos",
                     `Subpartida cod` == 402   ~ "Adquisición de valores",
                     `Subpartida cod` == 499   ~ "Otros activos financieros",
                     `Subpartida cod` == 501   ~ "Maquinaria, equipo y mobiliario",
                     `Subpartida cod` == 502   ~ "Construcciones, adiciones y mejoras",
                     `Subpartida cod` == 503   ~ "Bienes preexistentes",
                     `Subpartida cod` == 599   ~ "Bienes duraderos diversos",
                     `Subpartida cod` == 601   ~ "Transfererencias corrientes al sector público",
                     `Subpartida cod` == 602   ~ "Transferencias corrientes a personas",
                     `Subpartida cod` == 603   ~ "Prestaciones",
                     `Subpartida cod` == 604   ~ "Transferencias corrientes a empresas privadas sin fines de lucro",
                     `Subpartida cod` == 605   ~ "Transferencias corrientes a empresas privadas ",
                     `Subpartida cod` == 606   ~ "Otras transferencias corrientes al sector privado",
                     `Subpartida cod` == 607   ~ "Transferencias corrientes al sector externo",
                     `Subpartida cod` == 701   ~ "Transferencias de capital al sector público",
                     `Subpartida cod` == 702   ~ "Transferencias de capital a personas",
                     `Subpartida cod` == 703   ~ "Transferencias de capital a entidades privadas sin fines de lucro",
                     `Subpartida cod` == 704   ~ "Transferencias de capital a empresas privadas",
                     `Subpartida cod` == 705   ~ "Transferencias de capital al sector externo",
                     `Subpartida cod` == 801   ~ "Amortización de títulos valores",
                     `Subpartida cod` == 802   ~ "Amortización de préstamos",
                     `Subpartida cod` == 803   ~ "Amortización de otras obligaciones",
                     `Subpartida cod` == 901   ~ "Cuentas especiales diversas",
                     `Subpartida cod` == 902   ~ "Sumas sin asignación presupuestaria"
                   ), 
                   
                   ObjetoGasto_cod = as.numeric(sigaf_gasto_actual_mensual$ObjetoGasto_cod),
                   
                   `ObjetoGasto` = case_when(
                     ObjetoGasto_cod == 101   ~ "Sueldos para cargos fijos ",
                     ObjetoGasto_cod == 102   ~ "Jornales",
                     ObjetoGasto_cod == 103   ~ "Servicios especiales",
                     ObjetoGasto_cod == 104   ~ "Sueldos a base de comisión",
                     ObjetoGasto_cod == 105   ~ "Suplencias ",
                     ObjetoGasto_cod == 201   ~ "Tiempo extraordinario",
                     ObjetoGasto_cod == 202    ~ "Recargo de funciones",
                     ObjetoGasto_cod == 203   ~ "Disponibilidad laboral",
                     ObjetoGasto_cod == 204   ~ "Compensación de vacaciones",
                     ObjetoGasto_cod == 205   ~ "Dietas",
                     ObjetoGasto_cod == 301   ~ "Retribución por años servidos",
                     ObjetoGasto_cod == 302   ~ "Restricción al ejercicio liberal de la profesión",
                     ObjetoGasto_cod == 303   ~ "Decimotercer mes",
                     ObjetoGasto_cod == 304   ~ "Salario escolar",
                     ObjetoGasto_cod == 399   ~ "Otros incentivos salariales",
                     ObjetoGasto_cod == 401   ~ "Contribución patronal al seguro de salud de la Caja Costarricense de Seguro Social",
                     ObjetoGasto_cod == 402  ~ "Contribución patronal al Instituto Mixto de Ayuda Social ",
                     ObjetoGasto_cod == 403   ~ "Contribución patronal al Instituto Nacional de Aprendizaje",
                     ObjetoGasto_cod == 404   ~ "Contribución patronal al Fondo de Desarrollo Social y Asignaciones Familiares",
                     ObjetoGasto_cod == 405   ~ "Contribución patronal al Banco Popular y de Desarrollo Comunal",
                     ObjetoGasto_cod == 501    ~ "Contribución patronal al seguro de pensiones de la Caja Costarricense de Seguro Social  ",
                     ObjetoGasto_cod == 502   ~ "Aporte patronal al Régimen Obligatorio de Pensiones Complementarias ",
                     ObjetoGasto_cod == 503   ~ "Aporte patronal al Fondo de Capitalización Laboral ",
                     ObjetoGasto_cod == 504   ~ "Contribución patronal a otros fondos administrados por entes públicos",
                     ObjetoGasto_cod == 505   ~ "Contribución patronal a otros fondos administrados por entes privados",
                     ObjetoGasto_cod == 9901   ~ "Gastos de representación personal",
                     ObjetoGasto_cod == 9999    ~ "Otras remuneraciones",
                     
                     ObjetoGasto_cod == 10101    ~ "Alquiler de edificios, locales y terrenos",
                     ObjetoGasto_cod == 10102    ~ "Alquiler de maquinaria, equipo y mobiliario",
                     ObjetoGasto_cod == 10103    ~ "Alquiler de equipo de cómputo",
                     ObjetoGasto_cod == 10104    ~ "Alquiler de equipo y derechos para telecomunicaciones",
                     ObjetoGasto_cod == 10199    ~ "Otros alquileres",
                     
                     ObjetoGasto_cod == 10201    ~ "Servicio de agua y alcantarillado ",
                     ObjetoGasto_cod == 10202    ~ "Servicio de energía eléctrica",
                     ObjetoGasto_cod == 10203    ~ "Servicio de correo",
                     ObjetoGasto_cod == 10204    ~ "Servicio de telecomunicaciones",
                     ObjetoGasto_cod == 10299    ~ "Otros servicios básicos ",
                     
                     ObjetoGasto_cod == 10301    ~ "Información",
                     ObjetoGasto_cod == 10302    ~ "Publicidad y propaganda",
                     ObjetoGasto_cod == 10303    ~ "Impresión, encuadernación y otros",
                     ObjetoGasto_cod == 10304    ~ "Transporte de bienes",
                     ObjetoGasto_cod == 10305    ~ "Servicios aduaneros",
                     ObjetoGasto_cod == 10306    ~ "Comisiones y gastos por servicios financieros y comerciales",
                     ObjetoGasto_cod == 10307    ~ "Servicios de tecnologías de información",
                     
                     ObjetoGasto_cod == 10401    ~ "Servicios en ciencias de la salud",
                     ObjetoGasto_cod == 10402    ~ "Servicios jurídicos ",
                     ObjetoGasto_cod == 10403    ~ "Servicios de ingeniería y arquitectura",
                     ObjetoGasto_cod == 10404    ~ "Servicios en ciencias económicas y sociales",
                     ObjetoGasto_cod == 10405    ~ "Servicios informáticos",
                     ObjetoGasto_cod == 10406    ~ "Servicios generales ",
                     ObjetoGasto_cod == 10499    ~ "Otros servicios de gestión y apoyo",
                     
                     ObjetoGasto_cod == 10501    ~ "Transporte dentro del país",
                     ObjetoGasto_cod == 10502    ~ "Viáticos dentro del país",
                     ObjetoGasto_cod == 10503    ~ "Transporte en el exterior",
                     ObjetoGasto_cod == 10504    ~ "Viáticos en el exterior",
                     
                     ObjetoGasto_cod == 10601    ~ "Seguros ",
                     ObjetoGasto_cod == 10602    ~ "Reaseguros ",
                     ObjetoGasto_cod == 10603    ~ "Obligaciones por contratos de seguros",
                     
                     ObjetoGasto_cod == 10701    ~ "Actividades de capacitación",
                     ObjetoGasto_cod == 10702    ~ "Actividades protocolarias y sociales ",
                     ObjetoGasto_cod == 10703    ~ "Gastos de representación institucional",
                     
                     ObjetoGasto_cod == 10801    ~ "Mantenimiento de edificios, locales y terrenos",
                     ObjetoGasto_cod == 10802    ~ "Mantenimiento de vías de comunicación",
                     ObjetoGasto_cod == 10803    ~ "Mantenimiento de instalaciones y otras obras",
                     ObjetoGasto_cod == 10804    ~ "Mantenimiento y reparación de maquinaria y equipo de producción",
                     ObjetoGasto_cod == 10805    ~ "Mantenimiento y reparación de equipo de transporte",
                     ObjetoGasto_cod == 10806    ~ "Mantenimiento y reparación de equipo de comunicación",
                     ObjetoGasto_cod == 10807    ~ "Mantenimiento y reparación de equipo y mobiliario de oficina",
                     ObjetoGasto_cod == 10808    ~ "Mantenimiento y reparación de equipo de cómputo y sistemas de información",
                     ObjetoGasto_cod == 10899    ~ "Mantenimiento y reparación de otros equipos",
                     
                     ObjetoGasto_cod == 10901    ~ "Impuestos sobre ingresos y utilidades",
                     ObjetoGasto_cod == 10902    ~ "Impuestos sobre la propiedad bienes inmuebles ",
                     ObjetoGasto_cod == 10903    ~ "Impuestos de patentes",
                     ObjetoGasto_cod == 10999    ~ "Otros impuestos",
                     
                     ObjetoGasto_cod == 19901    ~ "Servicios de regulación",
                     ObjetoGasto_cod == 19902    ~ "Intereses moratorios y multas",
                     ObjetoGasto_cod == 19903    ~ "Gastos de oficinas en el exterior",
                     ObjetoGasto_cod == 19904    ~ "Gastos de misiones especiales en el exterior",
                     ObjetoGasto_cod == 19905    ~ "Deducibles",
                     ObjetoGasto_cod == 19999    ~ "Otros servicios no especificados",
                     
                     ObjetoGasto_cod == 20101    ~ "Combustibles y lubricantes",
                     ObjetoGasto_cod == 20102    ~ "Productos farmacéuticos y medicinales",
                     ObjetoGasto_cod == 20103    ~ "Productos veterinarios",
                     ObjetoGasto_cod == 20104    ~ "Tintas, pinturas y diluyentes ",
                     ObjetoGasto_cod == 20199    ~ "Otros productos químicos y conexos",
                     
                     ObjetoGasto_cod == 20201    ~ "Productos pecuarios y otras especies",
                     ObjetoGasto_cod == 20202    ~ "Productos agroforestales",
                     ObjetoGasto_cod == 20203    ~ "Alimentos y bebidas",
                     ObjetoGasto_cod == 20204    ~ "Alimentos para animales",
                     
                     ObjetoGasto_cod == 20301    ~ "Materiales y productos metálicos",
                     ObjetoGasto_cod == 20302    ~ "Materiales y productos minerales y asfálticos",
                     ObjetoGasto_cod == 20303    ~ "Madera y sus derivados",
                     ObjetoGasto_cod == 20304    ~ "Materiales y productos eléctricos, telefónicos y de cómputo",
                     ObjetoGasto_cod == 20305    ~ "Materiales y productos de vidrio",
                     ObjetoGasto_cod == 20306    ~ "Materiales y productos de plástico",
                     ObjetoGasto_cod == 20399    ~ "Otros materiales y productos de uso en la construcción y mantenimiento",
                     
                     ObjetoGasto_cod == 20401    ~ "Herramientas e instrumentos",
                     ObjetoGasto_cod == 20402    ~ "Repuestos y accesorios",
                     
                     ObjetoGasto_cod == 20501    ~ "Materia prima",
                     ObjetoGasto_cod == 20502    ~ "Productos terminado",
                     ObjetoGasto_cod == 20503    ~ "Energía eléctrica",
                     ObjetoGasto_cod == 20599    ~ "Otros bienes para la producción y comercialización",
                     
                     ObjetoGasto_cod == 29901    ~ "Útiles y materiales de oficina y cómputo",
                     ObjetoGasto_cod == 29902    ~ "Útiles y materiales médico, hospitalario y de investigación",
                     ObjetoGasto_cod == 29903    ~ "Productos de papel, cartón e impresos",
                     ObjetoGasto_cod == 29904    ~ "Textiles y vestuario",
                     ObjetoGasto_cod == 29905    ~ "Útiles y materiales de limpieza",
                     ObjetoGasto_cod == 29906    ~ "Útiles y materiales de resguardo y seguridad",
                     ObjetoGasto_cod == 29907    ~ "Útiles y materiales de cocina y comedor",
                     ObjetoGasto_cod == 29999    ~ "Otros útiles, materiales y suministros diversos",
                     
                     ObjetoGasto_cod == 30101    ~ "Intereses sobre títulos valores internos de corto plazo",
                     ObjetoGasto_cod == 30102    ~ "Intereses sobre títulos valores internos de largo plazo",
                     ObjetoGasto_cod == 30103    ~ "Intereses sobre títulos valores del sector externo de corto plazo",
                     ObjetoGasto_cod == 30104    ~ "Intereses sobre títulos valores del sector externo de largo plazo",
                     
                     ObjetoGasto_cod == 30201    ~ "Intereses sobre préstamos del Gobierno Central ",
                     ObjetoGasto_cod == 30202    ~ "Intereses sobre préstamos de Órganos Desconcentrados",
                     ObjetoGasto_cod == 30203    ~ "Intereses sobre préstamos de Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 30204    ~ "Intereses sobre préstamos de Gobiernos Locales",
                     ObjetoGasto_cod == 30205    ~ "Intereses sobre préstamos de Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 30206    ~ "Intereses sobre préstamos de Instituciones Públicas Financieras ",
                     ObjetoGasto_cod == 30207    ~ "Intereses sobre préstamos del sector privado",
                     ObjetoGasto_cod == 30208    ~ "Intereses sobre préstamos del sector externo",
                     
                     ObjetoGasto_cod == 30301    ~ "Intereses sobre depósitos bancarios a la vista",
                     ObjetoGasto_cod == 30399    ~ "Intereses sobre otras obligaciones",
                     
                     ObjetoGasto_cod == 30401    ~ "Comisiones y otros gastos sobre títulos valores internos",
                     ObjetoGasto_cod == 30402    ~ "Comisiones y otros gastos sobre títulos valores del sector externo",
                     ObjetoGasto_cod == 30403    ~ "Comisiones y otros gastos sobre préstamos internos",
                     ObjetoGasto_cod == 30404    ~ "Comisiones y otros gastos sobre préstamos del sector externo",
                     ObjetoGasto_cod == 30405    ~ "Diferencias por tipo de cambio",
                     
                     ObjetoGasto_cod == 40101    ~ "Préstamos al Gobierno Central",
                     ObjetoGasto_cod == 40102    ~ "Préstamos a Órganos Desconcentrados",
                     ObjetoGasto_cod == 40103    ~ "Préstamos a Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 40104    ~ "Préstamos a Gobiernos Locales",
                     ObjetoGasto_cod == 40105    ~ "Préstamos a Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 40106    ~ "Préstamos a Instituciones Públicas Financieras",
                     ObjetoGasto_cod == 40107    ~ "Préstamos al sector privado",
                     ObjetoGasto_cod == 40108    ~ "Préstamos al sector externo ",
                     
                     ObjetoGasto_cod == 40201    ~ "Adquisición de valores del Gobierno Central",
                     ObjetoGasto_cod == 40202    ~ "Adquisición de valores de Órganos Desconcentrados",
                     ObjetoGasto_cod == 40203    ~ "Adquisición de valores de Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 40204    ~ "Adquisición de valores de Gobiernos Locales",
                     ObjetoGasto_cod == 40205    ~ "Adquisición de valores de Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 40206    ~ "Adquisición de valores de Instituciones Públicas Financieras ",
                     ObjetoGasto_cod == 40207    ~ "Adquisición de valores del sector privado",
                     ObjetoGasto_cod == 40208    ~ "Adquisición de valores del sector externo",
                     
                     ObjetoGasto_cod == 49901    ~ "Aportes de capital a empresas",
                     ObjetoGasto_cod == 49999    ~ "Otros activos financieros",
                     
                     ObjetoGasto_cod == 50101    ~ "Maquinaria y equipo para la producción",
                     ObjetoGasto_cod == 50102    ~ "Equipo de transporte",
                     ObjetoGasto_cod == 50103    ~ "Equipo de comunicación",
                     ObjetoGasto_cod == 50104    ~ "Equipo y mobiliario de oficina",
                     ObjetoGasto_cod == 50105    ~ "Equipo de cómputo",
                     ObjetoGasto_cod == 50106    ~ "Equipo sanitario, de laboratorio e investigación",
                     ObjetoGasto_cod == 50107    ~ "Equipo y mobiliario educacional, deportivo y recreativo",
                     ObjetoGasto_cod == 50199    ~ "Maquinaria, equipo y mobiliario diverso",
                     
                     ObjetoGasto_cod == 50201    ~ "Edificios",
                     ObjetoGasto_cod == 50202    ~ "Vías de comunicación terrestre",
                     ObjetoGasto_cod == 50203    ~ "Vías férreas",
                     ObjetoGasto_cod == 50204    ~ "Obras marítimas y fluviales",
                     ObjetoGasto_cod == 50205    ~ "Aeropuertos",
                     ObjetoGasto_cod == 50206    ~ "Obras urbanísticas",
                     ObjetoGasto_cod == 50207    ~ "Instalaciones",
                     ObjetoGasto_cod == 50299    ~ "Otras construcciones, adiciones y mejoras",
                     
                     ObjetoGasto_cod == 50301    ~ "Terrenos",
                     ObjetoGasto_cod == 50302    ~ "Edificios preexistentes",
                     ObjetoGasto_cod == 50399    ~ "Otras obras preexistentes",
                     
                     ObjetoGasto_cod == 59901    ~ "Semovientes",
                     ObjetoGasto_cod == 59902    ~ "Piezas y obras de colección",
                     ObjetoGasto_cod == 59903    ~ "Bienes intangibles",
                     ObjetoGasto_cod == 59999    ~ "Otros bienes duraderos",
                     
                     ObjetoGasto_cod == 60101    ~ "Transferencias corrientes al Gobierno Central",
                     ObjetoGasto_cod == 60102    ~ "Transferencias corrientes a Órganos Desconcentrados",
                     ObjetoGasto_cod == 60103    ~ "Transferencias corrientes a Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 60104    ~ "Transferencias corrientes a Gobiernos Locales",
                     ObjetoGasto_cod == 60105    ~ "Transferencias corrientes a Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 60106    ~ "Transferencias corrientes a Instituciones Públicas Financieras ",
                     ObjetoGasto_cod == 60107    ~ "Dividendos",
                     ObjetoGasto_cod == 60108    ~ "Fondos en fideicomiso para gasto corriente",
                     ObjetoGasto_cod == 60109    ~ "Impuestos por transferir",
                     
                     ObjetoGasto_cod == 60201    ~ "Becas a funcionarios",
                     ObjetoGasto_cod == 60202    ~ "Becas a terceras personas",
                     ObjetoGasto_cod == 60203    ~ "Ayudas a funcionarios ",
                     ObjetoGasto_cod == 60299    ~ "Otras transferencias a personas",
                     
                     ObjetoGasto_cod == 60301    ~ "Prestaciones legales",
                     ObjetoGasto_cod == 60302    ~ "Pensiones y jubilaciones contributivas ",
                     ObjetoGasto_cod == 60303    ~ "Pensiones no contributivas ",
                     ObjetoGasto_cod == 60304    ~ "Decimotercer mes de jubilaciones y pensiones",
                     ObjetoGasto_cod == 60399    ~ "Otras prestaciones",
                     
                     ObjetoGasto_cod == 60401    ~ "Transferencias corrientes a asociaciones",
                     ObjetoGasto_cod == 60402    ~ "Transferencias corrientes a fundaciones",
                     ObjetoGasto_cod == 60403    ~ "Transferencias corrientes a cooperativas",
                     ObjetoGasto_cod == 60404    ~ "Transferencias corrientes a otras entidades privadas sin fines de lucro",
                     
                     ObjetoGasto_cod == 60501    ~ "Transferencias corrientes a empresas privadas",
                     
                     ObjetoGasto_cod == 60601    ~ "Indemnizaciones",
                     ObjetoGasto_cod == 60602    ~ "Reintegros o devoluciones",
                     
                     ObjetoGasto_cod == 60701    ~ "Transferencias corrientes a organismos internacionales",
                     ObjetoGasto_cod == 60702    ~ "Otras transferencias corrientes al sector externo",
                     
                     ObjetoGasto_cod == 70101    ~ "Transferencias de capital al Gobierno Central",
                     ObjetoGasto_cod == 70102    ~ "Transferencias de capital a Órganos Desconcentrados",
                     ObjetoGasto_cod == 70103    ~ "Transferencias de capital a Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 70104    ~ "Transferencias de capital a Gobiernos Locales",
                     ObjetoGasto_cod == 70105    ~ "Transferencias de capital a Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 70106    ~ "Transferencias de capital a Instituciones Públicas Financieras",
                     ObjetoGasto_cod == 70107    ~ "Fondos en fideicomiso para gasto de capital",
                     
                     ObjetoGasto_cod == 70201    ~ "Transferencias de capital a personas",
                     
                     ObjetoGasto_cod == 70301    ~ "Transferencias de capital a asociaciones",
                     ObjetoGasto_cod == 70302    ~ "Transferencias de capital a fundaciones ",
                     ObjetoGasto_cod == 70303    ~ "Transferencias de capital a cooperativas",
                     ObjetoGasto_cod == 70399    ~ "Transferencias de capital a otras entidades privadas sin fines de lucro",
                     
                     ObjetoGasto_cod == 70401    ~ "Transferencias de capital a empresas privadas",
                     
                     ObjetoGasto_cod == 70501    ~ "Transferencias de capital a organismos internacionales",
                     ObjetoGasto_cod == 70502    ~ "Otras transferencias de capital al sector externo",
                     
                     ObjetoGasto_cod == 80101    ~ "Amortización de títulos valores internos de corto plazo",
                     ObjetoGasto_cod == 80102    ~ "Amortización de títulos valores internos de largo plazo",
                     ObjetoGasto_cod == 80103    ~ "Amortización de títulos valores del sector externo de corto plazo",
                     ObjetoGasto_cod == 80104    ~ "Amortización de títulos valores del sector externo de largo plazo",
                     
                     ObjetoGasto_cod == 80201    ~ "Amortización de préstamos del Gobierno Central",
                     ObjetoGasto_cod == 80202    ~ "Amortización de préstamos de Órganos Desconcentrados",
                     ObjetoGasto_cod == 80203    ~ "Amortización de préstamos de Instituciones Descentralizadas no Empresariales",
                     ObjetoGasto_cod == 80204    ~ "Amortización de préstamos de Gobiernos Locales",
                     ObjetoGasto_cod == 80205    ~ "Amortización de préstamos de Empresas Públicas no Financieras",
                     ObjetoGasto_cod == 80206    ~ "Amortización de préstamos de Instituciones Públicas Financieras",
                     ObjetoGasto_cod == 80207    ~ "Amortización de préstamos del sector privado",
                     ObjetoGasto_cod == 80208    ~ "Amortización de préstamos de sector externo",
                     
                     ObjetoGasto_cod == 80301    ~ "Amortización de otras obligaciones ",
                     ObjetoGasto_cod == 90101    ~ "Gastos confidenciales",
                     ObjetoGasto_cod == 90201    ~ "Sumas libres sin asignación presupuestaria",
                     ObjetoGasto_cod == 90202    ~ "Sumas con destino específico sin asignación presupuestaria"
                     
                   )
                  
      )                
                   
                   #####    Descripción y Destinatario      #####

sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,
                   
                                                   `Descripción` = Descripcion,
                                                    Destinatario = Descripcion1, 
                   
                  
                                       )
  
  
#Los montos 
  
  
sigaf_gasto_actual_mensual  <- mutate(sigaf_gasto_actual_mensual,
                                
                                `Enero Millones` = Enero/Millones,
                                `Febrero Millones` = Febrero/Millones,
                                `Marzo Millones` = Marzo/Millones,
                                `Abril Millones` = Abril/Millones,
                                `Mayo Millones` = Mayo/Millones,
                                `Junio Millones` = Junio/Millones,
                                `Julio Millones` = Julio/Millones,
                                `Agosto Millones` = Agosto/Millones,
                                `Septiembre Millones` = Septiembre/Millones,
                                `Octubre Millones` = Octubre/Millones,
                                `Noviembre Millones` = Noviembre/Millones,
                                `Diciembre Millones` = Diciembre/Millones,
                                `Total Millones` = Total/Millones,
                              
                                
  )

  


################################ 
#     Destinatario arreglado   #
################################  
  
# Ingresos_a <- Ingresos_a %>%  dplyr::full_join(PPI_a) %>% dplyr::filter(Año >= 2000)   
  
sigaf_gasto_actual_mensual <- sigaf_gasto_actual_mensual %>% dplyr::full_join(Destinatario_2021_mensual)   
  

names(sigaf_gasto_actual_mensual)

#################################################
#  Seleccionar las variables del archivo final  #
#################################################

sigaf_gasto_actual_mensual  <- sigaf_gasto_actual_mensual %>% 
                                  dplyr::select(Año, `TIPO`,`Centro Gestor`, `Tit_cod` , `Título`, `Programa cod`, `Prog des`, `Subprograma cod`, `Subprograma des`,`POS PRE`,
                                                Partida_cod, Partida, `Subpartida cod`, Subpartida,ObjetoGasto_cod, `ObjetoGasto` ,IP, `Fuente cod`, Fuente, 
                                                CE1_cod, `CE1`, CE2_cod, CE2, CE3_cod, `CE3`,`CE_cod`, CE, 
                                                `CF1_cod`, CF1, CF2_cod, CF2, CF3_cod, CF3, CF_cod, `CF`, 
                                                `Descripción`, Destinatario, `Destinatario arreglado`, `Enero`,`Enero Millones`,`Febrero`,`Febrero Millones`,`Marzo`,`Marzo Millones`,
                                                `Abril`,`Abril Millones`,`Mayo`,`Mayo Millones`, `Junio`,`Junio Millones`,`Julio`,`Julio Millones`,`Agosto`,`Agosto Millones`,
                                                `Septiembre`,`Septiembre Millones`,`Octubre`,`Octubre Millones`,`Noviembre`,`Noviembre Millones`,`Diciembre`,`Diciembre Millones`,
                                                `Total`,`Total Millones`,
                                                

                                  )

sigaf_gasto_actual_mensual  <-  mutate(sigaf_gasto_actual_mensual,
                                        Año = as.numeric(Año),
                             `Centro Gestor`= as.numeric(`Centro Gestor`),
                                   `Tit_cod`= as.numeric(`Tit_cod`),
                              `Programa cod`= as.numeric(`Programa cod`),
                          `Subprograma cod`= as.numeric(`Subprograma cod`),
                          `Fuente cod`= as.numeric(`Fuente cod`),
                            `CE1_cod`= as.numeric(`CE1_cod`),
                            `CE2_cod`= as.numeric(`CE2_cod`),
                            `CE3_cod`= as.numeric(`CE3_cod`),
                            `CE_cod`= as.numeric(`CE_cod`),
                          `CF1_cod`= as.numeric(`CF1_cod`),
                          `CF2_cod`= as.numeric(`CF2_cod`),
                          `CF3_cod`= as.numeric(`CF3_cod`),
                          `CF_cod`= as.numeric(`CF_cod`),
                          
                          
                                       )
                                       

######################################################
######################################################
#       Unión de los gastos acumulados               #
#                                                    #
######################################################
######################################################

###################
#  Filtrar GT     #
################### 

gastos_m_2007_Abril_2021 <- gastos_m_2007_Abril_2021 %>% dplyr::filter(Año!=Ano_actual)


#####################
#  Unificar datos   #
#####################

# cbind(names(gastos_m_2007_Abril_2021),names(sigaf_gasto_actual_mensual),
#          names(gastos_m_2007_Abril_2021) == names(sigaf_gasto_actual_mensual) )

Gastos_Mensuales <- dplyr::bind_rows(gastos_m_2007_Abril_2021,sigaf_gasto_actual_mensual)

Gastos_Mensuales <- Gastos_Mensuales %>% distinct()


#####################
#  Elimiar archivos #
#####################

remove(sigaf_gasto_actual_mensual)
remove(Destinatario_2021_mensual)
remove(gastos_m_2007_Abril_2021)





#################################################################################################### 
####################################################################################################
####################################################################################################
#                                              Exportación                                         #
####################################################################################################
####################################################################################################
#################################################################################################### 

# Directorio de exportación de las tablas #



setwd("C:/Users/oscar/Desktop/Gastos - 2021/Insumos/Exportación")

###############
#  Opciones   #
###############

options(encoding="utf-8")
options(scipen=999)


# Prueba

Gastos_mensuales_export <- paste("Gastos_mensuales-",Sys.Date(),".xlsx", sep = "")

write.xlsx(Gastos_Mensuales,Gastos_mensuales_export)

###### 


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
