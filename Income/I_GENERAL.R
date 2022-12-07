start.time <- Sys.time()

########################################################################################
#                                                                                      #
#                                                                                      #
#       Extracción + transformación + creacción de tablas (datamart)                   #
#                                                                                      #
#                                                                                      #
########################################################################################

###############
#   General   #
###############


#############################
#   Estructura del código   #  
#############################

# 0. Establecimiento de la configuración general del espacio de trabajo.
#
# 1. Importación de los archivos y otros referentes a los ingresos.
#
# 2. Ciertas modificaciones previar generales.
#
# 3. Unión + Transformación de ingresos mensuales + anuales
#
# 4. Creación de las principales tablas.
#
# 5. Exportación de los tablas con información.


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

setwd("C:/Users/oscar/Desktop/Ingresos - SIGAF/AI ---/Insumos/data")

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
suppressMessages(if(!require(highcharter)){ install.packages("highcharter")})
suppressMessages(if(!require(RcppRoll)){ install.packages("RcppRoll")})
suppressMessages(if(!require(openxlsx)){ install.packages("openxlsx")}) 


suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(highcharter))
suppressMessages(library(RcppRoll))
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

#Ano_actual    <- 2021
#Ano_pasado_1  <- 2020  
#Ano_pasado_2  <- 2019

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
#                              Importación de los ingresos                                       #
##################################################################################################
##################################################################################################
##################################################################################################

#############################
#    Ingresos 2007-2015     #
#############################

# Por decidir

#############################
#   Ingresos  2016-2020     #
#############################

ingresos_2007_2021 <- suppressWarnings(read_excel("SIGAF_2007_2021.xlsx")) # ingresos_2007_2021
names(ingresos_2007_2021)

# dim(ingresos_2016_2020)

###############################
#   Ingresos  SIGAF  actual   #
###############################

# Este si se debe llamar siempre "SIGAF_Ingresos_actual."

SIGAF_Ingresos_actual <- suppressWarnings(read_excel("SIGAF_Ingresos_actual.xlsx"))
names(SIGAF_Ingresos_actual)

###############################
#         Tabla del PIB       #
###############################

PIB <-  suppressWarnings(read_excel("PIB.xlsx"))
names(PIB)


###############################################
#   Presupuesto inicial   #
###############################################

PPI <- suppressWarnings(read_excel("PPI.xlsx"))
names(PPI)

#####
# 2 #  
##################################################################################################
##################################################################################################
##################################################################################################
#        Cambio en el ingreso total  PosPre - I   +   PospresAnual en el Presu. Inicial          #
##################################################################################################
##################################################################################################
##################################################################################################

#  Creación de la variable I0000000000

SIGAF_Ingresos_actual <-  SIGAF_Ingresos_actual %>%
                          mutate (PosPre = recode(PosPre, 'I' = 'I0000000000000'))


#      Creación de la variable Pospres PPI + PospresAnual    #


PPI <- PPI %>% 
             mutate(     Pospre = paste0(Clase,Subclase,Grupo,Subgrupo,Partida,Subpartida,Renglón,Subrenglón,`Fuente finaciamiento`),
                    Pospreanual = paste0(Año, Clase,Subclase,Grupo,Subgrupo,Partida,Subpartida,Renglón,Subrenglón,`Fuente finaciamiento`)) %>%
             dplyr::select(Año,Pospre,Pospreanual,Descripción,Clase,Subclase,Grupo,Subgrupo,Partida,Subpartida,Renglón,Subrenglón,
                           `Fuente finaciamiento`,`Presupuesto inicial`)

PPI_a <- PPI  %>% 
                 dplyr::select(Pospreanual,`Presupuesto inicial`)

#str(PPI_a)

#conteo1 <- PPI_a  %>% dplyr::group_by(Pospreanual)   %>% dplyr::summarise('Conteo' = n())

#####
# 3 #  
##################################################################################################
##################################################################################################
##################################################################################################
#                                  Ingresos  Mensuales                                           #
##################################################################################################
##################################################################################################
##################################################################################################

#############################
#############################
#   Ingresos  2007-2020     #
#############################
#############################

# Eliminar para el archivo mensual ciertas variables + cambiar nombre #

ingresos_2007_2021_m <- ingresos_2007_2021 %>% 
                                select(-c('Dif Pto Estimado – Ac Real','Ingreso acumulado real','Pendiente-Excedente',
                                          'Devengado Acumulado')) %>%
                                  dplyr::rename(
                                                  'Descripción'='Desc.Pos.presupuestaria',
                                                  'Presupuesto actual'='Presupuesto Actual'
                                  )

# Utilizar gather para tener una estructura horizontal de los datos ---> pronósticos más adelante #                                   

ingresos_2007_2021_m <- ingresos_2007_2021_m %>%
                                gather(mes, Ingresos, -c(Año, Nivel, PosPre,  Descripción,`Presupuesto actual`))  

# Renombras a los meses sin el devengado #

ingresos_2007_2021_m <- dplyr::mutate(ingresos_2007_2021_m,
                                
                                mes = case_when(
                                  mes == "Devengado Enero" ~ "Enero",
                                  mes == "Devengado Febrero" ~ "Febrero",
                                  mes == "Devengado Marzo" ~ "Marzo",
                                  mes == "Devengado Abril" ~ "Abril",
                                  mes == "Devengado Mayo" ~ "Mayo",
                                  mes == "Devengado Junio" ~ "Junio",
                                  mes == "Devengado Julio" ~ "Julio",
                                  mes == "Devengado Agosto" ~ "Agosto",
                                  mes == "Devengado Setiembre" ~ "Septiembre",
                                  mes == "Devengado Octubre" ~ "Octubre",
                                  mes == "Devengado Noviembre" ~ "Noviembre",
                                  mes == "Devengado Diciembre" ~ "Diciembre"
                                  
                                )
)

# Crear la variable mes.cod ---> sirve para luego aplicar ordenamentos

ingresos_2007_2021_m <- mutate (ingresos_2007_2021_m,
                                mes.cod = case_when(
                                  mes == "Enero"    ~ "01",
                                  mes == "Febrero"  ~ "02",
                                  mes == "Marzo"    ~ "03",
                                  mes == "Abril"    ~ "04",
                                  mes == "Mayo"     ~ "05",
                                  mes == "Junio"    ~ "06",
                                  mes == "Julio"    ~ "07",
                                  mes == "Agosto"   ~ "08",
                                  mes == "Septiembre"  ~ "09",
                                  mes == "Octubre"  ~ "10",
                                  mes == "Noviembre"  ~ "11",
                                  mes == "Diciembre"  ~ "12"
                                  
                                )
                                    ) %>% dplyr::arrange(Año, PosPre, mes.cod, mes) 

# Seleccionamos la posición de las variables

ingresos_2007_2021_m <- ingresos_2007_2021_m %>%
           select(Año,Nivel, PosPre,Descripción,'Presupuesto actual',mes.cod,mes,Ingresos)

################################
################################
#   Ingresos  Sigaf Actual     #
################################ 
################################

# Eliminar para el archivo mensual ciertas variables + cambiar nombre * crear variable año #

SIGAF_Ingresos_actual_m <- SIGAF_Ingresos_actual %>% 
                  select(-c('Dif Pto Estimado – Ac Real','Ingreso acumulado real','Pendiente-Excedente',
                            'Devengado Acumulado')) %>%
                     dplyr::rename(
                       'Descripción'='Desc.Pos.presupuestaria',
                       'Presupuesto actual'='Presupuesto Actual'
                      ) %>% 
                      mutate(
                                  Año = Ano_actual
                   )
             
  


# Utilizar gather para tener una estructura horizontal de los datos ---> pronósticos más adelante # 

SIGAF_Ingresos_actual_m <- SIGAF_Ingresos_actual_m %>%
  gather(mes, Ingresos, -c(Año, Nivel, PosPre,  Descripción,`Presupuesto actual`))  



# Renombras a los meses sin el devengado #

SIGAF_Ingresos_actual_m <- dplyr::mutate(SIGAF_Ingresos_actual_m,
                                      
                                      mes = case_when(
                                        mes == "Devengado Enero" ~ "Enero",
                                        mes == "Devengado Febrero" ~ "Febrero",
                                        mes == "Devengado Marzo" ~ "Marzo",
                                        mes == "Devengado Abril" ~ "Abril",
                                        mes == "Devengado Mayo" ~ "Mayo",
                                        mes == "Devengado Junio" ~ "Junio",
                                        mes == "Devengado Julio" ~ "Julio",
                                        mes == "Devengado Agosto" ~ "Agosto",
                                        mes == "Devengado Setiembre" ~ "Septiembre",
                                        mes == "Devengado Octubre" ~ "Octubre",
                                        mes == "Devengado Noviembre" ~ "Noviembre",
                                        mes == "Devengado Diciembre" ~ "Diciembre"
                                        
                                      )
)

# Crear la variable mes.cod ---> sirve para luego aplicar ordenamentos

SIGAF_Ingresos_actual_m <- mutate (SIGAF_Ingresos_actual_m,
                                mes.cod = case_when(
                                  mes == "Enero"    ~ "01",
                                  mes == "Febrero"  ~ "02",
                                  mes == "Marzo"    ~ "03",
                                  mes == "Abril"    ~ "04",
                                  mes == "Mayo"     ~ "05",
                                  mes == "Junio"    ~ "06",
                                  mes == "Julio"    ~ "07",
                                  mes == "Agosto"   ~ "08",
                                  mes == "Septiembre"  ~ "09",
                                  mes == "Octubre"  ~ "10",
                                  mes == "Noviembre"  ~ "11",
                                  mes == "Diciembre"  ~ "12"
                                  
                                )
                                    ) %>% dplyr::arrange(PosPre, mes.cod, mes) 

# Seleccionamos la posición de las variables

SIGAF_Ingresos_actual_m <- SIGAF_Ingresos_actual_m %>%
                select(Año,Nivel, PosPre,Descripción, 'Presupuesto actual',mes.cod,mes,Ingresos)


###################################
###################################
#   Ingresos totales mensuales    #
###################################
###################################

Ingresos_m <- rbind.data.frame(ingresos_2007_2021_m, SIGAF_Ingresos_actual_m)

#############################
#      Transformación       # 
#############################

# Declaración de variables #

Ingresos_m  <- dplyr::mutate(Ingresos_m,
                                          
                             n_Nivel = case_when(
                                                   Nivel == "1" ~ "Clase",
                                                   Nivel == "2" ~ "Subclase",
                                                   Nivel == "3" ~ "Grupo",
                                                   Nivel == "4" ~ "Subgrupo",
                                                   Nivel == "5" ~ "Partida",
                                                   Nivel == "6" ~ "Subpartida",
                                                   Nivel == "7" ~ "Renglón",
                                                   Nivel == "8" ~ "Subrenglón"
                             )
                                                         
)

Ingresos_m  <- dplyr::mutate(Ingresos_m,
                             
                             #################################################################
                             # Creación del clasificador de los ingresos del sector público  #
                             #################################################################
                             
                             
                             
                             cod_clase = substr(Ingresos_m$`PosPre`, 2, 2),
                             
                             clase = case_when(
                               cod_clase == "1" ~ "Ingresos corrientes",
                               cod_clase == "2" ~ "Ingresos de capital",
                               cod_clase == "3" ~ "Financiamiento"
                             ),
                             
                             cod_subclase = substr(Ingresos_m$`PosPre`, 2, 3),
                             
                             subclase = case_when(
                               
                               cod_subclase == "11" ~ "Ingresos tributarios",
                               cod_subclase == "12" ~ "Contribuciones sociales",
                               cod_subclase == "13" ~ "Ingresos no tributarios",
                               cod_subclase == "14" ~ "Transferencias corrientes",
                               
                               cod_subclase == "21" ~ "Venta de activos", # NS
                               cod_subclase == "23" ~ "Recuperación de préstamos e inversiones financieras", # NS
                               cod_subclase == "24" ~ "Transferencias de capital",
                               cod_subclase == "25" ~ "Otros ingresos de capital", # NS
                               
                               cod_subclase == "31" ~ "Financiamiento interno",
                               cod_subclase == "32" ~ "Financiamiento externo",
                               cod_subclase == "33" ~ "Recursos de vigencias anteriores"
                               
                             ),
                             
                             cod_grupo = substr(Ingresos_m$`PosPre`, 2, 4),
                             
                             grupo = case_when(
                               
                               #######  Asignaciones  ############
                               
                               cod_grupo == "111" ~ "Impuestos a los ingresos y utilidades",
                               cod_grupo == "112" ~ "Impuestos sobre la propiedad",
                               cod_grupo == "113" ~ "Impuestos sobre bienes y servicios",
                               cod_grupo == "114" ~ "Impuestos sobre el comercio exterior y las transacciones internacionales",
                               cod_grupo == "119" ~ "Otros ingresos tributarios",
                               
                               cod_grupo == "121" ~ "Contribuciones a la seguridad social",
                               
                               cod_grupo == "131" ~ "Venta de bienes y servicios",
                               cod_grupo == "132" ~ "Ingresos de la propiedad",
                               cod_grupo == "133" ~ "Multas, sanciones, remates y comisos",
                               cod_grupo == "134" ~ "Intereses Moratorios",
                               cod_grupo == "139" ~ "Otros ingresos no tributarios",
                               
                               cod_grupo == "141" ~ "Transferencias corrientes del sector público",
                               cod_grupo == "142" ~ "Transferencias corrientes del sector privado",
                               cod_grupo == "143" ~ "Transferencias corrientes del sector externo",
                               
                               cod_grupo == "211" ~ "Venta de activos fijos", # NS
                               cod_grupo == "234" ~ "Recuperación de inversiones financieras",
                               cod_grupo == "243" ~ "Transferencias de capital del sector externo", # NS
                               cod_grupo == "241" ~ "Transferencias de capital del sector público",
                               
                               
                               cod_grupo == "313" ~ "Colocación de títulos valores",
                               
                               
                               cod_grupo == "321" ~ "Créditos directos",  # Préstamos directos 
                               cod_grupo == "323" ~ "Colocación de títulos valores en el exterior", # NS
                               
                               cod_grupo == "331" ~ "Superávit libre",
                               cod_grupo == "332" ~ "Superávit específico"
                               
                               ####### Remplazar por "-" #######
                             ),
                             
                             cod_subgrupo = substr(Ingresos_m$`PosPre`, 2, 5),
                             
                             subgrupo = case_when(
                               
                               #######  Asignaciones  ############
                               
                               cod_subgrupo == "1111" ~ "Impuestos sobre los ingresos y utilidades de personas físicas",
                               cod_subgrupo == "1112" ~ "Impuestos sobre los ingresos y utilidades de personas jurídicas",
                               
                               cod_subgrupo == "1113" ~ "Impuestos sobre dividendos e intereses de títulos valores", # NS
                               cod_subgrupo == "1114" ~ "Impuesto sobre remesas al exterior",
                               cod_subgrupo == "1115" ~ "Impuesto especial sobre bancos y entidades financieras no domiciliadas", # NS
                               
                               cod_subgrupo == "1121" ~ "Impuesto sobre la propiedad de bienes inmuebles",
                               cod_subgrupo == "1122" ~ "Impuesto sobre la propiedad de vehículos, aeronaves y embarcaciones",
                               cod_subgrupo == "1123" ~ "Impuestos sobre el patrimonio",
                               cod_subgrupo == "1124" ~ "Impuestos sobre el traspaso de bienes inmuebles",
                               cod_subgrupo == "1125" ~ "Impuestos sobre el traspaso de vehículos, aeronaves y embarcaciones ",
                               
                               cod_subgrupo == "1131" ~ "Impuesto general sobre ventas y consumo",
                               cod_subgrupo == "1132" ~ "Impuestos específicos sobre la producción y consumo de bienes y servicios",
                               
                               cod_subgrupo == "1141" ~ "Impuestos a las importaciones",
                               cod_subgrupo == "1142" ~ "Impuestos a las exportaciones",
                               cod_subgrupo == "1143" ~ "Otros impuestos sobre el comercio exterior y las transacciones internacionales",
                               
                               cod_subgrupo == "1191" ~ "Impuesto de timbres",
                               cod_subgrupo == "1199" ~ "Ingresos tributarios diversos",
                               
                               cod_subgrupo == "1213" ~ "Contribución a regímenes especiales de pensiones",
                               
                               cod_subgrupo == "1219" ~ "OTRAS CONTRIBUCIONES SOCIALES FODESAF", # NUEVO
                               
                               cod_subgrupo == "1311" ~ "Venta de bienes",
                               cod_subgrupo == "1312" ~ "Venta de servicios",
                               cod_subgrupo == "1313" ~ "Derechos administrativos",
                               
                               cod_subgrupo == "1321" ~ "Traspaso de dividendos",
                               cod_subgrupo == "1322" ~ "Renta de la propiedad",
                               cod_subgrupo == "1323" ~ "Renta de activos financieros",
                               
                               cod_subgrupo == "1331" ~ "Multas y sanciones",
                               cod_subgrupo == "1332" ~ "Remates y comisos",
                               cod_subgrupo == "1341" ~ "Intereses moratorios por atraso en pago de impuesto", # CN
                               cod_subgrupo == "1342" ~ "Intereses moratorios por atraso en pago de bienes y servicios", # CN
                               cod_subgrupo == "1349" ~ "Otros intereses moratorios", # NUEVO
                               
                               cod_subgrupo == "1391" ~ "Reintregros y devoluciones", #CN
                               
                               cod_subgrupo == "1392" ~ "Ejecución de contratos de seguros", #NS
                               cod_subgrupo == "1399" ~ "Ingresos varios no especificados",
                               
                               cod_subgrupo == "1411" ~ "Transferencias corrientes del Gobierno Central", #NS
                               cod_subgrupo == "1412" ~ "Transferencias corrientes de Órganos Desconcentrados", #NS
                               cod_subgrupo == "1413" ~ "Transferencias corrientes de Instituciones Descentralizadas No Empresariales",
                               cod_subgrupo == "1414" ~ "Transferencias corrientes de Gobiernos Locales",
                               cod_subgrupo == "1415" ~ "Transferencias corrientes de Empresas Públicas no Financieras",
                               cod_subgrupo == "1416" ~ "Transferencias corrientes de Instituciones Públicas Financieras",
                               
                               cod_subgrupo == "1421" ~ "Transferencias corrientes del sector privado", # NS
                               cod_subgrupo == "1423" ~ "Transferencias corrientes del sector privado", # NS
                               cod_subgrupo == "1424" ~ "Transferencias corrientes del sector privado", # NS
                               cod_subgrupo == "1425" ~ "Transferencias corrientes del Sector Privado-Superávit Libre artículo 5 Ley 9371", # NS
                               cod_subgrupo == "1426" ~ "Transferencias de aseguradoras privadas para Financ. INEC Ley 9694", # Nuevo nombre
                               cod_subgrupo == "1427" ~ "Transferencias corrientes del Sector Privado-según Convenio Ma rco de Cooperación no Reembolsable entre INCIENSA y FUNIN del 25 de octubre de 2007 (INCIENSA)",
                               cod_subgrupo == "1428" ~ "Transferencias corrientes del Sector Privado- Superávit Libre artículo 17 Tít ulo IV Ley 9635",
                               
                               cod_subgrupo == "1431" ~ "Transfererencias corrientes de organismos internacionales",
                               cod_subgrupo == "1432" ~ "Transferencias corrientes de gobiernos extranjeros", # NS
                               
                               cod_subgrupo == "2111" ~ "Venta de terrenos", # NS
                               cod_subgrupo == "2113" ~ "Venta de maquinaria y equipo", # NS
                               
                               cod_subgrupo == "2412" ~ "Transferencias de capital de Órganos Desconcentrados", # NS
                               cod_subgrupo == "2413" ~ "Transferencias de capital de Instituciones Descentralizadas No Empresariales",
                               cod_subgrupo == "2416" ~ "Transferencias de capital de Instituciones Públicas Financieras", # NS
                               
                               cod_subgrupo == "2431" ~ "Transferencias de capital de organismos internacionales", # NS
                               cod_subgrupo == "2432" ~ "Transferencias de capital de gobiernos extranjeros", # NS
                               
                               cod_subgrupo == "3131" ~ "Títulos valores",
                               
                               cod_subgrupo == "3211" ~ "Créditos de organismos internacionales de desarrollo",
                               cod_subgrupo == "3212" ~ "Créditos de gobiernos extranjeros", 
                               cod_subgrupo == "3213" ~ "Créditos de bancos privados", # NS
                               
                               cod_subgrupo == "3311" ~ "Superávit Libre", # NUEVO 
                               
                               cod_subgrupo == "3321" ~ "Superávit Específico", 
                               
                               cod_subgrupo == "3232" ~ "Colocación de títulos valores de largo plazo"
                               
                               ####### Remplazar por "-" #######
                             ),
                             
                             cod_partida = substr(Ingresos_m$`PosPre`, 2, 7),
                             
                             partida = case_when(
                               
                               #######  Asignaciones  ############
                               
                               cod_partida == "111101" ~ "Impuesto  sobre  salarios,  jubilaciones,  pensiones  y otros pagos laborales del Sector Público",
                               cod_partida == "111102" ~ "Impuesto  sobre  salarios,  jubilaciones,  pensiones  y otros pagos laborales del Sector Privado",
                               cod_partida == "111103" ~ "Impuestos sobre los ingresos y utilidades de personas físicas",
                               
                               cod_partida == "111201" ~ "Impuestos sobre los ingresos y utilidades de las personas jurídicas del Sector Público",
                               cod_partida == "111202" ~ "Impuestos sobre los ingresos y utilidades de las personas jurídicas del Sector Privado",
                               
                               cod_partida == "111301" ~ "Impuesto sobre dividendos", # NS
                               cod_partida == "111302" ~ "Impuestos sobre los intereses de títulos valores", # NS
                               
                               cod_partida == "111401" ~ "Impuesto sobre remesas al exterior",
                               
                               cod_partida == "111501" ~ "Impuesto especial sobre bancos y entidades financieras no domiciliadas", # NS
                               
                               cod_partida == "112101" ~ "Impuesto solidario de vivienda Ley N°8683",
                               cod_partida == "112201" ~ "Impuesto a la propiedad de vehículos Ley N°7088",
                               cod_partida == "112202" ~ "Timbre Fauna Silvestre Ley N°7317",
                               
                               cod_partida == "112301" ~ "Incremento Timbre de Educación y Cultura Ley N° 6879",
                               cod_partida == "112302" ~ "Impuesto sobre personas jurídicas Ley N° 9024", # NS
                               cod_partida == "112303" ~ "Impuesto a las Personas Jurídicas Ley 9428",
                               
                               cod_partida == "112401" ~ "Impuestos sobre el traspaso de bienes inmuebles",
                               cod_partida == "112501" ~ "Impuesto sobre el traspaso de vehículos usados Ley N° 7088",
                               
                               cod_partida == "113101" ~ "Impuesto general sobre las ventas",
                               cod_partida == "113102" ~ "Impuesto selectivo de consumo",
                               
                               cod_partida == "113201" ~ "Impuestos específicos sobre la producción y consumo de bienes",
                               cod_partida == "113202" ~ "Impuestos específicos sobre la producción y consumo de servicios",
                               
                               cod_partida == "114101" ~ "Derechos de importación de mercancías",
                               cod_partida == "114102" ~ "Impuestos sobre el valor aduanero de las mercancías",
                               cod_partida == "114109" ~ "Otros impuestos a las importaciones", # NS
                               
                               
                               cod_partida == "114201" ~ "Derechos de exportación de mercancías",
                               cod_partida == "114209" ~ "Otros impuestos a las exportaciones",
                               
                               cod_partida == "114301" ~ "Impuestos por movilización de carga portuaria", #NS
                               cod_partida == "114302" ~ "Impuesto por uso de terminal portuaria", #NS
                               cod_partida == "114303" ~ "Impuesto de salida al exterior",
                               cod_partida == "114304" ~ "Derechos consulares",
                               cod_partida == "114309" ~ "Otros impuestos sobre el comercio exterior y las transacciones internacionales",
                               
                               cod_partida == "119101" ~ "Timbre Fiscal Ley N° 7208", #
                               
                               cod_partida == "119102" ~ "Timbre del Registro Nacional",
                               cod_partida == "119103" ~ "Timbre Propiedad Intelectual - Registro Nacional", #NS
                               cod_partida == "119104" ~ "Timbre Portal Web - Registro Nacional", #NS
                               cod_partida == "119105" ~ "Placas - Registro Nacional", #NS
                               cod_partida == "119106" ~ "Timbre Gobierno Digital - Registro Nacional", #NS
                               cod_partida == "119107" ~ "Timbre Colegio Abogados - Direccion Nacional de No tariado", #NS
                               cod_partida == "119108" ~ "Timbre Archivo Nacional", #NS
                               cod_partida == "119109" ~ "Timbre Educacion y Cultura inciso c) artículo 10 L ey 5923 y sus reformas Museo Nacional", #NS
                               cod_partida == "119110" ~ "Timbre Educacion y Cultura inciso d) artículo 10 L ey 5923 y sus reformas SINEM", #NS
                               cod_partida == "119111" ~ "Timbre Pro-Parques Nacionales SINAC", #NS
                               cod_partida == "119112" ~ "Timbre de Vida Silvestre SINAC", #NS
                               cod_partida == "119113" ~ "Timbre Pro-Parques Nacionales CONAGEBIO", #NS
                               
                               cod_partida == "119901" ~ "Papel Sellado Ley N° 7345",
                               cod_partida == "119902" ~ "Ingresos por distribuir", # NS
                               
                               cod_partida == "121301" ~ "Contribución del Magisterio Nacional de miembros activos", # NN
                               cod_partida == "121302" ~ "Contribución del Magisterio Nacional miembros pensionados y jubilados",
                               cod_partida == "121303" ~ "Contribución a otros regímenes de pensiones",
                               cod_partida == "121304" ~ "Contribución Especial Solidaria Ley 9383",  # NS
                               
                               cod_partida == "121902" ~ "Contribución patronal sobre la nómina de Órganos Desconcentrados", # Nuevo
                               cod_partida == "121903" ~ "Contribución patronal sobre la nómina de Instituci ones Descentralizadas no Empresariales", # Nuevo
                               cod_partida == "121904" ~ "Contribución patronal sobre la nómina de Gobiernos Locales", # Nuevo
                               cod_partida == "121905" ~ "Contribución patronal sobre la nómina de Empresas Públicas No Financieras", # Nuevo
                               cod_partida == "121906" ~ "Contribución patronal sobre la nómina de Instituci ones Públicas Financieras", # Nuevo
                               cod_partida == "121907" ~ "Contribución patronal sobre la nómina del Sector P rivado", # Nuevo
                               cod_partida == "131101" ~ "Venta de bienes agropecuarios y forestales", # Nuevo
                               
                               cod_partida == "131109" ~ "Venta de otros bienes",
                               
                               cod_partida == "131201" ~ "Servicios de transporte",
                               cod_partida == "131203" ~ "Servicios financieros y de seguros",
                               cod_partida == "131204" ~ "Alquileres y otros alquileres",
                               cod_partida == "131209" ~ "Otros servicios",
                               
                               cod_partida == "131301" ~ "Derechos administrativos a los servicios de transporte",
                               cod_partida == "131302" ~ "Derechos administrativos a otros servicios públicos",
                               
                               cod_partida == "132101" ~ "20% utilidades del IMAS", #NS
                               cod_partida == "132102" ~ "Acciones CEMEX-CR SA", #NS
                               cod_partida == "132103" ~ "Utilidades INS (25%)", 
                               
                               cod_partida == "132201" ~ "Concesión de obra pública",
                               cod_partida == "132202" ~ "Alquiler de terrenos",
                               cod_partida == "132209" ~ "Otros ingresos de la renta de la propiedad",
                               
                               cod_partida == "132301" ~ "Intereses sobre títulos valores",
                               cod_partida == "132302" ~ "Intereses y comisiones sobre préstamos", #NS
                               cod_partida == "132303" ~ "Otras rentas de activos financieros",
                               
                               cod_partida == "133101" ~ "Multas de tránsito",
                               cod_partida == "133102" ~ "Multas por atraso en pago de impuestos",
                               cod_partida == "133104" ~ "Sanciones administrativas y judiciales", #NN
                               cod_partida == "133109" ~ "Otras multas y sanciones",
                               
                               cod_partida == "133201" ~ "Remates y confiscaciones",
                               cod_partida == "134200" ~ "Intereses moratorios por atraso en pago de bienes y servicios",
                               
                               cod_partida == "134901" ~ "Otros intereses moratorios sobre multas de tránsit o COSEVI",  #nuevo
                               cod_partida == "139200" ~ "Ejecución de contratos de seguros",
                               cod_partida == "139901" ~ "Ingresos varios no especificados", #nuevo
                               cod_partida == "139902" ~ "Ingresos varios no especificados COSEVI", #nuevo
                               cod_partida == "139903" ~ "Ingresos varios no especificados Instituto Costarr icense sobre Drogas (ICD)", #nuevo
                               cod_partida == "139904" ~ "Otros ingresos varios no especificados Artículo 44 Ley 8488 Comisión Nacional de Prevención y Atenci ón de Emergencias (CNE)", #nuevo
                               
                               cod_partida == "141100" ~ "Donaciones corrientes del Gobierno Central",
                               cod_partida == "141101" ~ "Donaciones corrientes del Gobierno Central", #NS
                               cod_partida == "141102" ~ "Transferencias corrientes Gobierno Central Superáv it Libre artículo 5 Ley 9371", #NS
                               
                               cod_partida == "141201" ~ "Transferencias de FODESAF", #NS
                               cod_partida == "141202" ~ "Transferencias de la Junta Administrativa del Registro Nacional Ley N° 7138", #NS
                               cod_partida == "141203" ~ "Consejo de Seguridad Vial", #NS
                               cod_partida == "141204" ~ "PROMECE", #NS
                               cod_partida == "141205" ~ "CNE Ley 8933", #NS
                               cod_partida == "141206" ~ "CTAC Ley 5222", #NS
                               cod_partida == "141207" ~ "Direc.Geología Minas", #NS
                               cod_partida == "141208" ~ "SINAC", #NS
                               cod_partida == "141209" ~ "Unidad Ejecutora del Programa de Catastro y Registro", #NS
                               cod_partida == "141211" ~ "Instituto Meteorológico Nacional", #NS
                               cod_partida == "141212" ~ "Consejo Superior de Educación", #NS
                               cod_partida == "141213" ~ "Unidad Ejecutora Programa para la Prevención de la Violencia y Promoción de la Inclusión Social del Ministerio de Justicia y Paz", #NS
                               cod_partida == "141214" ~ "Transferencias corrientes Órganos Desconcentrados Superávit Libre artículo 5 Ley 9371", #NS
                               
                               cod_partida == "141301" ~ "Cuotas a Organismos Internacionales, Ley N° 3418",
                               cod_partida == "141303" ~ "Junta de Desarrollo de la Zona Sur. Ley N° 7730",
                               cod_partida == "141304" ~ "Instituto Nacional de Estadística y Censos Ley N° 7839 artículo 13 -Convenio BCCR-INEC-MH para Estudio Económico a Empresas",
                               cod_partida == "141305" ~ "SENARA Ley 7064", #NS
                               cod_partida == "141306" ~ "INDER", #NS
                               cod_partida == "141307" ~ "Instituto de Desarrollo Rural Fideicomiso Pequeños Medianos productores de piña.", #NS
                               cod_partida == "141308" ~ "Instituto Nacional de Fomento y Asesoría Municipal recursos convenio MIVAH-PRUGAM", #NS
                               cod_partida == "141309" ~ "Servicios Nacional de Aguas Subterráneas, Riego y Avenamiento (SENARA)",
                               cod_partida == "141310" ~ "Transferencias corrientes Instituciones Descentral izadas no Empresariales Superávit Libre artículo 5 Ley 9371", #NS
                               
                               cod_partida == "141311" ~ "Transferencia Instituto Costarricense de Pesca y A cuicultura artículo 3 inciso h) Ley 8149 - INTA", #Nuevo
                               cod_partida == "141312" ~ "Transferencias corrientes Instituciones Descentral izadas No Empresariales a Consejo Técnico de Aviac ión Civil (CTAC)", #Nuevo
                               cod_partida == "141313" ~ "Transferencia Instituto de Desarrollo Rural Ley N° 9036 a Instituto sobre Alcoholismo y Farmacodepen dencia (IAFA)", #Nuevo
                               cod_partida == "141317" ~ "Transferencias corrientes Instituciones Descentrallizadas no Empresariales Reintegro de recursos artículo 4 Ley 9840",
                               
                               cod_partida == "141401" ~ "Transferencias de Municipalidades Ley N° 7729",
                               cod_partida == "141402" ~ "Transferencias de Municipalidades Ley N° 7755",
                               cod_partida == "141403" ~ "Transferencias de Gobiernos locales Superávit Libre artículo 5 Ley Nº 9371",
                               
                               cod_partida == "141404" ~ "Transferencia de Gobiernos Locales Ley 9303 a Cons ejo Nacional de Personas con Discapacidad (CONAPDI S)", # Nuevo
                               cod_partida == "141405" ~ "Transferencia de Gobierno Locales Ley 7509 a Regis tro Nacional", # Nuevo
                               cod_partida == "141406" ~ "Transferencia de Gobierno Locales Ley 7509 a Casa Cultura Puntarenas", # Nuevo
                               cod_partida == "141407" ~ "Transferencias de Gobiernos Locales Ley 8461 a Parque Marino del Pacífico",
                               
                               cod_partida == "141501" ~ "Cuotas Organismos Internacionales, Ley N° 3418",
                               cod_partida == "141502" ~ "RECOPE Ley N° 7399 Art 56",
                               cod_partida == "141504" ~ "Refinadora Costarricense de Petroleo art. 5 y 6 Ley N°9840 Emergencia COVID-19",
                               
                               cod_partida == "141505" ~ "Trasferencia Junta de Protección Social Ley 9379 a Consejo Nacional de Personas con Discapacidad (CO NAPDIS)", #Nuevo
                               cod_partida == "141506" ~ "Transferencia Junta de Protección Social Ley 8718 a Consejo Técnico Asistencia Médico Social (CTAMS)", # Nuevo
                               cod_partida == "141507" ~ "Transferencia Junta de Protección Social Ley 8111 a Comisión Nacional de Vacunación y Epidemiología (CNVE)", #Nuevp
                               cod_partida == "141508" ~ "Transferencia Fábrica Nacional de Licores (Ley N° 8289) a Instituto sobre Alcoholismo y Farmacodepen dencia (IAFA)", #Nuevo
                               cod_partida == "141511" ~ "Junta de Protección Social Ley 8718 art. 8 inc. d) a INCIENSA",
                               
                               cod_partida == "141601" ~ "Cuotas Organismos Internacionales, Ley N° 3418",
                               cod_partida == "141602" ~ "INS Parques Culturales // Transferencias de aseguradoras públicas para Financ. INEC Ley 9694", # Ojo a esta
                               
                               
                               cod_partida == "141604" ~ "Transferencia INS - Consejo Salud Ocupacional", 
                               
                               cod_partida == "142101" ~ "Código Notalerial Ley 7764",
                               
                               
                               cod_partida == "143101" ~ "Donación BIRF",
                               cod_partida == "143102" ~ "Donación PNUD",
                               cod_partida == "143103" ~ "Donación BCIE",
                               
                               
                               cod_partida == "143104" ~ "Transferencias de organismos internacionales al Archivo Nacional", #No SIGAF en casi todos 
                               
                               cod_partida == "143202" ~ "Donación República de China",
                               cod_partida == "143203" ~ "Donación Unión Europea",
                               cod_partida == "143204" ~ "DONACION KOLFACI",
                               cod_partida == "143205" ~ "Subvención de la Unión Europea para el Fortalecimi ento del Programa Justicia Restaurativa- Poder Judicial",
                               
                               cod_partida == "241203" ~ "Transf.Captal Inst M",
                               cod_partida == "241205" ~ "Unidad Coordinadora Proyecto Limón Ciudad Puerto",
                               cod_partida == "241206" ~ "Dir. Geologias Minas",
                               cod_partida == "241207" ~ "IMN",
                               cod_partida == "241208" ~ "FODESAF - Min. Cultu",
                               cod_partida == "241209" ~ "Comisión Nacional de Emergencias Ley No. 8933",
                               cod_partida == "241210" ~ "Consejo Nacional de la Política Pública de la Persona Joven (CPJ)",
                               cod_partida == "241211" ~ "Museo de Arte Costarricense",
                               cod_partida == "241212" ~ "Museo de Arte y Diseño Contemporáneo (MADC)",
                               cod_partida == "241213" ~ "Sistema Nacional de Educación Musical (SINEM)",
                               cod_partida == "241214" ~ "Dirección Nacional de Centros de Educación y Nutri ción y de Centros Infantiles de Atención Integral (Dirección de CEN-CINAI)",
                               
                               cod_partida == "241301" ~ "Instituto Nacional de Aprendizaje. Ley Nº7372 de 22-11-93", # Nuevo nombre
                               
                               cod_partida == "241602" ~ "Instituto Nacional de Seguros. Atención emergencia COVID-19 Ley N°9847",
                               
                               cod_partida == "243102" ~ "Donación BIRF",
                               
                               cod_partida == "243204" ~ "Donación China",
                               
                               cod_partida == "313101" ~ "Colocación de títulos valores de corto plazo",
                               cod_partida == "313102" ~ "Colocación de títulos valores de largo plazo",
                               
                               cod_partida == "321101" ~ "Banco Centroaméricano de Integración Económica",
                               cod_partida == "321102" ~ "Banco Interamericano de Desarrollo",
                               cod_partida == "321103" ~ "Banco Mundial",
                               cod_partida == "321105" ~ "Fondo Internacional de Desarrollo Agropecuario",
                               cod_partida == "321109" ~ "Otros Créditos de organismos internacionales de desarrollo",
                               
                               cod_partida == "331101" ~ "Superávit Libre",
                               cod_partida == "332101" ~ "Superávit Específico",
                               
                               cod_partida == "321203" ~ "Otros Créditos de organismos internacionales de desarrollo", # Nuevo
                               
                               ####### Remplazar por "" #######
                               
                               cod_partida == "119102" ~ "",
                               cod_partida == "119103" ~ "",
                               cod_partida == "119104" ~ "",
                               cod_partida == "119105" ~ "",
                               cod_partida == "119106" ~ "",
                               cod_partida == "119107" ~ "",
                               cod_partida == "119108" ~ "",
                               cod_partida == "119109" ~ "",
                               cod_partida == "119110" ~ "",
                               cod_partida == "119111" ~ "",
                               cod_partida == "119112" ~ "",
                               cod_partida == "119113" ~ "",
                               cod_partida == "121902" ~ "",
                               cod_partida == "121903" ~ "",
                               cod_partida == "121904" ~ "",
                               cod_partida == "121905" ~ "",
                               cod_partida == "121906" ~ "",
                               cod_partida == "121907" ~ "",
                               cod_partida == "131101" ~ "",
                               cod_partida == "131201" ~ "",
                               cod_partida == "132201" ~ "",
                               cod_partida == "132209" ~ "",
                               cod_partida == "134901" ~ "",
                               
                               cod_partida == "139901" ~ "",
                               cod_partida == "139902" ~ "",
                               cod_partida == "139903" ~ "",
                               cod_partida == "139904" ~ "",
                               cod_partida == "141311" ~ "",
                               cod_partida == "141312" ~ "",
                               cod_partida == "141313" ~ "",
                               cod_partida == "141404" ~ "",
                               cod_partida == "141405" ~ "",
                               cod_partida == "141406" ~ "",
                               cod_partida == "141505" ~ "",
                               cod_partida == "141506" ~ "",
                               cod_partida == "141507" ~ "",
                               cod_partida == "141508" ~ "",
                               cod_partida == "141604" ~ "",
                               cod_partida == "143104" ~ "",
                               
                               cod_partida == "321203" ~ "",
                               cod_partida == "323201" ~ "",
                               
                               cod_partida == "331103" ~ "",
                               cod_partida == "332102" ~ "",
                               
                               cod_partida == "141314" ~ "Transferencias corrientes Superávit Libre Ley 9925 Adición Transitorio III Ley 9371",
                               cod_partida == "141215" ~ "Transferencias corrientes Superávit Libre Ley 9925 Adición Transitorio III Ley 9371",
                               cod_partida == "141302" ~ "Instituto Nacional de Aprendizaje. Ley Nº7372 / 22 -11-93",
                               
                               
                               ####### Nombre en la jerarquía, poner un "-" #######
                               
                               cod_partida == "321200" ~ "-",
                               cod_partida == "134100" ~ "-",
                               cod_partida == "139900" ~ "",
                               cod_partida == "139100" ~ "Reintegros de efectivo Ley Nº 7056",
                               cod_partida == "141510" ~ "Transferencias Empresas Públicas no Financieras Su perávit Libre Ley 9925 adición Transitorios III y IV Ley 9371",
                               cod_partida == "142500" ~ "-",
                               
                               cod_partida == "142600" ~ "-"
                               
                             ),
                             
                             cod_subpartida = substr(Ingresos_m$`PosPre`, 2, 9),
                             
                             subpartida = case_when(
                               
                               #######  Asignaciones  ############
                               
                               cod_subpartida == "11110101" ~ "Impuesto sobre salarios, jubilaciones, pensiones y otros pagos laborales del Sector Público", # Nuevo
                               cod_subpartida == "11110201" ~ "Impuesto sobre salarios, jubilaciones, pensiones y otros pagos laborales del Sector Privado", # Nuevo
                               cod_subpartida == "11110301" ~ "IMPUESTO SOBRE LOS INGRESOS Y UTILIDADES DE PERSON AS FÍSICAS", # Nuevo
                               
                               
                               cod_subpartida == "11110302" ~ "Impuesto sobre rentas de capital inmobiliario de personas físicas",
                               cod_subpartida == "11110303" ~ "Impuesto sobre rentas de capital mobiliario de personas físicas",
                               cod_subpartida == "11110304" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas físicas"
                               ,
                               
                               cod_subpartida == "11120101" ~ "Impuesto sobre los ingresos y utilidades de las personas jurídicas del Sector Público",
                               cod_subpartida == "11120102" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del sector público",
                               cod_subpartida == "11120103" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del sector público",
                               
                               cod_subpartida == "11120202" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del Sector Privado",
                               cod_subpartida == "11120203" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del Sector Privado",
                               cod_subpartida == "11120204" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas jurídicas del sector privado", # NS
                               
                               cod_subpartida == "11120105" ~ "Impuesto a los ingresos y utilidades artículo 46 L ey 8488 CNE", # nuevo
                               cod_subpartida == "11120201" ~ "Impuesto sobre los ingresos y utilidades de las personas jurídicas del Sector Privado",# nuevo
                               cod_subpartida == "11120202" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del Sector Privado",# nuevo
                               cod_subpartida == "11120203" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del Sector Privado",# nuevo
                               cod_subpartida == "11120204" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas jurídicas del sector privado",# nuevo
                               
                               cod_subpartida == "11310101"~ "Impuesto sobre la venta de bienes y servicios internos",
                               cod_subpartida == "11310102"~ "Impuesto sobre la venta de bienes y servicios importados",
                               
                               cod_subpartida == "11310201" ~ "Impuesto selectivo de consumo de bienes internos",
                               cod_subpartida == "11310202" ~ "Impuesto selectivo de consumo de bienes importados",
                               
                               cod_subpartida == "11320101" ~ "Impuestos específicos sobre productos agropecuarios y forestales",
                               cod_subpartida == "11320102" ~ "Impuestos específicos sobre la explotación de recursos naturales y minerales",
                               cod_subpartida == "11320103" ~ "Impuestos sobre combustibles y energéticos",
                               cod_subpartida == "11320104" ~ "Impuestos sobre bienes manufacturados",
                               cod_subpartida == "11320109" ~ "Otros impuestos específicos sobre la producción y consumo de bienes",
                               
                               
                               cod_subpartida == "11320201" ~ "Impuestos específicos a los servicios de hospedaje", #NS
                               cod_subpartida == "11320202" ~ "Impuestos específicos a los servicios de transporte",
                               cod_subpartida == "11320203" ~ "Impuestos específicos a los servicios de diversión y esparcimiento",
                               
                               cod_subpartida == "11410101" ~ "Arancel de aduanas",
                               
                               cod_subpartida == "11410201" ~ "1% sobre el valor aduanero de las mercancías",
                               cod_subpartida == "11410202" ~ "1,5% valor CIF de productos agroquímicos SFE", # Nuevo
                               
                               cod_subpartida == "11420101" ~ "Derecho sobre exportación del banano",
                               cod_subpartida == "11420901" ~ "¢1.5 p/caja Ban Expo",
                               cod_subpartida == "11420902" ~ "Impuestos exportación vía terrestre",
                               
                               cod_subpartida == "11430301" ~ "Derecho de salida del territorio nacional vía áerea",
                               cod_subpartida == "11430302" ~ "Derecho de salida del territorio nacional vía terrestre",
                               
                               cod_subpartida == "11430401" ~ "Derechos consulares Ley N° 7293",
                               
                               cod_subpartida == "11430901" ~ "Impuestos Ley de Migración N° 8764",
                               cod_subpartida == "11430902" ~ "Impuesto General Forestal ",
                               
                               cod_subpartida == "11990201" ~ "Anticipos de aduana ",
                               cod_subpartida == "11990202" ~ "Anticipos impuesto al banano",
                               
                               cod_subpartida == "12130101" ~ "Contribución Magisterio Nacional - Activos. Ley Nº 7531", # Nuevo
                               cod_subpartida == "12130102" ~ "Contribución por traslado al régimen de Reparto L ey 8721", # Nuevo
                               cod_subpartida == "12130201" ~ "Contribución Magisterio Nacional - Pensionados. Le y Nº 7531", # Nuevo
                               
                               
                               cod_subpartida == "12130301" ~ "Deducción sueldos para pensionados Ley Nº 7302",
                               cod_subpartida == "12130302" ~ "Contribución Especial Solidaria Ley 9383", # nuevo
                               
                               
                               cod_subpartida == "13110101" ~ "Venta de Bienes Estaciones Experimentales INTA", # nuevo
                               
                               
                               cod_subpartida == "13110901" ~ "Venta bienes IGN-MOPT",  # nuevo
                               cod_subpartida == "13110902" & Año < 2021 ~ "Venta de bienes -FONAM- Ley 7554",
                               cod_subpartida == "13110902" & Año >= 2021 ~ "Venta de bienes SENASA",# nuevo
                               cod_subpartida == "13110903" ~ "Venta Libros, revistas y folletos Museo Nacional", # nuevo
                               cod_subpartida == "13110904" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense", # nuevo
                               cod_subpartida == "13110905" ~ "Venta de producción planteles y otros bienes Imprenta Nacional", # nuevo
                               cod_subpartida == "13110907" ~ "Venta de bienes SINAC",
                               
                               cod_subpartida == "13120101" ~ "Servicios de transporte por carretera",# nuevo 
                               cod_subpartida == "13120104" ~ "Servicio de transporte aeroportuario",# nuevo
                               
                               cod_subpartida == "13120301" ~ "Servicios financieros",
                               cod_subpartida == "13120304" ~ "Servicios de recaudación",
                               cod_subpartida == "13120309" ~ "Otros servicios financieros y de seguros", # nuevo
                               
                               
                               cod_subpartida == "13120401" ~ "Alquiler de edificos e instalaciones",
                               cod_subpartida == "13120409" ~ "Otros alquileres",
                               
                               
                               cod_subpartida == "13120901" ~ "Servicios de formación y capacitación",
                               cod_subpartida == "13120902" ~ "Servicios de investigación y desarrollo",
                               
                               cod_subpartida == "13120904" ~ "Servicios culturales y recreativos",
                               
                               cod_subpartida == "13120906" ~ "Servicios de publicidad e impresión",
                               cod_subpartida == "13120909" ~ "Venta de otros servicios",
                               
                               cod_subpartida == "13130101" ~ "Derechos administrativos a los servicios de transporte por carretera",
                               cod_subpartida == "13130103" ~ "Derechos administrativos a los servicios de transporte portuario",
                               cod_subpartida == "13130104" ~ "Derechos administrativos a los servicios de transporte aeroportuario",
                               
                               
                               
                               cod_subpartida == "13130201" ~ "Cánones por regulación de los servicios públicos",
                               cod_subpartida == "13130202" ~ "Derechos administrativos a los servicios de educación", # nuevo
                               
                               
                               cod_subpartida == "13130203" ~ "Derechos administrativos a actividades comerciales",
                               cod_subpartida == "13130209" ~ "Otros derechos administrativos a otros servicios públicos",
                               
                               
                               cod_subpartida == "13220101" ~ "Concesión de obra pública CTAC",# nuevo
                               cod_subpartida == "13220201" ~ "Alquiler de terrenos CTAC",# nuevo
                               cod_subpartida == "13220202" ~ "Alquiler de Terrenos SINAC",# nuevo
                               cod_subpartida == "13220901" ~ "Concesión Soda Comedor Registro Nacional",# nuevo
                               cod_subpartida == "13220902" ~ "Otros Ingresos de la Renta de la Propiedad SINAC",# nuevo
                               
                               
                               cod_subpartida == "13230101" ~ "Intereses p otros equivalentes efect sect púb int", #NS
                               cod_subpartida == "13230106" ~ "Intereses sobre títulos valores de Instituciones Públicas Financieras",
                               
                               
                               cod_subpartida == "13230205" ~ "Intereses sobre préstamos a empresas públicas no financieras",
                               cod_subpartida == "13230206" ~ "Intereses sobre préstamos a instituciones públicas financieras",
                               
                               cod_subpartida == "13230301" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                               
                               cod_subpartida == "13230304" ~ "Diferencias por tipo de cambio", # nuevo
                               
                               cod_subpartida == "13310201" ~ "Impuestos Internos", 
                               cod_subpartida == "13310202" ~ "Impuestos Aduanas", 
                               
                               cod_subpartida == "13310401" ~ "Sanciones Administrativas y Otros Ley No. 7092",
                               cod_subpartida == "13310402" ~ "Ejecución de garantías de cumplimiento y participación",
                               cod_subpartida == "13310403" ~ "Sanciones administrativas y judiciales FODESAF",
                               
                               cod_subpartida == "13310901" ~ "Otras multas", #NS
                               cod_subpartida == "13310902" ~ "1% Ley Protección al Consumidor",
                               cod_subpartida == "13310903" ~ "Multas por Incumplimiento",
                               cod_subpartida == "13310904" ~ "Infracciones Ley Lab",
                               cod_subpartida == "13310905" ~ "Multas Ley 8246 N° Código de Miniería",
                               cod_subpartida == "13310906" ~ "Multas artículo 36 Ley N° 9028 productos del tabaco",
                               cod_subpartida == "13310907" ~ "Multas por incumplimiento restricción sanitaria COVID-19",
                               cod_subpartida == "13310908" ~ "Multas Código Penal Ley 5386 Patronato de Construc ciones", #nuevo
                               cod_subpartida == "13310909" ~ "Multas Varias",
                               
                               cod_subpartida == "13320101" ~ "Remates",
                               cod_subpartida == "14110100" ~ "Fideicomiso No. 955 Ministerio de Hacienda _ Banco Nacional de Costa Rica",
                               cod_subpartida == "14120101" ~ "Fodesaf-Comedores-Ministerio Educación Pública Ley N° 8783 Artículo 3 inciso e", #NS
                               cod_subpartida == "14120102" ~ "Fodesaf-Ministerio de Obras Públicas y Transportes", #NS
                               cod_subpartida == "14120103" ~ "Fodesaf-Pronae-Ministerio de Trabajo", #NS
                               cod_subpartida == "14120104" ~ "Fodesaf-Pronamype", #NS
                               cod_subpartida == "14120105" ~ "Fodesaf-Régimen no Contribuitivo de Pensiones-CCSS Ley No. 8783", #NS
                               cod_subpartida == "14120106" ~ "Fodesaf-MEP-Comedores Escolares Juntas Ley No. 8783", #NS
                               cod_subpartida == "14120107" ~ "Fodesaf-MEP-Programa Avancemos-Ley No. 8783", #NS
                               cod_subpartida == "14120108" ~ "Fodesaf-IMAS-Jefas de Hogar Ley No. 8783", #NS
                               cod_subpartida == "14120109" ~ "Fodesaf-Desaf- Ley No. 8783", #NS
                               cod_subpartida == "14120110" ~ "Fodesaf Ley 8783", #NS
                               cod_subpartida == "14120111" ~ "Fodesaf-CONAI-L8783", #NS
                               cod_subpartida == "14120112" ~ "Fodesaf-IMAS-L 8783", #NS
                               cod_subpartida == "14120113" ~ "Fodesaf-BANHV-FOSUVI", #NS
                               cod_subpartida == "14120114" ~ "Fodesaf-BANHV-FOSUVI", #NS
                               cod_subpartida == "14120115" ~ "Fodesaf-Mi primer Empleo", #NS
                               cod_subpartida == "14120120" ~ "Fodesaf-Ministerio Obras Públicas y Transporte (previo 2009)", #NS
                               cod_subpartida == "14120130" ~ "Fodesaf-PRONAE (previo 2009)", #NS
                               
                               cod_subpartida == "14130101" ~ "CONICIT",
                               cod_subpartida == "14130102" ~ "Junta Administrativa Colegio San Luis Gonzaga",
                               cod_subpartida == "14130103" ~ "ICT",
                               cod_subpartida == "14130104" ~ "INEC",
                               cod_subpartida == "14130105" ~ "INAMU",
                               cod_subpartida == "14130106" ~ "PANI",
                               cod_subpartida == "14130107" ~ "INDER",
                               cod_subpartida == "14130108" ~ "IMAS",
                               cod_subpartida == "14130109" ~ "SENARA",
                               cod_subpartida == "14130110" ~ "ARESEP",
                               cod_subpartida == "14130111" ~ "INCOPESCA",
                               cod_subpartida == "14130112" ~ "Instituto Nacional de Aprendizaje",
                               cod_subpartida == "14130113" ~ "IFAM",
                               
                               
                               cod_subpartida == "14150101" ~ "AyA",
                               cod_subpartida == "14150102" ~ "ICE",
                               cod_subpartida == "14150103" ~ "INCOOP",
                               cod_subpartida == "14150104" ~ "JAPDEVA",
                               cod_subpartida == "14150105" ~ "CNP",
                               cod_subpartida == "14150106" ~ "INCOFER",
                               
                               cod_subpartida == "14160101" ~ "Banco de Costa Rica",
                               cod_subpartida == "14160102" ~ "Bancrédito", # NS
                               cod_subpartida == "14160103" ~ "Banco Nacional de Costa Rica",
                               cod_subpartida == "14160104" ~ "INVU",
                               cod_subpartida == "14160105" ~ "INS", # NS
                               cod_subpartida == "14160106" ~ "INFOCOOP",
                               cod_subpartida == "14160202" ~ "Transferencias de aseguradoras públicas para Financ. INEC Ley 9694", # NS
                               
                               # TODOS LOS SIGUIENTES SON NUEVOS #
                               
                               
                               cod_subpartida == "32110104" ~ "-",
                               cod_subpartida == "32110105" ~ "-",
                               cod_subpartida == "32110215" ~ "-",
                               cod_subpartida == "32110218" ~ "-",
                               cod_subpartida == "32110219" ~ "-",
                               
                               cod_subpartida == "32110221" ~ "-",
                               cod_subpartida == "32110222" ~ "-",
                               cod_subpartida == "32110223" ~ "-",
                               cod_subpartida == "32110224" ~ "-",
                               cod_subpartida == "32110226" ~ "-",
                               cod_subpartida == "32110227" ~ "-",
                               cod_subpartida == "32110302" ~ "-",
                               cod_subpartida == "32110307" ~ "-",
                               cod_subpartida == "32110901" ~ "-",
                               cod_subpartida == "32110902" ~ "-",
                               
                               # Nuevas asignaciones según la nomenclatura del PI
                               
                               ####### Remplazar por "-" #######
                               
                               cod_subpartida == "11120104"  ~ "",
                               
                               cod_subpartida == "11130101"  ~ "-",
                               cod_subpartida == "11130102"  ~ "-",
                               
                               cod_subpartida == "11130201"  ~ "",
                               cod_subpartida == "11130202"  ~ "",
                               cod_subpartida == "11150101"  ~ "",
                               
                               cod_subpartida == "11320109"  ~ "",
                               cod_subpartida == "12130401"  ~ "",
                               
                               cod_subpartida == "24160201"  ~ "",
                               cod_subpartida == "32110214"  ~ "",
                               cod_subpartida == "32110216"  ~ "",
                               cod_subpartida == "32110217"  ~ "",
                               cod_subpartida == "32110225"  ~ "Crédito BID N° 4819/OC-CR Programa de Apoyo a la Sostenibilidad Fiscal Ley N° 9754",
                               cod_subpartida == "32110304"  ~ "",
                               cod_subpartida == "32110305"  ~ "",
                               
                               ####### Otros  Remplazar por "-"  ############
                               
                               cod_subpartida == "11120101" ~ "",
                               cod_subpartida == "11120201" ~ "",
                               cod_subpartida == "11110301" ~ "",
                               
                               cod_subpartida == "11110101" ~ "",
                               cod_subpartida == "11110201" ~ "",
                               cod_subpartida == "11120105" ~ "",
                               
                               cod_subpartida == "11120105" ~ "",
                               
                               cod_subpartida == "11120105" ~ "",
                               
                               cod_subpartida == "11410202" ~ "",
                               
                               cod_subpartida == "12130101" ~ "",
                               cod_subpartida == "12130102" ~ "",
                               cod_subpartida == "12130201" ~ "",
                               cod_subpartida == "12130201" ~ "",
                               cod_subpartida == "12130301" ~ "",
                               cod_subpartida == "12130302" ~ "",
                               
                               cod_subpartida == "13110101" ~ "",
                               cod_subpartida == "13110903" ~ "",
                               cod_subpartida == "13110904" ~ "",
                               cod_subpartida == "13110905" ~ "",
                               cod_subpartida == "13120101" ~ "",
                               cod_subpartida == "13120104" ~ "",
                               cod_subpartida == "13120309" ~ "",
                               cod_subpartida == "13120409" ~ "",
                               cod_subpartida == "13120409" ~ "",
                               cod_subpartida == "13120409" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120904" ~ "",
                               cod_subpartida == "13120906" ~ "",
                               cod_subpartida == "13130104" ~ "",
                               cod_subpartida == "13130202" ~ "",
                               cod_subpartida == "13130202" ~ "",
                               
                               cod_subpartida == "13220101" ~ "",
                               cod_subpartida == "13220201" ~ "Alquiler de terrenos CTAC",
                               cod_subpartida == "13220202" ~ "",
                               cod_subpartida == "13220901" ~ "",
                               cod_subpartida == "13220902" ~ "",
                               cod_subpartida == "13230304" ~ "",
                               
                               cod_subpartida == "13310908" ~ "",
                               cod_subpartida == "13310910" ~ "Multas Sanciones Remanentes Confiscaciones Teatro Nacional",
                               
                               cod_subpartida == "32110104" ~ "",
                               cod_subpartida == "32110105" ~ "",
                               cod_subpartida == "32110105" ~ "",
                               cod_subpartida == "32110105" ~ "",
                               cod_subpartida == "32110215" ~ "",
                               cod_subpartida == "32110218" ~ "",
                               cod_subpartida == "32110219" ~ "",
                               cod_subpartida == "32110220" ~ "",
                               cod_subpartida == "32110221" ~ "",
                               cod_subpartida == "32110222" ~ "",
                               cod_subpartida == "32110223" ~ "",
                               cod_subpartida == "32110224" ~ "",
                               cod_subpartida == "32110226" ~ "",
                               cod_subpartida == "32110227" ~ "",
                               cod_subpartida == "32110228" ~ "",
                               cod_subpartida == "32110302" ~ "",
                               cod_subpartida == "32110307" ~ "",
            
                               cod_subpartida == "32110901" ~ "",
                               cod_subpartida == "32110902" ~ "",
                               cod_subpartida == "32110902" ~ "",
                               cod_subpartida == "32110902" ~ "",
                               
                               # Nuevos 
                               
                               cod_subpartida == "32110308"  ~ "",
                               cod_subpartida == "32110309" ~ "",
                               
                               ####### Nombre en la jerarquía, poner un "-" #######
                               
                               cod_subpartida == "32120010"  ~ "-",
                               cod_subpartida == "32120020"  ~ "-",
                               cod_subpartida == "32120300"  ~ "-",
                               cod_subpartida == "31310100"  ~ "-",
                               cod_subpartida == "31310200"  ~ "-",
                               cod_subpartida == "33210100"  ~ "-",
                               cod_subpartida == "33110100"  ~ "-",
                               cod_subpartida == "12190500"  ~ "-",
                               cod_subpartida == "12190400"  ~ "-",
                               cod_subpartida == "12190300"  ~ "-",
                               cod_subpartida == "12190600"  ~ "-",
                               cod_subpartida == "12190200"  ~ "-",
                               cod_subpartida == "12190700"  ~ "-",
                               cod_subpartida == "13210300"  ~ "-",
                               cod_subpartida == "13410000"  ~ "-",
                               cod_subpartida == "13490100"  ~ "-",
                               cod_subpartida == "13310100"  ~ "-",
                               cod_subpartida == "13310200"  ~ "Multas de tránsito. Ley No. 9078 COSEVI",
                               cod_subpartida == "13990000"  ~ "-",
                               cod_subpartida == "13990100"  ~ "-",
                               cod_subpartida == "13990200"  ~ "-",
                               cod_subpartida == "13990300"  ~ "-",
                               cod_subpartida == "13990400"  ~ "-",
                               cod_subpartida == "13910000"  ~ "-",
                               cod_subpartida == "11140100"  ~ "-",
                               cod_subpartida == "11120200"  ~ "-",
                               cod_subpartida == "11210100"  ~ "-",
                               cod_subpartida == "11220100"  ~ "-",
                               cod_subpartida == "11220200"  ~ "-",
                               cod_subpartida == "11230300"  ~ "-",
                               cod_subpartida == "11230100"  ~ "-",
                               cod_subpartida == "11240100"  ~ "-",
                               cod_subpartida == "11250100"  ~ "-",
                               cod_subpartida == "11910500"  ~ "-",
                               cod_subpartida == "11910800"  ~ "-",
                               cod_subpartida == "11910700"  ~ "-",
                               cod_subpartida == "11911200"  ~ "-",
                               cod_subpartida == "11910200"  ~ "-",
                               cod_subpartida == "11910900"  ~ "-",
                               cod_subpartida == "11911000"  ~ "-",
                               cod_subpartida == "11910100"  ~ "-",
                               cod_subpartida == "11910600"  ~ "-",
                               cod_subpartida == "11910400"  ~ "-",
                               cod_subpartida == "11911300"  ~ "-",
                               cod_subpartida == "11911100"  ~ "-",
                               cod_subpartida == "11910300"  ~ "-",
                               cod_subpartida == "11990100"  ~ "-",
                               cod_subpartida == "14310400"  ~ "-",
                               cod_subpartida == "14250000"  ~ "-",
                               cod_subpartida == "14260000"  ~ "-",
                               cod_subpartida == "14150800"  ~ "-",
                               cod_subpartida == "14150700"  ~ "-",
                               cod_subpartida == "14150600"  ~ "-",
                               cod_subpartida == "14150500"  ~ "-",
                               cod_subpartida == "14140600"  ~ "-",
                               cod_subpartida == "14140500"  ~ "-",
                               cod_subpartida == "14140400"  ~ "-",
                               cod_subpartida == "14140100"  ~ "-",
                               cod_subpartida == "14130200"  ~ "-",
                               cod_subpartida == "14130600"  ~ "-",
                               cod_subpartida == "14130400"  ~ "-",
                               cod_subpartida == "14130300"  ~ "-",
                               cod_subpartida == "14130900"  ~ "-",
                               cod_subpartida == "14131100"  ~ "-",
                               cod_subpartida == "14131300"  ~ "-",
                               cod_subpartida == "14131200"  ~ "-",
                               cod_subpartida == "14131000"  ~ "-",
                               cod_subpartida == "14131400"  ~ "-",
                               cod_subpartida == "14151000"  ~ "-",
                               cod_subpartida == "14160400"  ~ "-",
                               cod_subpartida == "14121400"  ~ "-",
                               cod_subpartida == "14121500"  ~ "-",
                               cod_subpartida == "14110200"  ~ "-",
                               cod_subpartida == "24130100"  ~ "-"
                               
                             ),            
                             
                             cod_renglon = substr(Ingresos_m$`PosPre`, 2, 10),
                             
                             renglon = case_when(
                               
                               #######  Asignaciones  ############
                               
                               
                               cod_renglon == "113101011" ~ "Impuesto de Ventas Ley N° 7543", # NS
                               
                               cod_renglon == "113101021" ~ "Impuesto de ventas bienes y servicios importados",
                               
                               
                               cod_renglon == "113102011" ~ "Impuesto selectivo de consumo de bienes internos",
                               
                               cod_renglon == "113102021" ~ "Impuesto selectivo de consumo de bienes importados",
                               
                               cod_renglon == "113201021" ~ "Licencia de caza y pesca continental",
                               cod_renglon == "113201022" ~ "Licencia de caza y pesca MIRENEM",
                               
                               cod_renglon == "113201031" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14",
                               
                               
                               cod_renglon == "113201041" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972",
                               cod_renglon == "113201042" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114",
                               cod_renglon == "113201043" ~ "Impuestos específicos sobre los jabones de tocador . Ley Nº 8114",
                               cod_renglon == "113201044" ~ "Impuesto al cemento",
                               cod_renglon == "113201045" ~ "Impuesto a los productos de tabaco", # Nuevo nombre
                               
                               cod_renglon == "113201091" ~ "Impuestos Ley No.6883 y Ley No. 8495 SENASA",
                               cod_renglon == "113202021" ~ "Permiso de Transporte carga liviana COSEVI",
                               cod_renglon == "113202031" ~ "Impuesto sobre casinos Ley N° 9050",
                               cod_renglon == "113202032" ~ "Impuesto sobre empresas de apuestas electrónicas Ley N° 9050",
                               
                               cod_renglon == "113202033" ~ "Impuestos espectáculos públicos leyes 3632 y 5780 Teatro Nacional",
                               
                               cod_renglon == "114309011" ~ "Fondo Social Migratorio L8764",
                               cod_renglon == "114309012" ~ "Fondo Especial de Migración",
                               cod_renglon == "114309013" ~ "Otros Impuestos Migratorios",
                               
                               cod_renglon == "131201011" ~ "Acarreo de vehículos detenidos COSEVI",
                               
                               cod_renglon == "131204011" ~ "Alquiler de edificios",
                               cod_renglon == "131209011" ~ "Servicios capacitación CGR",
                               cod_renglon == "131209021" ~ "Serv Metrolog MEIC",
                               cod_renglon == "131209022" ~ "Serv Amb SETENA",
                               cod_renglon == "131209091" ~ "Costo Doc Migrator.",
                               cod_renglon == "131209092" ~ "Venta Ser. IGN-MOPT",
                               
                               cod_renglon == "131301011" ~ "Licencia de Conducir Ley N° 7055",
                               cod_renglon == "131301012" ~ "Canon CTP Ley N° 7969",
                               cod_renglon == "131301031" ~ "Rev Barcos Cap Puert",
                               cod_renglon == "131301032" ~ "Der. Registro Naval",
                               cod_renglon == "131301033" ~ "Der Zarpe Embar Ext.",
                               cod_renglon == "131301034" ~ "Canon Cert Navegab.",
                               
                               cod_renglon == "131302031" ~ "Radio y TV Ley N° 1758",
                               cod_renglon == "131302032" ~ "Frecuencias de Radio Ley N° 1758",
                               cod_renglon == "131302034" ~ "Conc.explo.min L8246",
                               cod_renglon == "131302035" ~ "Derechos de Inscripción de Gestión de Residuos Ley 8839",
                               cod_renglon == "131302091" ~ "PERMISOS Y RENOVACION DE CEDULAS DE RESIDENCIA. (LEYES N? 7033 Y 7108 DE 13-8-86 Y 8-11-88).",
                               
                               cod_renglon == "132301061" ~ "Intereses bonos INVU",
                               cod_renglon == "132302051" ~ "Deuda renegociada AyA",
                               cod_renglon == "132302061" ~ "Deuda renegociada BCCR",
                               
                               # Siguientes son nuevos
                               
                               cod_renglon == "131201041" ~ "Servicio de transporte aeroportuario CTAC",
                               
                               cod_renglon == "131203091" ~ "Otros servicios financieros y de seguros COSEVI",
                               cod_renglon == "131204011" ~ "Alquiler de edificios",
                               cod_renglon == "131204012" ~ "Alquiler de edificios CTAC",
                               cod_renglon == "131204013" ~ "Alquiler de edificios Registro Nacional",
                               cod_renglon == "131204014" ~ "Alquiler de edificios Casa Cultura Puntarenas",
                               cod_renglon == "131204015" ~ "Alquiler de edificios Centro Nacional de la Música",
                               cod_renglon == "131204016" ~ "Alquiler de edificios Teatro Nacional",
                               cod_renglon == "131204017" ~ "Alquiler de edificios Teatro Melico Salazar",
                               cod_renglon == "131204091" ~ "Custodia de Vehículos COSEVI",
                               cod_renglon == "131204092" ~ "Apartados Registro Nacional",
                               cod_renglon == "131204093" ~ "Otros Alquileres Teatro Melico Salazar",
                               cod_renglon == "131209012" ~ "Venta Servicios Capacitación Archivo Nacional",
                               cod_renglon == "131209021" ~ "Venta de servicios metrológicos (MEIC) Ley Nº 8279",
                               cod_renglon == "131209022" ~ "Serv Amb SETENA",
                               cod_renglon == "131209024" ~ "Servicios de Análisis Laboratorio INTA",
                               cod_renglon == "131209025" ~ "Venta de servicios ambientales FONAFIFO",
                               cod_renglon == "131209041" ~ "Derechos de entrada a parques",
                               cod_renglon == "131209042" ~ "Servicios culturales y recreativos Centro Nacional de la Música",
                               cod_renglon == "131209043" ~ "Servicios culturales y recreativos Museo Nacional",
                               cod_renglon == "131209044" ~ "Venta de entradas y otros Teatro Nacional",
                               cod_renglon == "131209045" ~ "Venta de entradas y otros Teatro Melico Salazar",
                               cod_renglon == "131209046" ~ "Entradas a las Áreas Silvestres Protegidas SINAC",
                               cod_renglon == "131209061" ~ "Servicios de publicidad e impresión",
                               cod_renglon == "131209062" ~ "Servicios de publicidad e impresión Imprenta Nacional",
                               cod_renglon == "131209093" ~ "Honorarios por Servicios de Defensa Civil de la Víctima",
                               cod_renglon == "131209094" ~ "Venta de Servicio de Consulta de datos del TSE",
                               
                               # OJO A
                               
                               cod_renglon == "131209098" ~ "Venta de servicios varios",
                               cod_renglon == "131209099" ~ "Venta de servicios varios",
                               cod_renglon == "131301012" ~ "Canon Consejo de Transporte Público. Ley Nº 7969",
                               cod_renglon == "131301013" ~ "Derechos administrativos a los servicios de transp orte en carretera CNC- Concesión carretera San Jos é-Caldera",
                               cod_renglon == "131301014" ~ "Derechos administrativos a los servicios de transp orte en carretera CONAVI- Peajes carretera Florenc io Castillo",
                               cod_renglon == "131301015" ~ "Derechos administrativos a los servicios de transp orte en carretera CONAVI- Peajes carretera Braulio Carrillo",
                               cod_renglon == "131301031" ~ "Revisión de barcos por Capitanías de Puerto. Resol . Nº 40 del 13-6-52",
                               cod_renglon == "131301032" ~ "Derechos de inscripción en el Registro Naval. Art. 33 Ley Nº 8000",
                               cod_renglon == "131301033" ~ "Derechos de zarpe de embarcaciones extranjeras. Ar t.32 Ley Nº 8000",
                               cod_renglon == "131301034" ~ "Canon por certificado de navegabilidad. Art.31 Ley Nº 8000",
                               cod_renglon == "131301035" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                               cod_renglon == "131301041" ~ "Derechos administrativos a los servicios de transp orte aeroportuario CTAC",
                               cod_renglon == "131302013" ~ "Canon por aprovechamiento de aguas",
                               cod_renglon == "131302014" ~ "Canon ambiental por vertidos",
                               cod_renglon == "131302021" ~ "Matrícula Exámenes Teóricos y Pruebas Prácticas COSEVI",
                               cod_renglon == "131302022" ~ "Matrícula estudiantes Centro Nacional de la Música",
                               cod_renglon == "131302031" ~ "Radio y Televisión Ley 1758",
                               cod_renglon == "131302032" ~ "Frecuencias Radio Ley 1758",
                               cod_renglon == "131302034" ~ "Concesión de Explotación minera ley N°8246",
                               cod_renglon == "131302035" ~ "Derechos de Inscripción de Gestión de Residuos",
                               cod_renglon == "131302036" ~ "Cánones y Multas y Sanciones Administrativas PROHA B",
                               cod_renglon == "131302037" ~ "Derechos Administrativos a actividades comerciales SINAC",
                               cod_renglon == "131302092" ~ "Emisión de Licencias de Conducir COSEVI",
                               cod_renglon == "131302093" ~ "Registro e inscripción CANNON-CONIS",
                               cod_renglon == "131302094" ~ "Derecho de Filmación SINAC",
                               
                               
                               cod_renglon == "131109043" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                               
                               cod_renglon == "113201091" ~ "",
                               cod_renglon == "113202021" ~ "",
                               cod_renglon == "113202033" ~ "",
                               
                               cod_renglon == "131109043" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                               
                               cod_renglon == "131201011" ~ "Acarreo de vehículos detenidos COSEVI",
                               cod_renglon == "131201041" ~ "",
                               cod_renglon == "131203091" ~ "",
                               cod_renglon == "131204012" ~ "",
                               cod_renglon == "131204013" ~ "",
                               cod_renglon == "131204014" ~ "",
                               cod_renglon == "131204015" ~ "",
                               cod_renglon == "131204016" ~ "",
                               cod_renglon == "131204017" ~ "",
                               cod_renglon == "131204091" ~ "",
                               cod_renglon == "131204092" ~ "",
                               cod_renglon == "131204093" ~ "",
                               cod_renglon == "131209012" ~ "",
                               cod_renglon == "131209023" ~ "Ingresos por inspección",
                               cod_renglon == "131209024" ~ "",
                               cod_renglon == "131209025" ~ "",
                               cod_renglon == "131209041" ~ "",
                               cod_renglon == "131209042" ~ "",
                               cod_renglon == "131209043" ~ "",
                               cod_renglon == "131209044" ~ "",
                               cod_renglon == "131209045" ~ "",
                               cod_renglon == "131209046" ~ "",
                               cod_renglon == "131209061" ~ "",
                               cod_renglon == "131209062" ~ "",
                               cod_renglon == "131209093" ~ "",
                               cod_renglon == "131209094" ~ "",
                               cod_renglon == "131209098" ~ "Venta de servicios varios",
                          
                               cod_renglon == "131209099" ~ "Venta de servicios varios",
                               cod_renglon == "131301013" ~ "",
                               cod_renglon == "131301014" ~ "",
                               cod_renglon == "131301015" ~ "",
                               cod_renglon == "131301035" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                               cod_renglon == "131301041" ~ "",
                               cod_renglon == "131302013" ~ "",
                               cod_renglon == "131302014" ~ "",
                               cod_renglon == "131302021" ~ "",
                               cod_renglon == "131302022" ~ "",
                               cod_renglon == "131302036" ~ "",
                               cod_renglon == "131302037" ~ "",
                               cod_renglon == "131302092" ~ "",
                               cod_renglon == "131302093" ~ "",
                               cod_renglon == "131302094" ~ "",
                               cod_renglon == "133109010" ~ "Multas Sugeval",
                               cod_renglon == "133109011" ~ "Otras multas",
                               cod_renglon == "133109012" ~ "Multas por incumplimiento (2007)",
                               cod_renglon == "133109013" ~ "Multas de Aduanas (2007)",
                               cod_renglon == "133109014" ~ "1% SOBRE UTILIDADES PROTECCION AL CONSUMIDOR (2007)",
                               cod_renglon == "133109015" ~ "MULTA SEGUN ART. 158 INCISO 3 DE LA LEY REGULADORA DEL MERCADO DE VALORES (200 SALARIOS BASES)",
                               
                               ####### Nombre en la jerarquía, poner un "-" #######
                               
                               cod_renglon == "321200200" ~ "-",
                               cod_renglon == "321200100" ~ "-",
                               cod_renglon == "321203000" ~ "-",
                               cod_renglon == "321101050" ~ "-",
                               cod_renglon == "321101040" ~ "-",
                               cod_renglon == "321102220" ~ "-",
                               cod_renglon == "321102240" ~ "-",
                               cod_renglon == "321102210" ~ "-",
                               cod_renglon == "321102230" ~ "-",
                               cod_renglon == "321102270" ~ "-",
                               cod_renglon == "321102260" ~ "-",
                               cod_renglon == "321102150" ~ "-",
                               cod_renglon == "321102180" ~ "-",
                               cod_renglon == "321102190" ~ "-",
                               cod_renglon == "321102200" ~ "-",
                               cod_renglon == "321103020" ~ "-",
                               cod_renglon == "321103070" ~ "-",
                               cod_renglon == "321103080" ~ "-",
                               cod_renglon == "321109020" ~ "-",
                               cod_renglon == "321109010" ~ "-",
                               cod_renglon == "313101000" ~ "-",
                               cod_renglon == "313102000" ~ "-",
                               cod_renglon == "332101000" ~ "-",
                               cod_renglon == "331101000" ~ "-",
                               cod_renglon == "121303020" ~ "-",
                               cod_renglon == "121303010" ~ "-",
                               cod_renglon == "121301010" ~ "-",
                               cod_renglon == "121301020" ~ "-",
                               cod_renglon == "121302010" ~ "-",
                               cod_renglon == "121905000" ~ "-",
                               cod_renglon == "121904000" ~ "-",
                               cod_renglon == "121903000" ~ "-",
                               cod_renglon == "121906000" ~ "-",
                               cod_renglon == "121902000" ~ "-",
                               cod_renglon == "121907000" ~ "-",
                               cod_renglon == "132301060" & Año < 2021~ "-",
                               cod_renglon == "132301060" & Año >= 2021 ~ "Intereses por títulos y valores de Inst Púb Financ ICD",
                               cod_renglon == "132201010" ~ "-",
                               cod_renglon == "132209010" ~ "-",
                               
                               cod_renglon == "132303040" ~ "Diferencias por tipo de cambio",
                               cod_renglon == "132303010" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                               cod_renglon == "132202010" ~ "-",
                               cod_renglon == "132202020" ~ "-",
                               cod_renglon == "132209020" ~ "-",
                               cod_renglon == "132103000" ~ "-",
                               cod_renglon == "134100000" ~ "-",
                               cod_renglon == "134901000" ~ "-",
                               cod_renglon == "133101000" ~ "-",
                               cod_renglon == "133102000" ~ "-",
                               cod_renglon == "133102020" ~ "-",
                               cod_renglon == "133102010" ~ "-",
                               cod_renglon == "133109020" ~ "-",
                               cod_renglon == "133109040" ~ "-",
                               cod_renglon == "133109080" ~ "-",
                               cod_renglon == "133109050" ~ "-",
                               cod_renglon == "133109030" ~ "-",
                               cod_renglon == "133109070" ~ "-",
                               cod_renglon == "133109090" ~ "-",
                               cod_renglon == "133109100" ~ "-",
                               cod_renglon == "133104020" ~ "-",
                               cod_renglon == "133104010" ~ "-",
                               cod_renglon == "133201010" ~ "-",
                               cod_renglon == "139900000" ~ "-",
                               cod_renglon == "139901000" ~ "-",
                               cod_renglon == "139902000" ~ "-",
                               cod_renglon == "139903000" ~ "-",
                               cod_renglon == "139904000" ~ "-",
                               cod_renglon == "139100000" ~ "-",
                               cod_renglon == "131101010" ~ "-",
                               cod_renglon == "131109020" ~ "-",
                               cod_renglon == "131109050" ~ "-",
                               cod_renglon == "131109030" ~ "-",
                               cod_renglon == "131203040" ~ "Servicios de recaudación",
                               cod_renglon == "131203010" ~ "Servicios financieros",
                               cod_renglon == "111401000" ~ "-",
                               cod_renglon == "111102010" ~ "-",
                               cod_renglon == "111101010" ~ "-",
                               cod_renglon == "111103040" ~ "-",
                               cod_renglon == "111103010" ~ "-",
                               cod_renglon == "111103020" ~ "-",
                               cod_renglon == "111103030" ~ "-",
                               cod_renglon == "111202000" ~ "-",
                               cod_renglon == "111202040" ~ "-",
                               cod_renglon == "111202010" ~ "-",
                               cod_renglon == "111202020" ~ "-",
                               cod_renglon == "111202030" ~ "-",
                               cod_renglon == "111201050" ~ "-",
                               cod_renglon == "111201010" ~ "-",
                               cod_renglon == "111201020" ~ "-",
                               cod_renglon == "111201030" ~ "-",
                               cod_renglon == "113101020" ~ "-",
                               cod_renglon == "113101010" ~ "-",
                               cod_renglon == "113102020" ~ "-",
                               cod_renglon == "113102010" ~ "-",
                               cod_renglon == "114201010" ~ "-",
                               cod_renglon == "114209010" ~ "-",
                               cod_renglon == "114209020" ~ "-",
                               cod_renglon == "114101010" ~ "-",
                               cod_renglon == "114102010" ~ "-",
                               cod_renglon == "114102020" ~ "-",
                               cod_renglon == "114304010" ~ "-",
                               cod_renglon == "114303010" ~ "-",
                               cod_renglon == "114303020" ~ "-",
                               cod_renglon == "114309020" ~ "-",
                               cod_renglon == "112101000" ~ "-",
                               cod_renglon == "112201000" ~ "-",
                               cod_renglon == "112202000" ~ "-",
                               cod_renglon == "112303000" ~ "-",
                               cod_renglon == "112301000" ~ "-",
                               cod_renglon == "112401000" ~ "-",
                               cod_renglon == "112501000" ~ "-",
                               cod_renglon == "119105000" ~ "-",
                               cod_renglon == "119108000" ~ "-",
                               cod_renglon == "119107000" ~ "-",
                               cod_renglon == "119112000" ~ "-",
                               cod_renglon == "119102000" ~ "-",
                               cod_renglon == "119109000" ~ "-",
                               cod_renglon == "119110000" ~ "-",
                               cod_renglon == "119101000" ~ "-",
                               cod_renglon == "119106000" ~ "-",
                               cod_renglon == "119104000" ~ "-",
                               cod_renglon == "119113000" ~ "-",
                               cod_renglon == "119111000" ~ "-",
                               cod_renglon == "119103000" ~ "-",
                               cod_renglon == "119901000" ~ "-",
                               cod_renglon == "143104000" ~ "-",
                               cod_renglon == "142500000" ~ "-",
                               cod_renglon == "142600000" ~ "-",
                               cod_renglon == "141510000" ~ "-",
                               cod_renglon == "141501010" ~ "-",
                               cod_renglon == "141501050" ~ "-",
                               cod_renglon == "141501020" ~ "-",
                               cod_renglon == "141501060" ~ "-",
                               cod_renglon == "141501030" ~ "-",
                               cod_renglon == "141501040" ~ "-",
                               cod_renglon == "141508000" ~ "-",
                               cod_renglon == "141507000" ~ "-",
                               cod_renglon == "141506000" ~ "-",
                               cod_renglon == "141505000" ~ "-",
                               cod_renglon == "141406000" ~ "-",
                               cod_renglon == "141405000" ~ "-",
                               cod_renglon == "141404000" ~ "-",
                               cod_renglon == "141401000" ~ "-",
                               cod_renglon == "141302000" ~ "-",
                               cod_renglon == "141301100" ~ "-",
                               cod_renglon == "141301010" ~ "-",
                               cod_renglon == "141301030" ~ "-",
                               cod_renglon == "141301070" ~ "-",
                               cod_renglon == "141301130" ~ "-",
                               cod_renglon == "141301050" ~ "-",
                               cod_renglon == "141301110" ~ "-",
                               cod_renglon == "141301040" ~ "-",
                               cod_renglon == "141301120" ~ "-",
                               cod_renglon == "141301120" ~ "-",
                               cod_renglon == "141301020" ~ "-",
                               cod_renglon == "141301060" ~ "-",
                               cod_renglon == "141301090" ~ "-",
                               cod_renglon == "141306000" ~ "-",
                               cod_renglon == "141304000" ~ "-",
                               cod_renglon == "141303000" ~ "-",
                               cod_renglon == "141309000" ~ "-",
                               cod_renglon == "141311000" ~ "-",
                               cod_renglon == "141313000" ~ "-",
                               cod_renglon == "141312000" ~ "-",
                               cod_renglon == "141310000" ~ "-",
                               cod_renglon == "141314000" ~ "-",
                               cod_renglon == "141601010" ~ "-",
                               cod_renglon == "141601030" ~ "-",
                               cod_renglon == "141601060" ~ "-",
                               cod_renglon == "141601040" ~ "-",
                               cod_renglon == "141602020" ~ "-",
                               cod_renglon == "141604000" ~ "-",
                               cod_renglon == "141214000" ~ "-",
                               cod_renglon == "141215000" ~ "-",
                               cod_renglon == "141102000" ~ "-",
                               cod_renglon == "241301000" ~ "-"
                               
                             ),
                             
                             cod_subrenglon = substr(Ingresos_m$`PosPre`, 2, 11),
                             
                             subrenglon = case_when(
                               
                               # Existentes
                               
                               cod_subrenglon == "1132010311" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14 (interno)",
                               cod_subrenglon == "1132010312" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14 (importaciones)",
                               cod_subrenglon == "1132010411" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972 (Interno)",
                               cod_subrenglon == "1132010412" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972 (Importaciones)",
                               cod_subrenglon == "1132010421" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114 (Interno)",
                               cod_subrenglon == "1132010422" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114 (Importaciones)",
                               cod_subrenglon == "1132010431" ~ "Impuestos específicos sobre los jabones de tocador . Ley Nº 8114 (Importaciones)",
                               
                               cod_subrenglon == "1132010452" ~ "Impuesto a los productos de tabaco (Importaciones)",
                               
                               cod_subrenglon == "1312030101" ~ "Costo Transferencias SWIFT" ,
                               cod_subrenglon == "1312030102" ~ "Servicio de Recaudación de Tesorería Nacional" ,
                               cod_subrenglon == "1312030401" ~ "Servicio Recaudación Tesorería Nacional" ,
                               cod_subrenglon == "1312030402" ~ "Servicios tributarios Ley N°9355" ,
                               cod_subrenglon == "1312030403" ~ "Servicios de recaudación Teatro Nacional" ,
                               
                               cod_subrenglon == "1312090980" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)" ,
                               cod_subrenglon == "1312090981" ~ "Venta de Servicios Registrales  Registro Nacional" ,
                               cod_subrenglon == "1312090982" ~ "Venta de Servicios Direccion Nacional Notariado" ,
                               cod_subrenglon == "1312090983" ~ "Venta de Servicios SENASA" ,
                               cod_subrenglon == "1312090984" ~ "Venta de servicios varios Casa de Cultura Puntarenas" ,
                               cod_subrenglon == "1312090985" ~ "Venta de servicios varios Archivo Nacional" ,
                               cod_subrenglon == "1312090986" ~ "Venta de servicios Museo Nacional" ,
                               cod_subrenglon == "1312090987" ~ "Cursos libres Teatro Melico Salazar" ,
                               cod_subrenglon == "1312090988" ~ "Uso de instalaciones en las Áreas Silvestres Protegidas SINAC" ,
                               cod_subrenglon == "1312090990" ~ "Venta de otros servicios SFE" ,
                               cod_subrenglon == "1312090991" ~ "Venta de otros servicios INCIENSA" ,
                               
                               cod_subrenglon == "1323030101" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos" ,
                               cod_subrenglon == "1323030102" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales COSEVI" ,
                               cod_subrenglon == "1323030103" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales FODESAF" ,
                               cod_subrenglon == "1323030104" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Patronato Construcciones" ,
                               cod_subrenglon == "1323030106" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Teatro Nacional" ,
                               cod_subrenglon == "1323030401" ~ "Diferencias por tipo de cambio SINAC" ,
                               
                               # Sin un código dado el nivel
                               
                               
                               cod_subrenglon == "1323030401" ~ "Diferencias por tipo de cambio SINAC",
                               
                               cod_subrenglon == "1323030101" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                               cod_subrenglon == "1323030102" ~ "",
                               cod_subrenglon == "1323030103" ~ "",
                               cod_subrenglon == "1323030104" ~ "",
                               cod_subrenglon == "1323030106" ~ "",
                               
                               cod_subrenglon == "1312090981" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)",
                               cod_subrenglon == "1312090981" ~ "",
                               cod_subrenglon == "1312090982" ~ "",
                               cod_subrenglon == "1312090983" ~ "",
                               cod_subrenglon == "1312090984" ~ "",
                               cod_subrenglon == "1312090985" ~ "",
                               cod_subrenglon == "1312090986" ~ "",
                               cod_subrenglon == "1312090987" ~ "",
                               cod_subrenglon == "1312090988" ~ "",
                               cod_subrenglon == "1312090990" ~ "",
                               
                               cod_subrenglon == "1312030401" ~ "",
                               cod_subrenglon == "1312030402" ~ "",
                               cod_subrenglon == "1312030403" ~ "",
                               cod_subrenglon == "1312030101" ~ "",
                               cod_subrenglon == "1312030102" ~ "Servicio de Recaudación de Tesorería Nacional",
                               
                               ####### Nombre en la jerarquía, poner un "-" #######
                               
                               cod_subrenglon == "3212001000" ~ "-",
                               cod_subrenglon == "3212002000" ~ "-",
                               cod_subrenglon == "3212030000" ~ "-",
                               cod_subrenglon == "3211010500" ~ "-",
                               cod_subrenglon == "3211010400" ~ "-",
                               cod_subrenglon == "3211022200" ~ "-",
                               cod_subrenglon == "3211022400" ~ "-",
                               cod_subrenglon == "3211022100" ~ "-",
                               cod_subrenglon == "3211022300" ~ "-",
                               cod_subrenglon == "3211022700" ~ "-",
                               cod_subrenglon == "3211022600" ~ "-",
                               cod_subrenglon == "3211021500" ~ "-",
                               cod_subrenglon == "3211021800" ~ "-",
                               cod_subrenglon == "3211021900" ~ "-",
                               cod_subrenglon == "3211022000" ~ "-",
                               cod_subrenglon == "3211030200" ~ "-",
                               cod_subrenglon == "3211030700" ~ "-",
                               cod_subrenglon == "3211030800" ~ "-",
                               cod_subrenglon == "3211090200" ~ "-",
                               cod_subrenglon == "3211090100" ~ "-",
                               cod_subrenglon == "3131010000" ~ "-",
                               cod_subrenglon == "3131020000" ~ "-",
                               cod_subrenglon == "3321010000" ~ "-",
                               cod_subrenglon == "3311010000" ~ "-",
                               cod_subrenglon == "1213030200" ~ "-",
                               cod_subrenglon == "1213030100" ~ "-",
                               cod_subrenglon == "1213010100" ~ "-",
                               cod_subrenglon == "1213010200" ~ "-",
                               cod_subrenglon == "1213020100" ~ "-",
                               cod_subrenglon == "1219050000" ~ "-",
                               cod_subrenglon == "1219040000" ~ "-",
                               cod_subrenglon == "1219030000" ~ "-",
                               cod_subrenglon == "1219060000" ~ "-",
                               cod_subrenglon == "1219020000" ~ "-",
                               cod_subrenglon == "1219070000" ~ "-",
                               cod_subrenglon == "1323010600" ~ "-",
                               cod_subrenglon == "1323030401" ~ "-",
                               cod_subrenglon == "1323030100" ~ "",
                               cod_subrenglon == "1322020100" ~ "-",
                               cod_subrenglon == "1322020200" ~ "-",
                               cod_subrenglon == "1322010100" ~ "-",
                               cod_subrenglon == "1322090100" ~ "-",
                               cod_subrenglon == "1322090200" ~ "-",
                               cod_subrenglon == "1321030000" ~ "-",
                               cod_subrenglon == "1341000000" ~ "-",
                               cod_subrenglon == "1349010000" ~ "-",
                               cod_subrenglon == "1331010000" ~ "-",
                               cod_subrenglon == "1331020000" ~ "-",
                               cod_subrenglon == "1331020200" ~ "-",
                               cod_subrenglon == "1331020100" ~ "-",
                               cod_subrenglon == "1331090200" ~ "-",
                               cod_subrenglon == "1331090400" ~ "-",
                               cod_subrenglon == "1331090800" ~ "-",
                               cod_subrenglon == "1331090500" ~ "-",
                               cod_subrenglon == "1331090300" ~ "-",
                               cod_subrenglon == "1331090700" ~ "-",
                               cod_subrenglon == "1331090900" ~ "-",
                               cod_subrenglon == "1331091000" ~ "-",
                               cod_subrenglon == "1331040200" ~ "-",
                               cod_subrenglon == "1331040100" ~ "-",
                               cod_subrenglon == "1332010100" ~ "-",
                               cod_subrenglon == "1399000000" ~ "-",
                               cod_subrenglon == "1399010000" ~ "-",
                               cod_subrenglon == "1399020000" ~ "-",
                               cod_subrenglon == "1399030000" ~ "-",
                               cod_subrenglon == "1399040000" ~ "-",
                               cod_subrenglon == "1391000000" ~ "-",
                               cod_subrenglon == "1313010410" ~ "-",
                               cod_subrenglon == "1313010120" ~ "-",
                               cod_subrenglon == "1313010130" ~ "-",
                               cod_subrenglon == "1313010150" ~ "-",
                               cod_subrenglon == "1313010140" ~ "-",
                               cod_subrenglon == "1313010110" ~ "-",
                               cod_subrenglon == "1313010340" ~ "-",
                               cod_subrenglon == "1313010350" ~ "-",
                               cod_subrenglon == "1313010330" ~ "-",
                               cod_subrenglon == "1313010320" ~ "-",
                               cod_subrenglon == "1313010310" ~ "-",
                               cod_subrenglon == "1313020140" ~ "-",
                               cod_subrenglon == "1313020130" ~ "-",
                               cod_subrenglon == "1313020360" ~ "-",
                               cod_subrenglon == "1313020340" ~ "-",
                               cod_subrenglon == "1313020370" ~ "-",
                               cod_subrenglon == "1313020350" ~ "-",
                               cod_subrenglon == "1313020320" ~ "-",
                               cod_subrenglon == "1313020310" ~ "-",
                               cod_subrenglon == "1313020220" ~ "-",
                               cod_subrenglon == "1313020210" ~ "-",
                               cod_subrenglon == "1313020940" ~ "-",
                               cod_subrenglon == "1313020920" ~ "-",
                               cod_subrenglon == "1313020930" ~ "-",
                               cod_subrenglon == "1311010100" ~ "-",
                               cod_subrenglon == "1311090200" ~ "-",
                               cod_subrenglon == "1311090500" ~ "-",
                               cod_subrenglon == "1311090430" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                               cod_subrenglon == "1311090300" ~ "-",
                               cod_subrenglon == "1312040110" ~ "-",
                               cod_subrenglon == "1312040140" ~ "-",
                               cod_subrenglon == "1312040150" ~ "-",
                               cod_subrenglon == "1312040120" ~ "-",
                               cod_subrenglon == "1312040130" ~ "-",
                               cod_subrenglon == "1312040170" ~ "-",
                               cod_subrenglon == "1312040160" ~ "-",
                               cod_subrenglon == "1312040920" ~ "-",
                               cod_subrenglon == "1312040910" ~ "-",
                               cod_subrenglon == "1312040930" ~ "-",
                               cod_subrenglon == "1312090220" ~ "-",
                               cod_subrenglon == "1312090210" ~ "-",
                               cod_subrenglon == "1312090240" ~ "-",
                               cod_subrenglon == "1312090250" ~ "-",
                               cod_subrenglon == "1312090410" ~ "-",
                               cod_subrenglon == "1312090460" ~ "-",
                               cod_subrenglon == "1312090420" ~ "-",
                               cod_subrenglon == "1312090430" ~ "-",
                               cod_subrenglon == "1312090450" ~ "-",
                               cod_subrenglon == "1312090440" ~ "-",
                               cod_subrenglon == "1312090120" ~ "-",
                               cod_subrenglon == "1312090610" ~ "-",
                               cod_subrenglon == "1312090620" ~ "-",
                               cod_subrenglon == "1312090980" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocop ias, certificaciones, etc.)",
                               cod_subrenglon == "1312090930" ~ "-",
                               cod_subrenglon == "1312090990" ~ "-",
                               cod_subrenglon == "1312090991" ~ "-",
                               cod_subrenglon == "1312090940" ~ "-",
                               cod_subrenglon == "1312010410" ~ "-",
                               cod_subrenglon == "1312010110" ~ "-",
                               cod_subrenglon == "1312030910" ~ "-",
                               cod_subrenglon == "1312030401" ~ "-",
                               cod_subrenglon == "1312030403" ~ "-",
                               cod_subrenglon == "1312030402" ~ "-",
                               cod_subrenglon == "1114010000" ~ "-",
                               cod_subrenglon == "1111020100" ~ "-",
                               cod_subrenglon == "1111010100" ~ "-",
                               cod_subrenglon == "1111030400" ~ "-",
                               cod_subrenglon == "1111030100" ~ "-",
                               cod_subrenglon == "1111030200" ~ "-",
                               cod_subrenglon == "1111030300" ~ "-",
                               cod_subrenglon == "1112020000" ~ "-",
                               cod_subrenglon == "1112020400" ~ "-",
                               cod_subrenglon == "1112020100" ~ "-",
                               cod_subrenglon == "1112020200" ~ "-",
                               cod_subrenglon == "1112020300" ~ "-",
                               cod_subrenglon == "1112010500" ~ "-",
                               cod_subrenglon == "1112010100" ~ "-",
                               cod_subrenglon == "1112010200" ~ "-",
                               cod_subrenglon == "1112010300" ~ "-",
                               cod_subrenglon == "1131010200" ~ "-",
                               cod_subrenglon == "1131010100" ~ "-",
                               cod_subrenglon == "1131020200" ~ "-",
                               cod_subrenglon == "1131020100" ~ "-",
                               cod_subrenglon == "1132010432" ~ "Impuestos específicos sobre los jabones de tocador. Ley Nº 8114 (Importaciones)",
                               cod_subrenglon == "1132010440" ~ "-",
                               cod_subrenglon == "1132010451" ~ "Impuesto a los productos de tabaco (Interno)",
                               cod_subrenglon == "1132010910" ~ "-",
                               cod_subrenglon == "1132020310" ~ "-",
                               cod_subrenglon == "1132020320" ~ "-",
                               cod_subrenglon == "1132020330" ~ "-",
                               cod_subrenglon == "1132020210" ~ "-",
                               cod_subrenglon == "1142010100" ~ "-",
                               cod_subrenglon == "1142090100" ~ "-",
                               cod_subrenglon == "1142090200" ~ "-",
                               cod_subrenglon == "1141010100" ~ "-",
                               cod_subrenglon == "1141020100" ~ "-",
                               cod_subrenglon == "1141020200" ~ "-",
                               cod_subrenglon == "1143040100" ~ "-",
                               cod_subrenglon == "1143030100" ~ "-",
                               cod_subrenglon == "1143030200" ~ "-",
                               cod_subrenglon == "1143090200" ~ "-",
                               cod_subrenglon == "1143090120" ~ "-",
                               cod_subrenglon == "1143090110" ~ "-",
                               cod_subrenglon == "1143090130" ~ "-",
                               cod_subrenglon == "1121010000" ~ "-",
                               cod_subrenglon == "1122010000" ~ "-",
                               cod_subrenglon == "1122020000" ~ "-",
                               cod_subrenglon == "1123030000" ~ "-",
                               cod_subrenglon == "1123010000" ~ "-",
                               cod_subrenglon == "1124010000" ~ "-",
                               cod_subrenglon == "1125010000" ~ "-",
                               cod_subrenglon == "1191050000" ~ "-",
                               cod_subrenglon == "1191080000" ~ "-",
                               cod_subrenglon == "1191070000" ~ "-",
                               cod_subrenglon == "1191120000" ~ "-",
                               cod_subrenglon == "1191020000" ~ "-",
                               cod_subrenglon == "1191090000" ~ "-",
                               cod_subrenglon == "1191100000" ~ "-",
                               cod_subrenglon == "1191010000" ~ "-",
                               cod_subrenglon == "1191060000" ~ "-",
                               cod_subrenglon == "1191040000" ~ "-",
                               cod_subrenglon == "1191130000" ~ "-",
                               cod_subrenglon == "1191110000" ~ "-",
                               cod_subrenglon == "1191030000" ~ "-",
                               cod_subrenglon == "1199010000" ~ "-",
                               cod_subrenglon == "1431040000" ~ "-",
                               cod_subrenglon == "1425000000" ~ "-",
                               cod_subrenglon == "1426000000" ~ "-",
                               cod_subrenglon == "1415100000" ~ "-",
                               cod_subrenglon == "1415010100" ~ "-",
                               cod_subrenglon == "1415010500" ~ "-",
                               cod_subrenglon == "1415010200" ~ "-",
                               cod_subrenglon == "1415010600" ~ "-",
                               cod_subrenglon == "1415010300" ~ "-",
                               cod_subrenglon == "1415010400" ~ "-",
                               cod_subrenglon == "1415080000" ~ "-",
                               cod_subrenglon == "1415070000" ~ "-",
                               cod_subrenglon == "1415060000" ~ "-",
                               cod_subrenglon == "1415050000" ~ "-",
                               cod_subrenglon == "1414060000" ~ "-",
                               cod_subrenglon == "1414050000" ~ "-",
                               cod_subrenglon == "1414040000" ~ "-",
                               cod_subrenglon == "1414010000" ~ "-",
                               cod_subrenglon == "1413011000" ~ "-",
                               cod_subrenglon == "1413010100" ~ "-",
                               cod_subrenglon == "1413010300" ~ "-",
                               cod_subrenglon == "1413010700" ~ "-",
                               cod_subrenglon == "1413011300" ~ "-",
                               cod_subrenglon == "1413010500" ~ "-",
                               cod_subrenglon == "1413011100" ~ "-",
                               cod_subrenglon == "1413010400" ~ "-",
                               cod_subrenglon == "1413011200" ~ "-",
                               cod_subrenglon == "1413010200" ~ "-",
                               cod_subrenglon == "1413010600" ~ "-",
                               cod_subrenglon == "1413010900" ~ "-",
                               cod_subrenglon == "1413060000" ~ "-",
                               cod_subrenglon == "1413040000" ~ "-",
                               cod_subrenglon == "1413030000" ~ "-",
                               cod_subrenglon == "1413090000" ~ "-",
                               cod_subrenglon == "1413110000" ~ "-",
                               cod_subrenglon == "1413130000" ~ "-",
                               cod_subrenglon == "1413120000" ~ "-",
                               cod_subrenglon == "1413100000" ~ "-",
                               cod_subrenglon == "1413020000" ~ "-",
                               cod_subrenglon == "1413140000" ~ "-",
                               cod_subrenglon == "1416010100" ~ "-",
                               cod_subrenglon == "1416010300" ~ "-",
                               cod_subrenglon == "1416010600" ~ "-",
                               cod_subrenglon == "1416010400" ~ "-",
                               cod_subrenglon == "1416020200" ~ "-",
                               cod_subrenglon == "1416040000" ~ "-",
                               cod_subrenglon == "1412140000" ~ "-",
                               cod_subrenglon == "1412150000" ~ "-",
                               cod_subrenglon == "1411020000" ~ "-",
                               cod_subrenglon == "2413010000" ~ "-"
                               
                             ),
                             
                             cod_fuentefinanciacion = substr(Ingresos_m$`PosPre`, 2, 14),
                             
                             cod_fuentefinanciacion_3 = as.numeric(substr(Ingresos_m$`PosPre`, 12, 14)),
                             
                             fuentefinanciacion = case_when(
                               
                               cod_fuentefinanciacion == "1000000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1100000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1110000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111010101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111020101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030102001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1111030400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010102001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112010400001" ~ "Ingresos corrientes",
                        #       cod_fuentefinanciacion == "1112020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112020101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112020102001" ~ "Ingresos corrientes",
                      #         cod_fuentefinanciacion == "1112020200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112020300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1112020400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1113000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1113010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1113010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1113020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1113020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114010110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114010120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1114010130001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1115000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1115010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1115010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1120000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1121000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1121010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1122000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1122010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1122010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1122010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1122020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1123000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1123010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1123020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1123030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1124000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1124010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1125000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1125010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1130000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131010110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131010210001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131020110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131020200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1131020210001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010200000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010210001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010220001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010300000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010310000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010311001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010312001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010400000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010410000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010411001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010412001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010420000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010421001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010422001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010430000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010431001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010432001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010440000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010440001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010450000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010451001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132010452001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020200000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020300000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020310001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1132020320001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1140000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141010110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141010120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1141090000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1142000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1142010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1142090100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1142090200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143030000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143030100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143030200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143040000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143040100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090130001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1143090200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1190000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1191000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1191010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199020100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199020200000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1199020200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1200000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213030100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213030200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1213040100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1300000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1310000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1311000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1311090000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1311090100001" ~ "Ingresos corrientes",
                               #cod_fuentefinanciacion == "1311090200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030101001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030102001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030401001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312030402001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312040100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312040110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312040900001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090200000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090210001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090220001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090230001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090600000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090610001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090900000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090910001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090920001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090930001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1312090940001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010100000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010300000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010310001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010320001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010330001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313010340001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020130001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020140001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020300000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020310001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020320001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020340001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020350001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1313020910001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1320000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1321000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1321010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1321020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1321030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1322000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1322020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1322020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323010600000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323010600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323010610001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323020500000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323020510001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323020600000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323020610001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323030000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1323030100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1330000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331020000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331020100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331020200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331040000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331040100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331040200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090130001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090140001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090150001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090700001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1331090900001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1332000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1332010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1341000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1390000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1391000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1391010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1391020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1392000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1392000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399040000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399080000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399090000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399100000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399110000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1399120000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1400000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1410000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1411000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1411010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1411020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010700001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010800001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412010900001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412011500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412012000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412013000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412040000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412050000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412060000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412070000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412080000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412090000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412110000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412120000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412130000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1412140000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010700001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010800001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413010900001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413011500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413040000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413050000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413060000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413070000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413080000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413090000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1413100000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1414000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1414010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1414020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1414030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415010600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1415040000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010100001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010110001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010120001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010130001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010210001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010220001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010230001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010300001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010400001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010500001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416010600001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1416020200001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1420000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1421000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1421010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1423000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1424000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1425000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1426000000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1430000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1431000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1431010000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1431020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1431030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1432000000000" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1432020000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1432030000001" ~ "Ingresos corrientes",
                               cod_fuentefinanciacion == "1432040000001" ~ "Ingresos corrientes",
                               
                               cod_fuentefinanciacion == "2412010000060" ~ "Proyecto de Desarrollo Agrícola Península de Nicoya (PRODAPEN)",
                               cod_fuentefinanciacion == "2412020000060" ~ "Proyecto de Desarrollo Agrícola Península de Nicoya (PRODAPEN)",
                               cod_fuentefinanciacion == "2412030000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412050000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412060000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412070000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412080000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412090000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412100000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412110000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412120000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412130000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2412140000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2413010000060" ~ "Transferencia de capital OD",
                               cod_fuentefinanciacion == "2416020100060" ~ "Transferencia de capital OD",
                               
                               cod_fuentefinanciacion == "2431020000065" ~ "Donac. BIRF Fondo p/ Medio Ambiente Mund",
                               
                               cod_fuentefinanciacion == "2431020000065" ~ "Donac. BIRF Fondo p/ Medio Ambiente Mund",
                               cod_fuentefinanciacion == "2432010000066" ~ "Donaciones de Relaciones Exteriores Fi",
                               cod_fuentefinanciacion == "2432030000068" ~ "Cancelación operaciones cuasifiscales al BCCR. Acuerdo de Cooperación Energética de Caracas. Ley N° 8116",
                               cod_fuentefinanciacion == "2432040000070" ~ "Donación República de China",
                               cod_fuentefinanciacion == "2510000000121" ~ "Reintegro e intereses del crédito externo PL-480",
                               cod_fuentefinanciacion == "3131010000280" ~ "Títulos valores de deuda interna",
                               cod_fuentefinanciacion == "3131010000281" ~ "Títulos valores de deuda interna (Deuda Política)",
                               cod_fuentefinanciacion == "3131010000282" ~ "Títulos valores de deuda interna (Caja Única)",
                               cod_fuentefinanciacion == "3131010100280" ~ "Títulos valores de deuda interna",
                               
                               cod_fuentefinanciacion == "3131020000280" ~ "Títulos valores de deuda interna",
                               cod_fuentefinanciacion == "3131020000283" ~ "Títulos valores de deuda interna (Fideicomiso agropecuario)",
                               
                               cod_fuentefinanciacion == "3211010100450" ~ "Crédito BCIE-CR-26 FDS, Ley N° 7639 Programa de Infraestructura Universidad Nacional",
                               cod_fuentefinanciacion == "3211010200451" ~ "Crédito BCIE-CR-1129, Ley N° 7659. Proyecto de Desarrollo Agrícola Península de Nicoya",
                               cod_fuentefinanciacion == "3211010300452" ~ "Crédito BCIE-1605, Ley N° 8359 Programa para completar el complejo vial Costanera Sur",
                               cod_fuentefinanciacion == "3211010400453" ~ "Crédito BCIE 1709, Ley N° 8685 Programa de Gestión Integrada de Recursos Hídricos",
                               cod_fuentefinanciacion == "3211010500454" ~ "Crédito BCIE 2198 Programa de Alcantarillado y Con trol de Inundaciones para Limón, autorizado median te Ley No.9690 del 27 de junio del 2019",
                               cod_fuentefinanciacion == "3211010500513" ~ "Crédito BCIE-2157, Ley 9327 Proyecto de Mercado Regional Mayorista de la Región Chorotega",
                               
                               cod_fuentefinanciacion == "3211020900498" ~ "Crédito BID 1284/OC-CR, Ley N° 8154 Programa de Regularización del Catastro y Registro",
                               cod_fuentefinanciacion == "3211021000499" ~ "Crédito BID 1377/OC-CR, Ley N° 8273 Segunda Etapa del Programa de Modernización de la Administración de Justicia",
                               cod_fuentefinanciacion == "3211021100500" ~ "Crédito BID 1451/OC-CR, Ley N° 8403 Programa de Desarrollo del Sector Salud",
                               cod_fuentefinanciacion == "3211021100501" ~ "Crédito BID 667, Ley N° 7315 Proyecto Mejoramiento de la Calidad de la Educación General Básica",
                               cod_fuentefinanciacion == "3211021300502" ~ "Crédito BID 1436/OC-CR, Ley N° 8408 Programa de Fomento de la Producción Agropecuaria Sostenible",
                               cod_fuentefinanciacion == "3211021400503" ~ "Crédito BID 1556/OC-CR. Programa de Desarrollo Sostenible de la Cuenca del Río Sixaola",
                               cod_fuentefinanciacion == "3211021500504" ~ "Crédito BID 2007/-OC-CR, Ley N° 8845 Programa de Infraestructura Vial (PIVI)",
                               cod_fuentefinanciacion == "3211021600505" ~ "Crédito BID 2098/-OC-CR, Ley N° 8982 Programa Red Vial Cantonal (PIV1)",
                               cod_fuentefinanciacion == "3211021700506" ~ "Credito BID 1824/OC-CR, Ley N° 8967 Programa de turismo en Áreas Silvestres Protegidas y su Contrato Modificatorio",
                               cod_fuentefinanciacion == "3211021800507" ~ "Crédito BID 2526/OC-CR, Ley N° 9025 Programa para la Prevención de la Violencia y Promoción de la Inclusión Social",
                               cod_fuentefinanciacion == "3211021900508" ~ "Crédito BID Nº 2852/OC-CR-Programa de Innovación y Capital Humano para la Competitividad, Ley N° 9218",
                               cod_fuentefinanciacion == "3211022000509" ~ "Crédito BID N° 3071/OC-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                               cod_fuentefinanciacion == "3211022100510" ~ "Crédito BID N° 3072/CH-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                               cod_fuentefinanciacion == "3211022800521" ~ "Crédito BID N°4871/OC-CR Programa de Seguridad Ciu dadana y Prevención de Violencia Ley 9968",
                               
                               cod_fuentefinanciacion == "3211022200515" ~ "Crédito BID N°3488/OC-CR-Programa de Integración Fronteriza (PIF) Ley N°9451",
                               cod_fuentefinanciacion == "3211022300516" ~ "Crédito BID Nº4433/OC-CR Programa de Emergencias en Respuesta a la Tormenta Tropical Nate Ley N°9595",
                               cod_fuentefinanciacion == "3211022400517" ~ "Crédito BID N° 4507/OC-CR Programa Red Vial Cantonal II (PIV2) Ley N° 8982",
                               cod_fuentefinanciacion == "3211022500518" ~ "Crédito BID N° 4819/OC-CR Programa de Apoyo a la Sostenibilidad Fiscal Ley N° 9754",
                               cod_fuentefinanciacion == "3211022600519" ~ "Crédito BID N.°4988/OC-CR Programa hacia una Economía Verde: Apoyo al Plan de Descarbonización de Costa Rica. Ley Nº 9846",
                               cod_fuentefinanciacion == "3211030000000" ~ "Banco Mundial",
                               cod_fuentefinanciacion == "3211030200514" ~ "Crédito BIRF 8593-CR, Ley No 9396, Programa por Resultados para el Fortalecimiento del Seguro Universal de Salud en Costa Rica",
                               cod_fuentefinanciacion == "3211030200531" ~ "Crédito BIRF 4557-CR, Ley N° 8058 Programa de Pagos de Servicios Ambientales.",
                               cod_fuentefinanciacion == "3211030300532" ~ "Crédito BIRF 70-68-CR, Ley N° 8269 Proyecto de Fortalecimiento y Modernización del Sector Salud",
                               cod_fuentefinanciacion == "3211030400533" ~ "Crédito BIRF 7284-CR Proyecto Equidad y Eficiencia de la Educación",
                               cod_fuentefinanciacion == "3211030500534" ~ "Crédito BIRF 7388-CR Proyecto Introducción Instrumentos Financieros para Gestión Ambiental",
                               cod_fuentefinanciacion == "3211030500535" ~ "Crédito BIRF 7594-CR Opción de Desembolsos Diferido Ante el Riesgo de Catástrofes (CAT-DDO)",
                               cod_fuentefinanciacion == "3211030500536" ~ "Crédito BIRF 7498-CR, Ley N° 8725 Proyecto de Limón Cuidad-Puerto",
                               cod_fuentefinanciacion == "3211030600537" ~ "Crédito BIRF 7686-CR, Ley N° 8843 Apoyo de Políticas de Desarrollo de las Finanzas Públicas y la Competitividad",
                               cod_fuentefinanciacion == "3211030700538" ~ "Crédifo BIRF N° 8194-CR para el Proyecto de Mejoramiento de la Educación Superior",
                               cod_fuentefinanciacion == "3211050100610" ~ "Crédito FIDA 371-CR, Ley N° 7659 Proyecto de Desarrollo Agrícola Península de Nicoya",
                               cod_fuentefinanciacion == "3211090100650" ~ "Crédito Banco Japonés de Cooperación Internacional CR-P4, Ley N° 8559 Proyecto de Mejoramiento del Medio Ambiente del Area Metropolitana de San José",
                               cod_fuentefinanciacion == "3211090200660" ~ "Crédito Corporación Andina De Fomento, Ley N° 8844 Proyecto Bajos de Chilamate - Vuelta Kooper",
                               cod_fuentefinanciacion == "3211090200661" ~ "Contrato Préstamo para financiar el Programa de Apoyo para el Fortalecimiento de las Finanzas Públicas",
                               cod_fuentefinanciacion == "3211090200662" ~ "AFD Contrato de Préstamo N.° CCR 1011 01F Ley Nº 9846. Programa de Apoyo Presupuestario para el Fortalecimiento de las Políticas",
                               cod_fuentefinanciacion == "3211090200663" ~ "FMI Crédito Instrumento de Financiamiento Rápido ( IFR) para apoyo presupuestario en la atención de l a emergencia COVID-19",
                               cod_fuentefinanciacion == "3211090200664" ~ "FMI Crédito Servicio Ampliado del Fondo (SAF)",
                               cod_fuentefinanciacion == "3211010500456" ~ "Crédito BCIE-2270 Ley 10056 Proyecto adqyuisición y aplicación de vacunas COVID-19",
                               cod_fuentefinanciacion == "3211030900540" ~ "CREDITO BIRF No. 9146-CR Primer préstamo de polít ica de desarrollo fiscal y de descarbonización Le y 9988",
                      
                               
                               cod_fuentefinanciacion == "3212001000511" ~ "Cred EXIMBANK Ley 9293Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32, Tramo: Ruta N° 4 - Limón",
                               cod_fuentefinanciacion == "3212002000512" ~ "Cred EXIMBANK Ley 9293Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32, Tramo: Ruta N° 4 - Limón",
                               cod_fuentefinanciacion == "3212010000690" ~ "Crédito del Gobierno de los EEUU, Convenio PL-480 Leyes N° 6945, 6978, 7019, 7059, 7098, 7203 y 7307",
                               cod_fuentefinanciacion == "3212030000692" ~ "Crédito KFW 2002-65-066, Leyes N° 6979, 7132 y 7109 Programa de Rehabilitación y Mantenimiento de la Red Vial Cantonal",
                               cod_fuentefinanciacion == "3212030000693" ~ "Créditos KFW, Ley N° 7132 Programa de Agua Potable y Saneamiento Básico Rural II",
                               cod_fuentefinanciacion == "3212030000694" ~ "Crédito KFW N° 28568 Programa de Saneamiento en Zo nas Prioritarias, Ley N° 9723",
                               
                               cod_fuentefinanciacion == "3213010000730" ~ "Crédito Export - Import Bank de la República de China, Ley N° 7624 Proyecto Construcción Carretera Florencia - Naranjo.",
                               
                               cod_fuentefinanciacion == "3232010000890" ~ "Colocación de títulos valores de deuda externa",
                     
                               cod_fuentefinanciacion == "3311010000900" ~ "Superávit Libre Museo Histórico Cultural Juan Santamaria",
                               cod_fuentefinanciacion == "3311010000901" ~ "Superávit Libre Instituto sobre Alcoholismo y Farm acodependencia (IAFA)",
                               cod_fuentefinanciacion == "3311010000902" ~ "Superávit Libre Instituto Nacional de Innovación T ecnológica Agropecuaria (INTA)",
                               cod_fuentefinanciacion == "3311010000906" ~ "Superávit Libre Consejo Nacional de la Persona Adu lta Mayor (CONAPAM)",
                               cod_fuentefinanciacion == "3311010000908" ~ "Superávit Libre Instituto Costarricense de Investi gación y Enseñanza en Nutrición y Salud (INCIENSA)",
                               cod_fuentefinanciacion == "3311010000910" ~ "Superávit Libre Consejo Nacional de Concesiones (C NC)",
                               cod_fuentefinanciacion == "3311010000911" ~ "Superávit Libre Teatro Nacional (TN)",
                               cod_fuentefinanciacion == "3311010000912" ~ "Superávit Libre Consejo Nacional de Investigación en Salud (CONIS)",
                               cod_fuentefinanciacion == "3311010000913" ~ "Superávit Libre Junta Administrativa del Registro Nacional (JARN)",
                               cod_fuentefinanciacion == "3311010000914" ~ "Superávit Libre Dirección Nacional de Notariado (D NN)",
                               cod_fuentefinanciacion == "3311010000915" ~ "Superávit Libre Tribunal Registral Administrativo (TRA)",
                               cod_fuentefinanciacion == "3311010000916" ~ "Superávit Libre Museo Nacional (MN)",
                               cod_fuentefinanciacion == "3311010000917" ~ "Superávit Libre Casa de la Cultura de Puntarenas ( CCP)",
                               cod_fuentefinanciacion == "3311010000918" ~ "Superávit Libre Teatro Melico Salazar (TPMS)",
                               cod_fuentefinanciacion == "3311010000919" ~ "Superávit Libre Archivo Nacional (JAAN)",
                               cod_fuentefinanciacion == "3311010000920" ~ "Superávit Libre Consejo de la Persona Joven (CPJ)",
                      
                               cod_fuentefinanciacion == "3311030000980" ~ "Superávit Defensoría",
                               cod_fuentefinanciacion == "3311040000980" ~ "Superávit Poder Judicial",
                               cod_fuentefinanciacion == "3311050000980" ~ "Supervávit Asamblea Legislativa (2009)",
                               cod_fuentefinanciacion == "3311060000905" ~ "Superávit Contraloría",
                               cod_fuentefinanciacion == "3311060000906" ~ "Superávit Asamblea Legislativa",
                               cod_fuentefinanciacion == "3321010000922" ~ "Superávit Específico Colocación Títulos en el Exterior",
                               cod_fuentefinanciacion == "3321010000923" ~ "Superávit Específico Donación UE",
                               cod_fuentefinanciacion == "3321010000924" ~ "Superávit Específico de la Donación República de orea Iniciativa de Cooperación entre Corea y Améri ca Latina para la Alimentación y la Agricultura (K",
                               cod_fuentefinanciacion == "3321010000925" ~ "Superávit Específico préstamo CAF Bajos de Chilama te - Vuelta Kooper",
                               cod_fuentefinanciacion == "3321020000923" ~ "Superávit Específico de la donación de la Unión Europea",
                               cod_fuentefinanciacion == "3211022700520" ~ "Crédito BID N.°4864/OC-CR Programa de Infraestruct ura Vial y Promoción de Asociaciones Público Privadas. Ley Nº 9899",
                               cod_fuentefinanciacion == "3211030800539" ~ "Crédito BIRF N° 9075-CR Modernizar y Digitalizar los Sistemas Tecnológicos del Ministerio de Hacien da Ley N° 9922",
                      
                               cod_fuentefinanciacion == "3321010000943" ~ "Superávit Específico Consejo Nacional de Concesion es (CNC)",
                               cod_fuentefinanciacion == "3321010000947" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 7972",
                               cod_fuentefinanciacion == "3321010000948" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 9028",
                               cod_fuentefinanciacion == "3321010000949" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 8204",
                               cod_fuentefinanciacion == "3321010000950" ~ "Superávit Especifico IAFA Ley 9036",
                               cod_fuentefinanciacion == "3321010000952" ~ "Superávit Específico Sistema Nacional de Áreas de Conservación (SINAC)",
                               cod_fuentefinanciacion == "3321010000957" ~ "Superávit Específico Consejo Nacional de Viabilida d (CONAVI)",
                               cod_fuentefinanciacion == "3321010000958" ~ "Superávit Específico Comisión Nacional de Vacunaci ón y Epidemiología (CNVE)",
                               cod_fuentefinanciacion == "3321010000959" ~ "Superávit Específico Fondo de Desarrollo Social y Asignaciones Familiares (FODESAF)",
                               cod_fuentefinanciacion == "3321010000960" ~ "Superávit Específico Instituto Costarricense de In vestigación y Enseñanza en Nutrición y Salud (INCI ENSA)",
                               cod_fuentefinanciacion == "3321010000961" ~ "Superávit Específico Consejo de Seguridad Vial (CO SEVI)",
                               cod_fuentefinanciacion == "3321010000962" ~ "Superávit Específico Dirección General de Migración y Extranjería (DGME)",
                      
                                                     
                               # Desentralizadas 
                               
                               cod_fuentefinanciacion == "1112010500001" ~ "Impuesto a los ingresos y utilidades artículo 46 Ley 8488 CNE",
                               cod_fuentefinanciacion == "1132010910001" ~ "Impuestos Ley No.6883 y Ley No. 8495 SENASA",
                               cod_fuentefinanciacion == "1132020210001" ~ "Permiso de Transporte carga liviana COSEVI",
                               cod_fuentefinanciacion == "1132020330001" ~ "Impuestos espectáculos públicos leyes 3632 y 5780 Teatro Nacional",
                               cod_fuentefinanciacion == "1141020200001" ~ "1,5% valor CIF de productos agroquímicos SFE",
                               cod_fuentefinanciacion == "1191020000001" ~ "Timbre del Registro Nacional",
                               cod_fuentefinanciacion == "1191030000001" ~ "Timbre Propiedad Intelectual - Registro Nacional",
                               cod_fuentefinanciacion == "1191040000001" ~ "Timbre Portal Web - Registro Nacional",
                               cod_fuentefinanciacion == "1191050000001" ~ "Placas - Registro Nacional",
                               cod_fuentefinanciacion == "1191060000001" ~ "Timbre Gobierno Digital - Registro Nacional",
                               
                               cod_fuentefinanciacion == "1191070000001" ~ "Timbre Colegio Abogados - Direccion Nacional de Notariado",
                               cod_fuentefinanciacion == "1191080000001" ~ "Timbre Archivo Nacional",
                               cod_fuentefinanciacion == "1191090000001" ~ "Timbre Educacion y Cultura inciso c) artículo 10 Ley 5923 y sus reformas Museo Nacional",
                               cod_fuentefinanciacion == "1191100000001" ~ "Timbre Educacion y Cultura inciso d) artículo 10 Ley 5923 y sus reformas SINEM",
                               cod_fuentefinanciacion == "1191110000001" ~ "Timbre Pro-Parques Nacionales SINAC",
                               cod_fuentefinanciacion == "1191120000001" ~ "Timbre de Vida Silvestre SINAC",
                               cod_fuentefinanciacion == "1191130000001" ~ "Timbre Pro-Parques Nacionales CONAGEBIO",
                               cod_fuentefinanciacion == "1219020000001" ~ "Contrib. Patronal sobre la Nómina de Organos Desconcentrados",
                               cod_fuentefinanciacion == "1219030000001" ~ "Contrib. Patronal sobre la Nómina de Instittuciones Descentralizadas no Empresariales",
                               cod_fuentefinanciacion == "1219040000001" ~ "Contrib. Patronal sobre la Nómina de Gobiernos Locales",
                               
                               cod_fuentefinanciacion == "1219050000001" ~ "Contrib. Patronal sobre la Nómina de Empresas Públicas no Financieras",
                               cod_fuentefinanciacion == "1219060000001" ~ "Contrib. Patronal sobre la Nómina de Instituciones Públicas Financieras",
                               cod_fuentefinanciacion == "1219070000001" ~ "Contribución Patronal sobre la Nómina de Empresas del Sector Privado",
                               cod_fuentefinanciacion == "1311010100001" ~ "Venta de Bienes Estaciones Experimentales INTA",
                               cod_fuentefinanciacion == "1311090200001" & Año < 2021 ~ "Venta de Bienes -FONAM- Ley 7554",
                               cod_fuentefinanciacion == "1311090200001" & Año >= 2021 ~ "Venta de Bienes SENASA",
                               cod_fuentefinanciacion == "1311090300001" ~ "Venta Libros, revistas y folletos Museo Nacional",
                               cod_fuentefinanciacion == "1311090430001" ~ "Venta Libros, revistas y folletos Museo de Arte Costarricense",
                               cod_fuentefinanciacion == "1311090500001" ~ "Venta de producción planteles y otros bienes Imprenta Nacional",
                               cod_fuentefinanciacion == "1312010110001" ~ "Acarreo de Vehículos detenidos COSEVI",
                               cod_fuentefinanciacion == "1312010410001" ~ "Servicio de transporte aeroportuario CTAC",
                               
                               cod_fuentefinanciacion == "1312030403001" ~ "Servicios de recaudación Teatro Nacional",
                               cod_fuentefinanciacion == "1312030910001" ~ "Otros servicios financieros y de seguros COSEVI",
                               cod_fuentefinanciacion == "1312040120001" ~ "Alquiler de edificios CTAC ",
                               cod_fuentefinanciacion == "1312040130001" ~ "Alquiler de edificios Registro Nacional",
                               cod_fuentefinanciacion == "1312040140001" ~ "Alquiler de edificios Casa Cultura Puntarenas",
                               cod_fuentefinanciacion == "1312040150001" ~ "Alquiler de edificios Centro Nacional de la Música",
                               cod_fuentefinanciacion == "1312040160001" ~ "Alquiler de edificios Teatro Nacional",
                               cod_fuentefinanciacion == "1312040170001" ~ "Alquiler de edificios Teatro Melico Salazar",
                               cod_fuentefinanciacion == "1312040910001" ~ "Custodia de Vehículos COSEVI",
                               cod_fuentefinanciacion == "1312040920001" ~ "Apartados Registro Nacional",
                               
                               cod_fuentefinanciacion == "1312040930001" ~ "Otros Alquileres Teatro Melico Salazar",
                               cod_fuentefinanciacion == "1312090120001" ~ "Venta Servicios Capacitación Archivo Nacional",
                               cod_fuentefinanciacion == "1312090240001" ~ "Servicios de Análisis Laboratorio INTA",
                               cod_fuentefinanciacion == "1312090250001" ~ "Venta de servicios ambientales FONAFIFO",
                               cod_fuentefinanciacion == "1312090410001" ~ "Servicios culturales y recreativos Centro Costarricense de Producción Cinematográfica",
                               cod_fuentefinanciacion == "1312090420001" ~ "Servicios culturales y recreativos Centro Nacional de la Música",
                               cod_fuentefinanciacion == "1312090430001" ~ "Servicios culturales y recreativos Museo Nacional",
                               cod_fuentefinanciacion == "1312090440001" ~ "Venta de entradas y otros Teatro Nacional",
                               cod_fuentefinanciacion == "1312090450001" ~ "Venta de entradas y otros Teatro Melico Salazar",
                               cod_fuentefinanciacion == "1312090460001" ~ "Entradas a las Áreas Silvestres Protegidas SINAC",
                               
                               cod_fuentefinanciacion == "1312090620001" ~ "Servicios de publicidad e impresión de Imprenta Nacional",
                               cod_fuentefinanciacion == "1312090980001" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)",
                               cod_fuentefinanciacion == "1312090981001" ~ "Venta de Servicios Registrales  Registro Nacional",
                               cod_fuentefinanciacion == "1312090982001" ~ "Venta de Servicios Direccion Nacional Notariado",
                               cod_fuentefinanciacion == "1312090983001" ~ "Venta de Servicios SENASA",
                               cod_fuentefinanciacion == "1312090984001" ~ "Venta de servicios varios Casa de Cultura Puntarenas",
                               cod_fuentefinanciacion == "1312090985001" ~ "Venta de servicios varios Archivo Nacional",
                               cod_fuentefinanciacion == "1312090986001" ~ "Venta de servicios Museo Nacional",
                               cod_fuentefinanciacion == "1312090987001" ~ "Cursos libres Teatro Melico Salazar",
                               cod_fuentefinanciacion == "1312090988001" ~ "Uso de instal. en las Áreas Silvestres Protegidas SINAC",
                               
                               cod_fuentefinanciacion == "1312090990001" ~ "Venta de otros servicios Servicio Fitosanitario del Estado (SFE)",
                               cod_fuentefinanciacion == "1313010130001" ~ "Derechos administrativos a los servicios de transporte en carretera CNC- Concesión carretera San José-Caldera",
                               cod_fuentefinanciacion == "1313010140001" ~ "Derechos administrativos a los servicios de transporte en carretera CONAVI- Peajes carretera Florencio Castillo",
                               cod_fuentefinanciacion == "1313010150001" ~ "Derechos administrativos a los servicios de transporte en carretera CONAVI- Peajes carretera Braulio Carrillo",
                               cod_fuentefinanciacion == "1313010350001" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                               cod_fuentefinanciacion == "1313010410001" ~ "Derechos administrativos a los servicios de transporte aeroportuario CTAC",
                               cod_fuentefinanciacion == "1313020210001" ~ "Matrícula Exámenes Teóricos y Pruebas Prácticas COSEVI",
                               cod_fuentefinanciacion == "1313020220001" ~ "Matrícula estudiantes Centro Nacional de la Música",
                               cod_fuentefinanciacion == "1313020360001" ~ "Cánones y Multas y Sanciones Administrativas PROHAB",
                               cod_fuentefinanciacion == "1313020370001" ~ "Derechos Administrativos a actividades comerciales SINAC",
                               
                               cod_fuentefinanciacion == "1313020920001" ~ "Emisión de Licencias de Conducir COSEVI",
                               cod_fuentefinanciacion == "1313020930001" ~ "Registro e inscripción CANNON-CONIS",
                               cod_fuentefinanciacion == "1313020940001" ~ "Derecho de Filmación SINAC",
                               cod_fuentefinanciacion == "1322010100001" ~ "Concesión de obra pública CTAC",
                               cod_fuentefinanciacion == "1322020100001" ~ "Alquiler de terrenos CTAC",
                               cod_fuentefinanciacion == "1322020200001" ~ "Alquiler de Terrenos SINAC",
                               cod_fuentefinanciacion == "1322090100001" ~ "Concesión Soda Comedor Registro Nacional",
                               cod_fuentefinanciacion == "1322090200001" ~ "Otros Ingresos de la Renta de la Propiedad SINAC",
                               cod_fuentefinanciacion == "1323010600001" ~ "Intereses sobre títulos valores de Instituciones Públicas Financieras",
                               cod_fuentefinanciacion == "1323030102001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales COSEVI",
                               
                               cod_fuentefinanciacion == "1323030103001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales FODESAF",
                               cod_fuentefinanciacion == "1323030104001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Patronato Construcciones",
                               cod_fuentefinanciacion == "1323030106001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Teatro Nacional",
                               cod_fuentefinanciacion == "1323030401001" ~ "Diferencias por tipo de cambio SINAC",
                               cod_fuentefinanciacion == "1331020000001" ~ "Multas de tránsito. Ley No. 9078 COSEVI",
                               cod_fuentefinanciacion == "1331090800001" ~ "Multas Código Penal Ley 5386  Patronato de Construcciones",
                               cod_fuentefinanciacion == "1331091000001" ~ "Multas Sanciones Remanentes Confiscaciones Teatro Nacional",
                               cod_fuentefinanciacion == "1349010000001" ~ "Sobre multas de tránsito COSEVI",
                               cod_fuentefinanciacion == "1399020000001" ~ "Ingresos varios no especificados COSEVI",
                               cod_fuentefinanciacion == "1399030000001" ~ "Ingresos varios no especificados Instituto Costarricense sobre Drogas",
                               
                               cod_fuentefinanciacion == "1399040000001" ~ "Otros ingresos varios no especificados Artículo 44 Ley 8488 CNE",
                               cod_fuentefinanciacion == "1413090000001" ~ "Servicio Nacional de Aguas Subterráneas Riego y Avenamiento artículo 3 inciso h) Ley 8149 - INTA",
                               cod_fuentefinanciacion == "1413110000001" ~ "Instituto Costarricense de Pesca y Acuicultura artículo 3 inciso h) Ley 8149 - INTA",
                               cod_fuentefinanciacion == "1413120000001" ~ "Transf. corrientes Instit. Desce. No Empresariales CTAC",
                               cod_fuentefinanciacion == "1413130000001" ~ "Instituto de Desarrollo Rural Ley N° 9036 a IAFA",
                               cod_fuentefinanciacion == "1414040000001" ~ "De Gobierno Locales Ley 9303 CONAPDIS",
                               cod_fuentefinanciacion == "1414050000001" ~ "De Gobierno Locales Ley 7509 Registro Nacional",
                               cod_fuentefinanciacion == "1414060000001" ~ "De Gobierno Locales Casa Cultura Puntarenas",
                               cod_fuentefinanciacion == "1415050000001" ~ "Junta de  Protección Social Ley 9379 CONAPDIS",
                               cod_fuentefinanciacion == "1415060000001" ~ "Junta de Protección Social Ley 8718  a CTAMS",
                               
                               cod_fuentefinanciacion == "1415070000001" ~ "Junta de Protección Social Ley 8111 a CNVE",
                               cod_fuentefinanciacion == "1415080000001" ~ "Fábrica Nacional de Licores (Ley N° 8289) a IAFA",
                               cod_fuentefinanciacion == "1416040000001" ~ "Transferencia INS - Consejo Salud Ocupacional",
                               cod_fuentefinanciacion == "1431040000001" ~ "Transferencias de organismos internacionales Archivo Nacional",
                               cod_fuentefinanciacion == "3211010500455" ~ "Adenda al crédito público N.° 2080 BCIE/CONAVI",
                               cod_fuentefinanciacion == "3311010000901" ~ "Superávit Libre Instituto sobre Alcoholismo y Farm acodependencia (IAFA)",
                               cod_fuentefinanciacion == "3311010000911" ~ "Superávit Libre Teatro Nacional (TN)",
                               cod_fuentefinanciacion == "3321010000940" ~ "Superávit Específico Comisión Nacional para la Ges tión de la Biodiversidad (CONAGEBIO)",
                               cod_fuentefinanciacion == "3321010000942" ~ "Superávit Específico Instituto Costarricense sobre Drogas (ICD)",
                               cod_fuentefinanciacion == "3321010000945" ~ "Superávit Específico Consejo de Transporte Público (CTP)",
                               cod_fuentefinanciacion == "3321010000951" ~ "Superávit Específico Teatro Nacional (TN)",
                               cod_fuentefinanciacion == "3321010000953" ~ "Superávit Específico Patronato de Construcciones, Instalaciones y Adquisición de Bienes (PCIAB)"
                             
                             ), 
                             
)

names(Ingresos_m)

Ingresos_m <- dplyr::mutate(Ingresos_m,
                            
                            cod_sector = case_when(
                              
                              # Gobierno Central 
                              
                              cod_fuentefinanciacion == "1000000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1100000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1110000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111010101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111020101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010400001" ~ 'Gobierno Central',
                        #      cod_fuentefinanciacion == "1112020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020102001" ~ 'Gobierno Central',
                        #      cod_fuentefinanciacion == "1112020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1120000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1121000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1121010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1124000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1124010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1125000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1125010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1130000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010310000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010311001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010312001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010400000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010410000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010411001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010412001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010420000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010421001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010422001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010430000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010431001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010432001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010440000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010440001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010450000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010451001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010452001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1140000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143040000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1190000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1191000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1191010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1200000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1300000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1310000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090200001" & Año < 2021 ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090200001" & Año >= 2021 ~ 'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030401001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030402001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090230001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090600000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090900000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090910001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090920001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090930001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090940001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010330001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010340001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020140001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020340001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020350001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020910001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1320000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010600001" & Año < 2021 ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010600001" & Año >= 2021 ~ 'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323010610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020500000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020510001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020600000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1330000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090140001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090150001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1341000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1390000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1392000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1392000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399110000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399120000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1400000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1410000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010800001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412012000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412013000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412050000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412060000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412070000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412110000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412120000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412130000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412140000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010800001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413050000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413060000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413070000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010230001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1420000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1421000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1421010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1423000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1424000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1425000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1426000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1430000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412010000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412020000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412030000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412050000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412060000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412070000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412080000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412090000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412100000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412110000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412120000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412130000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412140000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2413010000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2416020100060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2431020000065" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2431020000065" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432010000066" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432030000068" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432040000070" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2510000000121" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000281" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000282" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010100280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131020000280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131020000283" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010100450" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010200451" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010300452" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010400453" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500454" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500513" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211020900498" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021000499" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021100500" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021100501" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021300502" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021400503" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021500504" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021600505" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021700506" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021800507" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021900508" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022000509" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022100510" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022200515" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022300516" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022400517" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022500518" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022600519" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030200514" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030200531" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030300532" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030400533" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500534" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500535" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500536" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030600537" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030700538" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211050100610" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090100650" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200660" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200661" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200662" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200663" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212001000511" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212002000512" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212010000690" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000692" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000693" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000694" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3213010000730" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3232010000890" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311030000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311040000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311050000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311060000905" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311060000906" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000922" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000923" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000924" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000925" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321020000923" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1342000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412150000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413140000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030800539" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022700520" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022800521" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030900540" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200664" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500456" ~ 'Gobierno Central',
                        
                        
                                                     
                              # Desentralizadas 
                              
                              cod_fuentefinanciacion == "1112010500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1112010500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132010910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132020210001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132020330001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1141020200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191080000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191090000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191100000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191120000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191130000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311010100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090200001" & Año == 2007 ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090200001" & Año >= 2021 ~ 'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090300001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090430001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312010110001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312010410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312030403001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312030910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040120001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040130001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040140001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040150001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040160001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040170001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040920001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040930001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090120001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090240001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090250001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090420001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090430001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090440001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090450001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090460001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090620001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090980001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090981001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090982001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090983001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090984001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090985001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090986001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090987001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090988001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090990001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010130001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010140001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010150001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010350001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020210001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020220001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020360001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020370001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020920001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020930001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020940001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322010100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322020100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322020200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322090100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322090200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323010600001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030102001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030103001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030104001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030106001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030401001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331090800001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331091000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1349010000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413090000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413120000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413130000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415080000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1416040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1431040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090991001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3211010500455" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000901" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000911" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000940" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000942" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000945" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000951" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000953" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311010200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132010230001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331040300001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1427000000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000900" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000902" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000906" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000908" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000910" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000912" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000913" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000914" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000915" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000916" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000917" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000918" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000919" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000920" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000943" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000947" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000948" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000949" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000950" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000952" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000957" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000958" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000959" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000960" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000961" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000962" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090600001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090700001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040180001" ~'Órganos desconcentrados'
                            
                        
                            )
                            
)


Ingresos_m <-  dplyr::mutate(Ingresos_m,
                             
                             Descripción = if_else (
                               
                               Ingresos_m$PosPre == "I1413020000001",  "Instituto Nacional de Aprendizaje. Ley Nº7372 / 22 -11-93", Ingresos_m$Descripción
                             )
)


Ingresos_m <- dplyr::mutate(Ingresos_m, 
                            
                            'Impuesto a los Ingresos y Utilidades-ISR' = case_when(
                                      Ingresos_m$cod_grupo == 111 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto al Valor Agregado-IVA' = case_when(
                              Ingresos_m$cod_partida == 113101 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto sobre la propiedad de bienes inmuebles' = case_when(
                              Ingresos_m$cod_subgrupo == 1121  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto sobre la propiedad de vehiculos, aeronaves y embarcaciones' = case_when(
                              Ingresos_m$cod_partida == 112201 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Timbre Fauna Silvestre' = case_when(
                              Ingresos_m$cod_partida == 112202 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Incremento timbre de educacion y cultura' = case_when(
                              Ingresos_m$cod_partida == 112301 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto a las Personas Juridicas' = case_when(
                              Ingresos_m$cod_partida == 112303  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto sobre el traspaso de bienes inmuebles' = case_when(
                              Ingresos_m$cod_subgrupo == 1124 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto traspaso de vehiculos usados' = case_when(
                              Ingresos_m$cod_partida == 112501 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto sobre las ventas de bienes y servicios internos' = case_when(
                              Ingresos_m$cod_subpartida == 11310101 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto sobre las ventas de bienes y servicios importados' = case_when(
                              Ingresos_m$cod_subpartida == 11310102 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto Selectivo de Consumo' = case_when(
                              Ingresos_m$cod_partida == 113102 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto selectivo de consumo de bienes internos' = case_when(
                              Ingresos_m$cod_subpartida == 11310201  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto selectivo de consumo de bienes importados' = case_when(
                              Ingresos_m$cod_subpartida ==  11310202 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuestos especificos sobre los combustibles y energeticos' = case_when(
                              Ingresos_m$cod_subpartida == 11320103 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                              'Impuesto unico a los combustibles. Interno' = case_when(
                                Ingresos_m$cod_subrenglon == 1132010311  ~   Ingresos_m$'Ingresos'
                                
                              ),
                              
                              'Impuesto unico a los combustibles. Importado' = case_when(
                                Ingresos_m$cod_subrenglon == 1132010312 ~   Ingresos_m$'Ingresos'
                                
                              ),
                              
                              'Impuestos especificos sobre bebidas alcoholicas.' = case_when(
                                Ingresos_m$cod_renglon == 113201041  ~   Ingresos_m$'Ingresos'
                                
                              ),
                              
                              'Impuestos especificos sobre bebidas envasadas sin contenido alcoholico' = case_when(
                                Ingresos_m$cod_renglon == 113201042 ~   Ingresos_m$'Ingresos'
                                
                              ),
                            
                            'Impuestos especificos sobre los jabones de tocador' = case_when(
                              Ingresos_m$cod_renglon == 113201043 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto al Cemento' = case_when(
                              Ingresos_m$cod_renglon == 113201044  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuestos a los Productos del Tabaco' = case_when(
                              Ingresos_m$cod_renglon == 113201045 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            'Impuesto a casinos' = case_when(
                              Ingresos_m$cod_renglon == 113202031 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto a empresas de enlace de apuestas electronicas.' = case_when(
                              Ingresos_m$cod_renglon == 113202032  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Arancel de Aduanas' = case_when(
                              Ingresos_m$cod_subpartida == 11410101  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            '1% sobre el valor aduanero de las mercancias' = case_when(
                              Ingresos_m$cod_subpartida == 11410201 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Derechos sobre exportacion de banano' = case_when(
                              Ingresos_m$cod_subpartida == 11420101 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Otros impuestos a las exportaciones' = case_when(
                              Ingresos_m$cod_partida == 114209  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            '¢1.5 Por caja de banano exportado.' = case_when(
                              Ingresos_m$cod_subpartida == 11420901  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuestos a las exportaciones por via terrestre' = case_when(
                              Ingresos_m$cod_subpartida == 11420902 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Derechos de salida del territorio nacional por via aerea' = case_when(
                              Ingresos_m$cod_subpartida == 11430301 ~   Ingresos_m$'Ingresos'
                              
                            ),
                                                        

                            
                            'Derechos de salida del Territorio Nacional por via terrestre' = case_when(
                              Ingresos_m$cod_subpartida == 11430302  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Derechos consulares' = case_when(
                              Ingresos_m$cod_subpartida == 11430401  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Fondo Social Migratorio' = case_when(
                              Ingresos_m$cod_renglon  == 114309011  ~   Ingresos_m$'Ingresos'
                              

                            
                            ),
                            
                            'Fondo Especial de Migracion' = case_when(
                              Ingresos_m$cod_renglon == 114309012  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Otros Impuestos Migratorios ' = case_when(
                              Ingresos_m$cod_renglon == 114309013 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Impuesto General Forestal' = case_when(
                              Ingresos_m$cod_subpartida == 11430902 ~   Ingresos_m$'Ingresos'
                              

                            ),
                            
                            'Timbre Fiscal' = case_when(
                              Ingresos_m$cod_partida == 119101  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Papel Sellado' = case_when(
                              Ingresos_m$cod_partida == 119901 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Contribuciones Sociales' = case_when(
                              Ingresos_m$cod_subclase ==  12~   Ingresos_m$'Ingresos'
                              
                            ),
                            

                            
                            'Ingresos No Tributarios' = case_when(
                              Ingresos_m$cod_subclase ==  13 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Derechos Administrativos' = case_when(
                              Ingresos_m$cod_subgrupo == 1313 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            '25% utilidades del INS' = case_when(
                              Ingresos_m$cod_partida == 132103 ~   Ingresos_m$'Ingresos'
                              

                            ),
                            
                            'Multas, sanciones, remates y comisos' = case_when(
                              Ingresos_m$cod_grupo == 133 ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Transferencias Corrientes' = case_when(
                              Ingresos_m$cod_subclase == 14  ~   Ingresos_m$'Ingresos'
                              
                            ),
                            
                            'Ingresos de Capital' = case_when(
                              Ingresos_m$cod_clase == 2  ~   Ingresos_m$'Ingresos'
                              
                            )
                       
)


                              
names(Ingresos_m)                            


Impuestos <- Ingresos_m %>% 
                         dplyr::select(
                            Año, mes.cod,mes, cod_fuentefinanciacion_3,
                           `Impuesto a los Ingresos y Utilidades-ISR`,
                           `Impuesto al Valor Agregado-IVA`,
                           `Impuesto sobre la propiedad de bienes inmuebles`,
                           `Impuesto sobre la propiedad de vehiculos, aeronaves y embarcaciones`,
                           `Timbre Fauna Silvestre`,
                           `Incremento timbre de educacion y cultura`,
                           `Impuesto a las Personas Juridicas`,
                           `Impuesto sobre el traspaso de bienes inmuebles`,
                           `Impuesto traspaso de vehiculos usados`,
                           `Impuesto sobre las ventas de bienes y servicios internos`,
                           `Impuesto sobre las ventas de bienes y servicios importados`,
                           `Impuesto Selectivo de Consumo`,
                           `Impuesto selectivo de consumo de bienes internos`,
                           `Impuesto selectivo de consumo de bienes importados`,
                           `Impuestos especificos sobre los combustibles y energeticos`,
                           `Impuesto unico a los combustibles. Interno`,
                           `Impuesto unico a los combustibles. Importado`,
                           `Impuestos especificos sobre bebidas alcoholicas.`,
                           `Impuestos especificos sobre bebidas envasadas sin contenido alcoholico`,
                           `Impuestos especificos sobre los jabones de tocador`,
                           `Impuesto al Cemento`,
                           `Impuestos a los Productos del Tabaco`,
                           `Impuesto a casinos`,
                           `Impuesto a empresas de enlace de apuestas electronicas.`,
                           `Arancel de Aduanas`,
                           `1% sobre el valor aduanero de las mercancias`,
                           `Derechos sobre exportacion de banano`,
                           `Otros impuestos a las exportaciones`,
                           `¢1.5 Por caja de banano exportado.`,
                           `Impuestos a las exportaciones por via terrestre`,
                           `Derechos de salida del territorio nacional por via aerea`,
                           `Derechos de salida del Territorio Nacional por via terrestre`,
                           `Derechos consulares`,
                           `Fondo Social Migratorio`,
                           `Fondo Especial de Migracion`,
                           `Otros Impuestos Migratorios `,
                           `Impuesto General Forestal`,
                           `Timbre Fiscal`,
                           `Papel Sellado`,
                           `Contribuciones Sociales`,
                           `Ingresos No Tributarios`,
                           `Derechos Administrativos`,
                           `25% utilidades del INS`,
                           `Multas, sanciones, remates y comisos`,
                           `Transferencias Corrientes`,
                           `Ingresos de Capital`
                         )


#### Archivo de impuestos

Impuestos <- Impuestos %>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)

Impuestos <- as.data.frame(Impuestos)


#### Archivo final de los Ingresos mensuales 

Ingresos_m <- Ingresos_m  %>%
  select(Año, Nivel, n_Nivel ,PosPre, cod_sector,'Presupuesto actual', mes.cod,mes,cod_clase,clase,cod_subclase,subclase,cod_grupo,grupo,cod_subgrupo,subgrupo,
         cod_partida,partida,cod_subpartida,subpartida,cod_renglon,renglon,cod_subrenglon,subrenglon,
         cod_fuentefinanciacion,fuentefinanciacion,cod_fuentefinanciacion_3,Descripción,Ingresos)
        


##################################################################################################
##################################################################################################
##################################################################################################
#                                  Ingresos  Anuales                                             #
##################################################################################################
##################################################################################################
##################################################################################################

#############################
#   Ingresos  20167-2020     #
#############################

ingresos_2007_2021_a <- ingresos_2007_2021 

# dim(ingresos_2016_2020)

###############################
#   Ingresos  SIGAF  actual   #
###############################

#SIGAF_Ingresos_actual_2022 <- SIGAF_Ingresos_actual_2022
#names(SIGAF_Ingresos_actual_2022)

# Creación de la variable Año #

SIGAF_Ingresos_actual_a <- SIGAF_Ingresos_actual %>%
                                                   mutate( 
                                                           Año = Ano_actual)





# cbind(names(ingresos_2007_2021_a), names(SIGAF_Ingresos_2022_a))
# dim(ingresos_2007_2021_a) ; dim(SIGAF_Ingresos_2022_a)


######################################
# Merge de las tablas por la fuerza  # 
######################################



Ingresos_a <- dplyr::bind_rows(ingresos_2007_2021_a, SIGAF_Ingresos_actual_a) 


###########################################################
#       Transformación de datos                           #
###########################################################

Ingresos_a  <- dplyr::mutate(Ingresos_a,
                             
                             n_Nivel = case_when(
                               Nivel == "1" ~ "Clase",
                               Nivel == "2" ~ "Subclase",
                               Nivel == "3" ~ "Grupo",
                               Nivel == "4" ~ "Subgrupo",
                               Nivel == "5" ~ "Partida",
                               Nivel == "6" ~ "Subpartida",
                               Nivel == "7" ~ "Renglón",
                               Nivel == "8" ~ "Subrenglón"
                             )
                             
)

Ingresos_a <- dplyr::mutate(Ingresos_a,
  
                             #################################################################
                             # Creación del clasificador de los ingresos del sector público  #
                             #################################################################
                             
                            Pospre = substr(Ingresos_a$`PosPre`, 2, 14),
                            Pospreanual = paste0(Año,Pospre),
                            
                            cod_clase = substr(Ingresos_a$`PosPre`, 2, 2),
                            
                            clase = case_when(
                              cod_clase == "1" ~ "Ingresos corrientes",
                              cod_clase == "2" ~ "Ingresos de capital",
                              cod_clase == "3" ~ "Financiamiento"
                            ),
                            
                            cod_subclase = substr(Ingresos_a$`PosPre`, 2, 3),
                            
                            subclase = case_when(
                              
                              cod_subclase == "11" ~ "Ingresos tributarios",
                              cod_subclase == "12" ~ "Contribuciones sociales",
                              cod_subclase == "13" ~ "Ingresos no tributarios",
                              cod_subclase == "14" ~ "Transferencias corrientes",
                              
                              cod_subclase == "21" ~ "Venta de activos", # NS
                              cod_subclase == "23" ~ "Recuperación de préstamos e inversiones financieras", # NS
                              cod_subclase == "24" ~ "Transferencias de capital",
                              cod_subclase == "25" ~ "Otros ingresos de capital", # NS
                              
                              cod_subclase == "31" ~ "Financiamiento interno",
                              cod_subclase == "32" ~ "Financiamiento externo",
                              cod_subclase == "33" ~ "Recursos de vigencias anteriores"
                              
                            ),
                            
                            cod_grupo = substr(Ingresos_a$`PosPre`, 2, 4),
                            
                            grupo = case_when(
                              
                              #######  Asignaciones  ############
                              
                              cod_grupo == "111" ~ "Impuestos a los ingresos y utilidades",
                              cod_grupo == "112" ~ "Impuestos sobre la propiedad",
                              cod_grupo == "113" ~ "Impuestos sobre bienes y servicios",
                              cod_grupo == "114" ~ "Impuestos sobre el comercio exterior y las transacciones internacionales",
                              cod_grupo == "119" ~ "Otros ingresos tributarios",
                              
                              cod_grupo == "121" ~ "Contribuciones a la seguridad social",
                              
                              cod_grupo == "131" ~ "Venta de bienes y servicios",
                              cod_grupo == "132" ~ "Ingresos de la propiedad",
                              cod_grupo == "133" ~ "Multas, sanciones, remates y comisos",
                              cod_grupo == "134" ~ "Intereses Moratorios",
                              cod_grupo == "139" ~ "Otros ingresos no tributarios",
                              
                              cod_grupo == "141" ~ "Transferencias corrientes del sector público",
                              cod_grupo == "142" ~ "Transferencias corrientes del sector privado",
                              cod_grupo == "143" ~ "Transferencias corrientes del sector externo",
                              
                              cod_grupo == "211" ~ "Venta de activos fijos", # NS
                              cod_grupo == "234" ~ "Recuperación de inversiones financieras",
                              cod_grupo == "243" ~ "Transferencias de capital del sector externo", # NS
                              cod_grupo == "241" ~ "Transferencias de capital del sector público",
                              
                              
                              cod_grupo == "313" ~ "Colocación de títulos valores",
                              
                              
                              cod_grupo == "321" ~ "Créditos directos",  # Préstamos directos 
                              cod_grupo == "323" ~ "Colocación de títulos valores en el exterior", # NS
                              
                              cod_grupo == "331" ~ "Superávit libre",
                              cod_grupo == "332" ~ "Superávit específico"
                              
                              ####### Remplazar por "-" #######
                            ),
                            
                            cod_subgrupo = substr(Ingresos_a$`PosPre`, 2, 5),
                            
                            subgrupo = case_when(
                              
                              #######  Asignaciones  ############
                              
                              cod_subgrupo == "1111" ~ "Impuestos sobre los ingresos y utilidades de personas físicas",
                              cod_subgrupo == "1112" ~ "Impuestos sobre los ingresos y utilidades de personas jurídicas",
                              
                              cod_subgrupo == "1113" ~ "Impuestos sobre dividendos e intereses de títulos valores", # NS
                              cod_subgrupo == "1114" ~ "Impuesto sobre remesas al exterior",
                              cod_subgrupo == "1115" ~ "Impuesto especial sobre bancos y entidades financieras no domiciliadas", # NS
                              
                              cod_subgrupo == "1121" ~ "Impuesto sobre la propiedad de bienes inmuebles",
                              cod_subgrupo == "1122" ~ "Impuesto sobre la propiedad de vehículos, aeronaves y embarcaciones",
                              cod_subgrupo == "1123" ~ "Impuestos sobre el patrimonio",
                              cod_subgrupo == "1124" ~ "Impuestos sobre el traspaso de bienes inmuebles",
                              cod_subgrupo == "1125" ~ "Impuestos sobre el traspaso de vehículos, aeronaves y embarcaciones ",
                              
                              cod_subgrupo == "1131" ~ "Impuesto general sobre ventas y consumo",
                              cod_subgrupo == "1132" ~ "Impuestos específicos sobre la producción y consumo de bienes y servicios",
                              
                              cod_subgrupo == "1141" ~ "Impuestos a las importaciones",
                              cod_subgrupo == "1142" ~ "Impuestos a las exportaciones",
                              cod_subgrupo == "1143" ~ "Otros impuestos sobre el comercio exterior y las transacciones internacionales",
                              
                              cod_subgrupo == "1191" ~ "Impuesto de timbres",
                              cod_subgrupo == "1199" ~ "Ingresos tributarios diversos",
                              
                              cod_subgrupo == "1213" ~ "Contribución a regímenes especiales de pensiones",
                              
                              cod_subgrupo == "1219" ~ "OTRAS CONTRIBUCIONES SOCIALES FODESAF", # NUEVO
                              
                              cod_subgrupo == "1311" ~ "Venta de bienes",
                              cod_subgrupo == "1312" ~ "Venta de servicios",
                              cod_subgrupo == "1313" ~ "Derechos administrativos",
                              
                              cod_subgrupo == "1321" ~ "Traspaso de dividendos",
                              cod_subgrupo == "1322" ~ "Renta de la propiedad",
                              cod_subgrupo == "1323" ~ "Renta de activos financieros",
                              
                              cod_subgrupo == "1331" ~ "Multas y sanciones",
                              cod_subgrupo == "1332" ~ "Remates y comisos",
                              cod_subgrupo == "1341" ~ "Intereses moratorios por atraso en pago de impuesto", # CN
                              cod_subgrupo == "1342" ~ "Intereses moratorios por atraso en pago de bienes y servicios", # CN
                              cod_subgrupo == "1349" ~ "Otros intereses moratorios", # NUEVO
                              
                              cod_subgrupo == "1391" ~ "Reintregros y devoluciones", #CN
                              
                              cod_subgrupo == "1392" ~ "Ejecución de contratos de seguros", #NS
                              cod_subgrupo == "1399" ~ "Ingresos varios no especificados",
                              
                              cod_subgrupo == "1411" ~ "Transferencias corrientes del Gobierno Central", #NS
                              cod_subgrupo == "1412" ~ "Transferencias corrientes de Órganos Desconcentrados", #NS
                              cod_subgrupo == "1413" ~ "Transferencias corrientes de Instituciones Descentralizadas No Empresariales",
                              cod_subgrupo == "1414" ~ "Transferencias corrientes de Gobiernos Locales",
                              cod_subgrupo == "1415" ~ "Transferencias corrientes de Empresas Públicas no Financieras",
                              cod_subgrupo == "1416" ~ "Transferencias corrientes de Instituciones Públicas Financieras",
                              
                              cod_subgrupo == "1421" ~ "Transferencias corrientes del sector privado", # NS
                              cod_subgrupo == "1423" ~ "Transferencias corrientes del sector privado", # NS
                              cod_subgrupo == "1424" ~ "Transferencias corrientes del sector privado", # NS
                              cod_subgrupo == "1425" ~ "Transferencias corrientes del Sector Privado-Superávit Libre artículo 5 Ley 9371", # NS
                              cod_subgrupo == "1426" ~ "Transferencias de aseguradoras privadas para Financ. INEC Ley 9694", # Nuevo nombre
                              cod_subgrupo == "1427" ~ "Transferencias corrientes del Sector Privado-según Convenio Ma rco de Cooperación no Reembolsable entre INCIENSA y FUNIN del 25 de octubre de 2007 (INCIENSA)",
                              cod_subgrupo == "1428" ~ "Transferencias corrientes del Sector Privado- Superávit Libre artículo 17 Tít ulo IV Ley 9635",
                              
                              cod_subgrupo == "1431" ~ "Transfererencias corrientes de organismos internacionales",
                              cod_subgrupo == "1432" ~ "Transferencias corrientes de gobiernos extranjeros", # NS
                              
                              cod_subgrupo == "2111" ~ "Venta de terrenos", # NS
                              cod_subgrupo == "2113" ~ "Venta de maquinaria y equipo", # NS
                              
                              cod_subgrupo == "2412" ~ "Transferencias de capital de Órganos Desconcentrados", # NS
                              cod_subgrupo == "2413" ~ "Transferencias de capital de Instituciones Descentralizadas No Empresariales",
                              cod_subgrupo == "2416" ~ "Transferencias de capital de Instituciones Públicas Financieras", # NS
                              
                              cod_subgrupo == "2431" ~ "Transferencias de capital de organismos internacionales", # NS
                              cod_subgrupo == "2432" ~ "Transferencias de capital de gobiernos extranjeros", # NS
                              
                              cod_subgrupo == "3131" ~ "Títulos valores",
                              
                              cod_subgrupo == "3211" ~ "Créditos de organismos internacionales de desarrollo",
                              cod_subgrupo == "3212" ~ "Créditos de gobiernos extranjeros", 
                              cod_subgrupo == "3213" ~ "Créditos de bancos privados", # NS
                              
                              cod_subgrupo == "3311" ~ "Superávit Libre", # NUEVO 
                              
                              cod_subgrupo == "3321" ~ "Superávit Específico", 
                              
                              cod_subgrupo == "3232" ~ "Colocación de títulos valores de largo plazo"
                              
                              ####### Remplazar por "-" #######
                            ),
                            
                            cod_partida = substr(Ingresos_a$`PosPre`, 2, 7),
                            
                            partida = case_when(
                              
                              #######  Asignaciones  ############
                              
                              cod_partida == "111101" ~ "Impuesto  sobre  salarios,  jubilaciones,  pensiones  y otros pagos laborales del Sector Público",
                              cod_partida == "111102" ~ "Impuesto  sobre  salarios,  jubilaciones,  pensiones  y otros pagos laborales del Sector Privado",
                              cod_partida == "111103" ~ "Impuestos sobre los ingresos y utilidades de personas físicas",
                              
                              cod_partida == "111201" ~ "Impuestos sobre los ingresos y utilidades de las personas jurídicas del Sector Público",
                              cod_partida == "111202" ~ "Impuestos sobre los ingresos y utilidades de las personas jurídicas del Sector Privado",
                              
                              cod_partida == "111301" ~ "Impuesto sobre dividendos", # NS
                              cod_partida == "111302" ~ "Impuestos sobre los intereses de títulos valores", # NS
                              
                              cod_partida == "111401" ~ "Impuesto sobre remesas al exterior",
                              
                              cod_partida == "111501" ~ "Impuesto especial sobre bancos y entidades financieras no domiciliadas", # NS
                              
                              cod_partida == "112101" ~ "Impuesto solidario de vivienda Ley N°8683",
                              cod_partida == "112201" ~ "Impuesto a la propiedad de vehículos Ley N°7088",
                              cod_partida == "112202" ~ "Timbre Fauna Silvestre Ley N°7317",
                              
                              cod_partida == "112301" ~ "Incremento Timbre de Educación y Cultura Ley N° 6879",
                              cod_partida == "112302" ~ "Impuesto sobre personas jurídicas Ley N° 9024", # NS
                              cod_partida == "112303" ~ "Impuesto a las Personas Jurídicas Ley 9428",
                              
                              cod_partida == "112401" ~ "Impuestos sobre el traspaso de bienes inmuebles",
                              cod_partida == "112501" ~ "Impuesto sobre el traspaso de vehículos usados Ley N° 7088",
                              
                              cod_partida == "113101" ~ "Impuesto general sobre las ventas",
                              cod_partida == "113102" ~ "Impuesto selectivo de consumo",
                              
                              cod_partida == "113201" ~ "Impuestos específicos sobre la producción y consumo de bienes",
                              cod_partida == "113202" ~ "Impuestos específicos sobre la producción y consumo de servicios",
                              
                              cod_partida == "114101" ~ "Derechos de importación de mercancías",
                              cod_partida == "114102" ~ "Impuestos sobre el valor aduanero de las mercancías",
                              cod_partida == "114109" ~ "Otros impuestos a las importaciones", # NS
                              
                              
                              cod_partida == "114201" ~ "Derechos de exportación de mercancías",
                              cod_partida == "114209" ~ "Otros impuestos a las exportaciones",
                              
                              cod_partida == "114301" ~ "Impuestos por movilización de carga portuaria", #NS
                              cod_partida == "114302" ~ "Impuesto por uso de terminal portuaria", #NS
                              cod_partida == "114303" ~ "Impuesto de salida al exterior",
                              cod_partida == "114304" ~ "Derechos consulares",
                              cod_partida == "114309" ~ "Otros impuestos sobre el comercio exterior y las transacciones internacionales",
                              
                              cod_partida == "119101" ~ "Timbre Fiscal Ley N° 7208", #
                              
                              cod_partida == "119102" ~ "Timbre del Registro Nacional",
                              cod_partida == "119103" ~ "Timbre Propiedad Intelectual - Registro Nacional", #NS
                              cod_partida == "119104" ~ "Timbre Portal Web - Registro Nacional", #NS
                              cod_partida == "119105" ~ "Placas - Registro Nacional", #NS
                              cod_partida == "119106" ~ "Timbre Gobierno Digital - Registro Nacional", #NS
                              cod_partida == "119107" ~ "Timbre Colegio Abogados - Direccion Nacional de No tariado", #NS
                              cod_partida == "119108" ~ "Timbre Archivo Nacional", #NS
                              cod_partida == "119109" ~ "Timbre Educacion y Cultura inciso c) artículo 10 L ey 5923 y sus reformas Museo Nacional", #NS
                              cod_partida == "119110" ~ "Timbre Educacion y Cultura inciso d) artículo 10 L ey 5923 y sus reformas SINEM", #NS
                              cod_partida == "119111" ~ "Timbre Pro-Parques Nacionales SINAC", #NS
                              cod_partida == "119112" ~ "Timbre de Vida Silvestre SINAC", #NS
                              cod_partida == "119113" ~ "Timbre Pro-Parques Nacionales CONAGEBIO", #NS
                              
                              cod_partida == "119901" ~ "Papel Sellado Ley N° 7345",
                              cod_partida == "119902" ~ "Ingresos por distribuir", # NS
                              
                              cod_partida == "121301" ~ "Contribución del Magisterio Nacional de miembros activos", # NN
                              cod_partida == "121302" ~ "Contribución del Magisterio Nacional miembros pensionados y jubilados",
                              cod_partida == "121303" ~ "Contribución a otros regímenes de pensiones",
                              cod_partida == "121304" ~ "Contribución Especial Solidaria Ley 9383",  # NS
                              
                              cod_partida == "121902" ~ "Contribución patronal sobre la nómina de Órganos Desconcentrados", # Nuevo
                              cod_partida == "121903" ~ "Contribución patronal sobre la nómina de Instituci ones Descentralizadas no Empresariales", # Nuevo
                              cod_partida == "121904" ~ "Contribución patronal sobre la nómina de Gobiernos Locales", # Nuevo
                              cod_partida == "121905" ~ "Contribución patronal sobre la nómina de Empresas Públicas No Financieras", # Nuevo
                              cod_partida == "121906" ~ "Contribución patronal sobre la nómina de Instituci ones Públicas Financieras", # Nuevo
                              cod_partida == "121907" ~ "Contribución patronal sobre la nómina del Sector P rivado", # Nuevo
                              cod_partida == "131101" ~ "Venta de bienes agropecuarios y forestales", # Nuevo
                              
                              cod_partida == "131109" ~ "Venta de otros bienes",
                              
                              cod_partida == "131201" ~ "Servicios de transporte",
                              cod_partida == "131203" ~ "Servicios financieros y de seguros",
                              cod_partida == "131204" ~ "Alquileres y otros alquileres",
                              cod_partida == "131209" ~ "Otros servicios",
                              
                              cod_partida == "131301" ~ "Derechos administrativos a los servicios de transporte",
                              cod_partida == "131302" ~ "Derechos administrativos a otros servicios públicos",
                              
                              cod_partida == "132101" ~ "20% utilidades del IMAS", #NS
                              cod_partida == "132102" ~ "Acciones CEMEX-CR SA", #NS
                              cod_partida == "132103" ~ "Utilidades INS (25%)", 
                              
                              cod_partida == "132201" ~ "Concesión de obra pública",
                              cod_partida == "132202" ~ "Alquiler de terrenos",
                              cod_partida == "132209" ~ "Otros ingresos de la renta de la propiedad",
                              
                              cod_partida == "132301" ~ "Intereses sobre títulos valores",
                              cod_partida == "132302" ~ "Intereses y comisiones sobre préstamos", #NS
                              cod_partida == "132303" ~ "Otras rentas de activos financieros",
                              
                              cod_partida == "133101" ~ "Multas de tránsito",
                              cod_partida == "133102" ~ "Multas por atraso en pago de impuestos",
                              cod_partida == "133104" ~ "Sanciones administrativas y judiciales", #NN
                              cod_partida == "133109" ~ "Otras multas y sanciones",
                              
                              cod_partida == "133201" ~ "Remates y confiscaciones",
                              cod_partida == "134200" ~ "Intereses moratorios por atraso en pago de bienes y servicios",
                              
                              cod_partida == "134901" ~ "Otros intereses moratorios sobre multas de tránsit o COSEVI",  #nuevo
                              cod_partida == "139200" ~ "Ejecución de contratos de seguros",
                              cod_partida == "139901" ~ "Ingresos varios no especificados", #nuevo
                              cod_partida == "139902" ~ "Ingresos varios no especificados COSEVI", #nuevo
                              cod_partida == "139903" ~ "Ingresos varios no especificados Instituto Costarr icense sobre Drogas (ICD)", #nuevo
                              cod_partida == "139904" ~ "Otros ingresos varios no especificados Artículo 44 Ley 8488 Comisión Nacional de Prevención y Atenci ón de Emergencias (CNE)", #nuevo
                              
                              cod_partida == "141100" ~ "Donaciones corrientes del Gobierno Central",
                              cod_partida == "141101" ~ "Donaciones corrientes del Gobierno Central", #NS
                              cod_partida == "141102" ~ "Transferencias corrientes Gobierno Central Superáv it Libre artículo 5 Ley 9371", #NS
                              
                              cod_partida == "141201" ~ "Transferencias de FODESAF", #NS
                              cod_partida == "141202" ~ "Transferencias de la Junta Administrativa del Registro Nacional Ley N° 7138", #NS
                              cod_partida == "141203" ~ "Consejo de Seguridad Vial", #NS
                              cod_partida == "141204" ~ "PROMECE", #NS
                              cod_partida == "141205" ~ "CNE Ley 8933", #NS
                              cod_partida == "141206" ~ "CTAC Ley 5222", #NS
                              cod_partida == "141207" ~ "Direc.Geología Minas", #NS
                              cod_partida == "141208" ~ "SINAC", #NS
                              cod_partida == "141209" ~ "Unidad Ejecutora del Programa de Catastro y Registro", #NS
                              cod_partida == "141211" ~ "Instituto Meteorológico Nacional", #NS
                              cod_partida == "141212" ~ "Consejo Superior de Educación", #NS
                              cod_partida == "141213" ~ "Unidad Ejecutora Programa para la Prevención de la Violencia y Promoción de la Inclusión Social del Ministerio de Justicia y Paz", #NS
                              cod_partida == "141214" ~ "Transferencias corrientes Órganos Desconcentrados Superávit Libre artículo 5 Ley 9371", #NS
                              
                              cod_partida == "141301" ~ "Cuotas a Organismos Internacionales, Ley N° 3418",
                              cod_partida == "141303" ~ "Junta de Desarrollo de la Zona Sur. Ley N° 7730",
                              cod_partida == "141304" ~ "Instituto Nacional de Estadística y Censos Ley N° 7839 artículo 13 -Convenio BCCR-INEC-MH para Estudio Económico a Empresas",
                              cod_partida == "141305" ~ "SENARA Ley 7064", #NS
                              cod_partida == "141306" ~ "INDER", #NS
                              cod_partida == "141307" ~ "Instituto de Desarrollo Rural Fideicomiso Pequeños Medianos productores de piña.", #NS
                              cod_partida == "141308" ~ "Instituto Nacional de Fomento y Asesoría Municipal recursos convenio MIVAH-PRUGAM", #NS
                              cod_partida == "141309" ~ "Servicios Nacional de Aguas Subterráneas, Riego y Avenamiento (SENARA)",
                              cod_partida == "141310" ~ "Transferencias corrientes Instituciones Descentral izadas no Empresariales Superávit Libre artículo 5 Ley 9371", #NS
                              
                              cod_partida == "141311" ~ "Transferencia Instituto Costarricense de Pesca y A cuicultura artículo 3 inciso h) Ley 8149 - INTA", #Nuevo
                              cod_partida == "141312" ~ "Transferencias corrientes Instituciones Descentral izadas No Empresariales a Consejo Técnico de Aviac ión Civil (CTAC)", #Nuevo
                              cod_partida == "141313" ~ "Transferencia Instituto de Desarrollo Rural Ley N° 9036 a Instituto sobre Alcoholismo y Farmacodepen dencia (IAFA)", #Nuevo
                              cod_partida == "141317" ~ "Transferencias corrientes Instituciones Descentrallizadas no Empresariales Reintegro de recursos artículo 4 Ley 9840",
                              
                              cod_partida == "141401" ~ "Transferencias de Municipalidades Ley N° 7729",
                              cod_partida == "141402" ~ "Transferencias de Municipalidades Ley N° 7755",
                              cod_partida == "141403" ~ "Transferencias de Gobiernos locales Superávit Libre artículo 5 Ley Nº 9371",
                              
                              cod_partida == "141404" ~ "Transferencia de Gobiernos Locales Ley 9303 a Cons ejo Nacional de Personas con Discapacidad (CONAPDI S)", # Nuevo
                              cod_partida == "141405" ~ "Transferencia de Gobierno Locales Ley 7509 a Regis tro Nacional", # Nuevo
                              cod_partida == "141406" ~ "Transferencia de Gobierno Locales Ley 7509 a Casa Cultura Puntarenas", # Nuevo
                              cod_partida == "141407" ~ "Transferencias de Gobiernos Locales Ley 8461 a Parque Marino del Pacífico",
                              
                              cod_partida == "141501" ~ "Cuotas Organismos Internacionales, Ley N° 3418",
                              cod_partida == "141502" ~ "RECOPE Ley N° 7399 Art 56",
                              cod_partida == "141504" ~ "Refinadora Costarricense de Petroleo art. 5 y 6 Ley N°9840 Emergencia COVID-19",
                              
                              cod_partida == "141505" ~ "Trasferencia Junta de Protección Social Ley 9379 a Consejo Nacional de Personas con Discapacidad (CO NAPDIS)", #Nuevo
                              cod_partida == "141506" ~ "Transferencia Junta de Protección Social Ley 8718 a Consejo Técnico Asistencia Médico Social (CTAMS)", # Nuevo
                              cod_partida == "141507" ~ "Transferencia Junta de Protección Social Ley 8111 a Comisión Nacional de Vacunación y Epidemiología (CNVE)", #Nuevp
                              cod_partida == "141508" ~ "Transferencia Fábrica Nacional de Licores (Ley N° 8289) a Instituto sobre Alcoholismo y Farmacodepen dencia (IAFA)", #Nuevo
                              cod_partida == "141511" ~ "Junta de Protección Social Ley 8718 art. 8 inc. d) a INCIENSA",
                              
                              cod_partida == "141601" ~ "Cuotas Organismos Internacionales, Ley N° 3418",
                              cod_partida == "141602" ~ "INS Parques Culturales // Transferencias de aseguradoras públicas para Financ. INEC Ley 9694", # Ojo a esta
                              
                              
                              cod_partida == "141604" ~ "Transferencia INS - Consejo Salud Ocupacional", 
                              
                              cod_partida == "142101" ~ "Código Notalerial Ley 7764",
                              
                              
                              cod_partida == "143101" ~ "Donación BIRF",
                              cod_partida == "143102" ~ "Donación PNUD",
                              cod_partida == "143103" ~ "Donación BCIE",
                              
                              
                              cod_partida == "143104" ~ "Transferencias de organismos internacionales al Archivo Nacional", #No SIGAF en casi todos 
                              
                              cod_partida == "143202" ~ "Donación República de China",
                              cod_partida == "143203" ~ "Donación Unión Europea",
                              cod_partida == "143204" ~ "DONACION KOLFACI",
                              cod_partida == "143205" ~ "Subvención de la Unión Europea para el Fortalecimi ento del Programa Justicia Restaurativa- Poder Judicial",
                              
                              cod_partida == "241203" ~ "Transf.Captal Inst M",
                              cod_partida == "241205" ~ "Unidad Coordinadora Proyecto Limón Ciudad Puerto",
                              cod_partida == "241206" ~ "Dir. Geologias Minas",
                              cod_partida == "241207" ~ "IMN",
                              cod_partida == "241208" ~ "FODESAF - Min. Cultu",
                              cod_partida == "241209" ~ "Comisión Nacional de Emergencias Ley No. 8933",
                              cod_partida == "241210" ~ "Consejo Nacional de la Política Pública de la Persona Joven (CPJ)",
                              cod_partida == "241211" ~ "Museo de Arte Costarricense",
                              cod_partida == "241212" ~ "Museo de Arte y Diseño Contemporáneo (MADC)",
                              cod_partida == "241213" ~ "Sistema Nacional de Educación Musical (SINEM)",
                              cod_partida == "241214" ~ "Dirección Nacional de Centros de Educación y Nutri ción y de Centros Infantiles de Atención Integral (Dirección de CEN-CINAI)",
                              
                              cod_partida == "241301" ~ "Instituto Nacional de Aprendizaje. Ley Nº7372 de 22-11-93", # Nuevo nombre
                              
                              cod_partida == "241602" ~ "Instituto Nacional de Seguros. Atención emergencia COVID-19 Ley N°9847",
                              
                              cod_partida == "243102" ~ "Donación BIRF",
                              
                              cod_partida == "243204" ~ "Donación China",
                              
                              cod_partida == "313101" ~ "Colocación de títulos valores de corto plazo",
                              cod_partida == "313102" ~ "Colocación de títulos valores de largo plazo",
                              
                              cod_partida == "321101" ~ "Banco Centroaméricano de Integración Económica",
                              cod_partida == "321102" ~ "Banco Interamericano de Desarrollo",
                              cod_partida == "321103" ~ "Banco Mundial",
                              cod_partida == "321105" ~ "Fondo Internacional de Desarrollo Agropecuario",
                              cod_partida == "321109" ~ "Otros Créditos de organismos internacionales de desarrollo",
                              
                              cod_partida == "331101" ~ "Superávit Libre",
                              cod_partida == "332101" ~ "Superávit Específico",
                              
                              cod_partida == "321203" ~ "Otros Créditos de organismos internacionales de desarrollo", # Nuevo
                              
                              ####### Remplazar por "" #######
                              
                              cod_partida == "119102" ~ "",
                              cod_partida == "119103" ~ "",
                              cod_partida == "119104" ~ "",
                              cod_partida == "119105" ~ "",
                              cod_partida == "119106" ~ "",
                              cod_partida == "119107" ~ "",
                              cod_partida == "119108" ~ "",
                              cod_partida == "119109" ~ "",
                              cod_partida == "119110" ~ "",
                              cod_partida == "119111" ~ "",
                              cod_partida == "119112" ~ "",
                              cod_partida == "119113" ~ "",
                              cod_partida == "121902" ~ "",
                              cod_partida == "121903" ~ "",
                              cod_partida == "121904" ~ "",
                              cod_partida == "121905" ~ "",
                              cod_partida == "121906" ~ "",
                              cod_partida == "121907" ~ "",
                              cod_partida == "131101" ~ "",
                              cod_partida == "131201" ~ "",
                              cod_partida == "132201" ~ "",
                              cod_partida == "132209" ~ "",
                              cod_partida == "134901" ~ "",
                              
                              cod_partida == "139901" ~ "",
                              cod_partida == "139902" ~ "",
                              cod_partida == "139903" ~ "",
                              cod_partida == "139904" ~ "",
                              cod_partida == "141311" ~ "",
                              cod_partida == "141312" ~ "",
                              cod_partida == "141313" ~ "",
                              cod_partida == "141404" ~ "",
                              cod_partida == "141405" ~ "",
                              cod_partida == "141406" ~ "",
                              cod_partida == "141505" ~ "",
                              cod_partida == "141506" ~ "",
                              cod_partida == "141507" ~ "",
                              cod_partida == "141508" ~ "",
                              cod_partida == "141604" ~ "",
                              cod_partida == "143104" ~ "",
                              
                              cod_partida == "321203" ~ "",
                              cod_partida == "323201" ~ "",
                              
                              cod_partida == "331103" ~ "",
                              cod_partida == "332102" ~ "",
                              
                              cod_partida == "141314" ~ "Transferencias corrientes Superávit Libre Ley 9925 Adición Transitorio III Ley 9371",
                              cod_partida == "141215" ~ "Transferencias corrientes Superávit Libre Ley 9925 Adición Transitorio III Ley 9371",
                              cod_partida == "141302" ~ "Instituto Nacional de Aprendizaje. Ley Nº7372 / 22 -11-93",
                              
                              
                              ####### Nombre en la jerarquía, poner un "-" #######
                              
                              cod_partida == "321200" ~ "-",
                              cod_partida == "134100" ~ "-",
                              cod_partida == "139900" ~ "",
                              cod_partida == "139100" ~ "Reintegros de efectivo Ley Nº 7056",
                              cod_partida == "141510" ~ "Transferencias Empresas Públicas no Financieras Su perávit Libre Ley 9925 adición Transitorios III y IV Ley 9371",
                              cod_partida == "142500" ~ "-",
                              
                              cod_partida == "142600" ~ "-"
                              
                            ),
                            
                            cod_subpartida = substr(Ingresos_a$`PosPre`, 2, 9),
                            
                            subpartida = case_when(
                              
                              #######  Asignaciones  ############
                              
                              cod_subpartida == "11110101" ~ "Impuesto sobre salarios, jubilaciones, pensiones y otros pagos laborales del Sector Público", # Nuevo
                              cod_subpartida == "11110201" ~ "Impuesto sobre salarios, jubilaciones, pensiones y otros pagos laborales del Sector Privado", # Nuevo
                              cod_subpartida == "11110301" ~ "IMPUESTO SOBRE LOS INGRESOS Y UTILIDADES DE PERSON AS FÍSICAS", # Nuevo
                              
                              
                              cod_subpartida == "11110302" ~ "Impuesto sobre rentas de capital inmobiliario de personas físicas",
                              cod_subpartida == "11110303" ~ "Impuesto sobre rentas de capital mobiliario de personas físicas",
                              cod_subpartida == "11110304" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas físicas"
                              ,
                              
                              cod_subpartida == "11120101" ~ "Impuesto sobre los ingresos y utilidades de las personas jurídicas del Sector Público",
                              cod_subpartida == "11120102" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del sector público",
                              cod_subpartida == "11120103" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del sector público",
                              
                              cod_subpartida == "11120202" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del Sector Privado",
                              cod_subpartida == "11120203" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del Sector Privado",
                              cod_subpartida == "11120204" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas jurídicas del sector privado", # NS
                              
                              cod_subpartida == "11120105" ~ "Impuesto a los ingresos y utilidades artículo 46 L ey 8488 CNE", # nuevo
                              cod_subpartida == "11120201" ~ "Impuesto sobre los ingresos y utilidades de las personas jurídicas del Sector Privado",# nuevo
                              cod_subpartida == "11120202" ~ "Impuesto sobre rentas de capital inmobiliario de personas jurídicas del Sector Privado",# nuevo
                              cod_subpartida == "11120203" ~ "Impuesto sobre rentas de capital mobiliario de personas jurídicas del Sector Privado",# nuevo
                              cod_subpartida == "11120204" ~ "Impuesto sobre las ganancias y pérdidas de capital de personas jurídicas del sector privado",# nuevo
                              
                              cod_subpartida == "11310101"~ "Impuesto sobre la venta de bienes y servicios internos",
                              cod_subpartida == "11310102"~ "Impuesto sobre la venta de bienes y servicios importados",
                              
                              cod_subpartida == "11310201" ~ "Impuesto selectivo de consumo de bienes internos",
                              cod_subpartida == "11310202" ~ "Impuesto selectivo de consumo de bienes importados",
                              
                              cod_subpartida == "11320101" ~ "Impuestos específicos sobre productos agropecuarios y forestales",
                              cod_subpartida == "11320102" ~ "Impuestos específicos sobre la explotación de recursos naturales y minerales",
                              cod_subpartida == "11320103" ~ "Impuestos sobre combustibles y energéticos",
                              cod_subpartida == "11320104" ~ "Impuestos sobre bienes manufacturados",
                              cod_subpartida == "11320109" ~ "Otros impuestos específicos sobre la producción y consumo de bienes",
                              
                              
                              cod_subpartida == "11320201" ~ "Impuestos específicos a los servicios de hospedaje", #NS
                              cod_subpartida == "11320202" ~ "Impuestos específicos a los servicios de transporte",
                              cod_subpartida == "11320203" ~ "Impuestos específicos a los servicios de diversión y esparcimiento",
                              
                              cod_subpartida == "11410101" ~ "Arancel de aduanas",
                              
                              cod_subpartida == "11410201" ~ "1% sobre el valor aduanero de las mercancías",
                              cod_subpartida == "11410202" ~ "1,5% valor CIF de productos agroquímicos SFE", # Nuevo
                              
                              cod_subpartida == "11420101" ~ "Derecho sobre exportación del banano",
                              cod_subpartida == "11420901" ~ "¢1.5 p/caja Ban Expo",
                              cod_subpartida == "11420902" ~ "Impuestos exportación vía terrestre",
                              
                              cod_subpartida == "11430301" ~ "Derecho de salida del territorio nacional vía áerea",
                              cod_subpartida == "11430302" ~ "Derecho de salida del territorio nacional vía terrestre",
                              
                              cod_subpartida == "11430401" ~ "Derechos consulares Ley N° 7293",
                              
                              cod_subpartida == "11430901" ~ "Impuestos Ley de Migración N° 8764",
                              cod_subpartida == "11430902" ~ "Impuesto General Forestal ",
                              
                              cod_subpartida == "11990201" ~ "Anticipos de aduana ",
                              cod_subpartida == "11990202" ~ "Anticipos impuesto al banano",
                              
                              cod_subpartida == "12130101" ~ "Contribución Magisterio Nacional - Activos. Ley Nº 7531", # Nuevo
                              cod_subpartida == "12130102" ~ "Contribución por traslado al régimen de Reparto L ey 8721", # Nuevo
                              cod_subpartida == "12130201" ~ "Contribución Magisterio Nacional - Pensionados. Le y Nº 7531", # Nuevo
                              
                              
                              cod_subpartida == "12130301" ~ "Deducción sueldos para pensionados Ley Nº 7302",
                              cod_subpartida == "12130302" ~ "Contribución Especial Solidaria Ley 9383", # nuevo
                              
                              
                              cod_subpartida == "13110101" ~ "Venta de Bienes Estaciones Experimentales INTA", # nuevo
                              
                              
                              cod_subpartida == "13110901" ~ "Venta bienes IGN-MOPT",  # nuevo
                              cod_subpartida == "13110902" & Año < 2021 ~ "Venta de bienes -FONAM- Ley 7554",
                              cod_subpartida == "13110902" & Año >= 2021 ~ "Venta de bienes SENASA", # nuevo
                              cod_subpartida == "13110903" ~ "Venta Libros, revistas y folletos Museo Nacional", # nuevo
                              cod_subpartida == "13110904" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense", # nuevo
                              cod_subpartida == "13110905" ~ "Venta de producción planteles y otros bienes Imprenta Nacional", # nuevo
                              cod_subpartida == "13110907" ~ "Venta de bienes SINAC",
                              
                              cod_subpartida == "13120101" ~ "Servicios de transporte por carretera",# nuevo 
                              cod_subpartida == "13120104" ~ "Servicio de transporte aeroportuario",# nuevo
                              
                              cod_subpartida == "13120301" ~ "Servicios financieros",
                              cod_subpartida == "13120304" ~ "Servicios de recaudación",
                              cod_subpartida == "13120309" ~ "Otros servicios financieros y de seguros", # nuevo
                              
                              
                              cod_subpartida == "13120401" ~ "Alquiler de edificos e instalaciones",
                              cod_subpartida == "13120409" ~ "Otros alquileres",
                              
                              
                              cod_subpartida == "13120901" ~ "Servicios de formación y capacitación",
                              cod_subpartida == "13120902" ~ "Servicios de investigación y desarrollo",
                              
                              cod_subpartida == "13120904" ~ "Servicios culturales y recreativos",
                              
                              cod_subpartida == "13120906" ~ "Servicios de publicidad e impresión",
                              cod_subpartida == "13120909" ~ "Venta de otros servicios",
                              
                              cod_subpartida == "13130101" ~ "Derechos administrativos a los servicios de transporte por carretera",
                              cod_subpartida == "13130103" ~ "Derechos administrativos a los servicios de transporte portuario",
                              cod_subpartida == "13130104" ~ "Derechos administrativos a los servicios de transporte aeroportuario",
                              
                              
                              
                              cod_subpartida == "13130201" ~ "Cánones por regulación de los servicios públicos",
                              cod_subpartida == "13130202" ~ "Derechos administrativos a los servicios de educación", # nuevo
                              
                              
                              cod_subpartida == "13130203" ~ "Derechos administrativos a actividades comerciales",
                              cod_subpartida == "13130209" ~ "Otros derechos administrativos a otros servicios públicos",
                              
                              
                              cod_subpartida == "13220101" ~ "Concesión de obra pública CTAC",# nuevo
                              cod_subpartida == "13220201" ~ "Alquiler de terrenos CTAC",# nuevo
                              cod_subpartida == "13220202" ~ "Alquiler de Terrenos SINAC",# nuevo
                              cod_subpartida == "13220901" ~ "Concesión Soda Comedor Registro Nacional",# nuevo
                              cod_subpartida == "13220902" ~ "Otros Ingresos de la Renta de la Propiedad SINAC",# nuevo
                              
                              
                              cod_subpartida == "13230101" ~ "Intereses p otros equivalentes efect sect púb int", #NS
                              cod_subpartida == "13230106" ~ "Intereses sobre títulos valores de Instituciones Públicas Financieras",
                              
                              cod_subpartida == "13230205" ~ "Intereses sobre préstamos a empresas públicas no financieras",
                              cod_subpartida == "13230206" ~ "Intereses sobre préstamos a instituciones públicas financieras",
                              
                              cod_subpartida == "13230301" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                              
                              cod_subpartida == "13230304" ~ "Diferencias por tipo de cambio", # nuevo
                              
                              cod_subpartida == "13310201" ~ "Impuestos Internos", 
                              cod_subpartida == "13310202" ~ "Impuestos Aduanas", 
                              
                              cod_subpartida == "13310401" ~ "Sanciones Administrativas y Otros Ley No. 7092",
                              cod_subpartida == "13310402" ~ "Ejecución de garantías de cumplimiento y participación",
                              cod_subpartida == "13310403" ~ "Sanciones administrativas y judiciales FODESAF",
                              
                              cod_subpartida == "13310901" ~ "Otras multas", #NS
                              cod_subpartida == "13310902" ~ "1% Ley Protección al Consumidor",
                              cod_subpartida == "13310903" ~ "Multas por Incumplimiento",
                              cod_subpartida == "13310904" ~ "Infracciones Ley Lab",
                              cod_subpartida == "13310905" ~ "Multas Ley 8246 N° Código de Miniería",
                              cod_subpartida == "13310906" ~ "Multas artículo 36 Ley N° 9028 productos del tabaco",
                              cod_subpartida == "13310907" ~ "Multas por incumplimiento restricción sanitaria COVID-19",
                              cod_subpartida == "13310908" ~ "Multas Código Penal Ley 5386 Patronato de Construc ciones", #nuevo
                              cod_subpartida == "13310909" ~ "Multas Varias",
                              
                              cod_subpartida == "13320101" ~ "Remates",
                              cod_subpartida == "14110100" ~ "Fideicomiso No. 955 Ministerio de Hacienda _ Banco Nacional de Costa Rica",
                              cod_subpartida == "14120101" ~ "Fodesaf-Comedores-Ministerio Educación Pública Ley N° 8783 Artículo 3 inciso e", #NS
                              cod_subpartida == "14120102" ~ "Fodesaf-Ministerio de Obras Públicas y Transportes", #NS
                              cod_subpartida == "14120103" ~ "Fodesaf-Pronae-Ministerio de Trabajo", #NS
                              cod_subpartida == "14120104" ~ "Fodesaf-Pronamype", #NS
                              cod_subpartida == "14120105" ~ "Fodesaf-Régimen no Contribuitivo de Pensiones-CCSS Ley No. 8783", #NS
                              cod_subpartida == "14120106" ~ "Fodesaf-MEP-Comedores Escolares Juntas Ley No. 8783", #NS
                              cod_subpartida == "14120107" ~ "Fodesaf-MEP-Programa Avancemos-Ley No. 8783", #NS
                              cod_subpartida == "14120108" ~ "Fodesaf-IMAS-Jefas de Hogar Ley No. 8783", #NS
                              cod_subpartida == "14120109" ~ "Fodesaf-Desaf- Ley No. 8783", #NS
                              cod_subpartida == "14120110" ~ "Fodesaf Ley 8783", #NS
                              cod_subpartida == "14120111" ~ "Fodesaf-CONAI-L8783", #NS
                              cod_subpartida == "14120112" ~ "Fodesaf-IMAS-L 8783", #NS
                              cod_subpartida == "14120113" ~ "Fodesaf-BANHV-FOSUVI", #NS
                              cod_subpartida == "14120114" ~ "Fodesaf-BANHV-FOSUVI", #NS
                              cod_subpartida == "14120115" ~ "Fodesaf-Mi primer Empleo", #NS
                              cod_subpartida == "14120120" ~ "Fodesaf-Ministerio Obras Públicas y Transporte (previo 2009)", #NS
                              cod_subpartida == "14120130" ~ "Fodesaf-PRONAE (previo 2009)", #NS
                              
                              cod_subpartida == "14130101" ~ "CONICIT",
                              cod_subpartida == "14130102" ~ "Junta Administrativa Colegio San Luis Gonzaga",
                              cod_subpartida == "14130103" ~ "ICT",
                              cod_subpartida == "14130104" ~ "INEC",
                              cod_subpartida == "14130105" ~ "INAMU",
                              cod_subpartida == "14130106" ~ "PANI",
                              cod_subpartida == "14130107" ~ "INDER",
                              cod_subpartida == "14130108" ~ "IMAS",
                              cod_subpartida == "14130109" ~ "SENARA",
                              cod_subpartida == "14130110" ~ "ARESEP",
                              cod_subpartida == "14130111" ~ "INCOPESCA",
                              cod_subpartida == "14130112" ~ "Instituto Nacional de Aprendizaje",
                              cod_subpartida == "14130113" ~ "IFAM",
                              
                              
                              cod_subpartida == "14150101" ~ "AyA",
                              cod_subpartida == "14150102" ~ "ICE",
                              cod_subpartida == "14150103" ~ "INCOOP",
                              cod_subpartida == "14150104" ~ "JAPDEVA",
                              cod_subpartida == "14150105" ~ "CNP",
                              cod_subpartida == "14150106" ~ "INCOFER",
                              
                              cod_subpartida == "14160101" ~ "Banco de Costa Rica",
                              cod_subpartida == "14160102" ~ "Bancrédito", # NS
                              cod_subpartida == "14160103" ~ "Banco Nacional de Costa Rica",
                              cod_subpartida == "14160104" ~ "INVU",
                              cod_subpartida == "14160105" ~ "INS", # NS
                              cod_subpartida == "14160106" ~ "INFOCOOP",
                              cod_subpartida == "14160202" ~ "Transferencias de aseguradoras públicas para Financ. INEC Ley 9694", # NS
                              
                              # TODOS LOS SIGUIENTES SON NUEVOS #
                              
                              
                              cod_subpartida == "32110104" ~ "-",
                              cod_subpartida == "32110105" ~ "-",
                              cod_subpartida == "32110215" ~ "-",
                              cod_subpartida == "32110218" ~ "-",
                              cod_subpartida == "32110219" ~ "-",
                              
                              cod_subpartida == "32110221" ~ "-",
                              cod_subpartida == "32110222" ~ "-",
                              cod_subpartida == "32110223" ~ "-",
                              cod_subpartida == "32110224" ~ "-",
                              cod_subpartida == "32110226" ~ "-",
                              cod_subpartida == "32110227" ~ "-",
                              cod_subpartida == "32110302" ~ "-",
                              cod_subpartida == "32110307" ~ "-",
                              cod_subpartida == "32110901" ~ "-",
                              cod_subpartida == "32110902" ~ "-",
                              
                              # Nuevas asignaciones según la nomenclatura del PI
                              
                              ####### Remplazar por "-" #######
                              
                              cod_subpartida == "11120104"  ~ "",
                              
                              cod_subpartida == "11130101"  ~ "-",
                              cod_subpartida == "11130102"  ~ "-",
                              
                              cod_subpartida == "11130201"  ~ "",
                              cod_subpartida == "11130202"  ~ "",
                              cod_subpartida == "11150101"  ~ "",
                              
                              cod_subpartida == "11320109"  ~ "",
                              cod_subpartida == "12130401"  ~ "",
                              
                              cod_subpartida == "24160201"  ~ "",
                              cod_subpartida == "32110214"  ~ "",
                              cod_subpartida == "32110216"  ~ "",
                              cod_subpartida == "32110217"  ~ "",
                              cod_subpartida == "32110225"  ~ "Crédito BID N° 4819/OC-CR Programa de Apoyo a la Sostenibilidad Fiscal Ley N° 9754",
                              cod_subpartida == "32110304"  ~ "",
                              cod_subpartida == "32110305"  ~ "",
                              
                              ####### Otros  Remplazar por "-"  ############
                              
                              cod_subpartida == "11120101" ~ "",
                              cod_subpartida == "11120201" ~ "",
                              cod_subpartida == "11110301" ~ "",
                              
                              cod_subpartida == "11110101" ~ "",
                              cod_subpartida == "11110201" ~ "",
                              cod_subpartida == "11120105" ~ "",
                              
                              cod_subpartida == "11120105" ~ "",
                              
                              cod_subpartida == "11120105" ~ "",
                              
                              cod_subpartida == "11410202" ~ "",
                              
                              cod_subpartida == "12130101" ~ "",
                              cod_subpartida == "12130102" ~ "",
                              cod_subpartida == "12130201" ~ "",
                              cod_subpartida == "12130201" ~ "",
                              cod_subpartida == "12130301" ~ "",
                              cod_subpartida == "12130302" ~ "",
                              
                              cod_subpartida == "13110101" ~ "",
                              cod_subpartida == "13110903" ~ "",
                              cod_subpartida == "13110904" ~ "",
                              cod_subpartida == "13110905" ~ "",
                              cod_subpartida == "13120101" ~ "",
                              cod_subpartida == "13120104" ~ "",
                              cod_subpartida == "13120309" ~ "",
                              cod_subpartida == "13120409" ~ "",
                              cod_subpartida == "13120409" ~ "",
                              cod_subpartida == "13120409" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120904" ~ "",
                              cod_subpartida == "13120906" ~ "",
                              cod_subpartida == "13130104" ~ "",
                              cod_subpartida == "13130202" ~ "",
                              cod_subpartida == "13130202" ~ "",
                              
                              cod_subpartida == "13220101" ~ "",
                              cod_subpartida == "13220201" ~ "Alquiler de terrenos CTAC",
                              cod_subpartida == "13220202" ~ "",
                              cod_subpartida == "13220901" ~ "",
                              cod_subpartida == "13220902" ~ "",
                              cod_subpartida == "13230304" ~ "",
                              
                              cod_subpartida == "13310908" ~ "",
                              cod_subpartida == "13310910" ~ "Multas Sanciones Remanentes Confiscaciones Teatro Nacional",
                              
                              cod_subpartida == "32110104" ~ "",
                              cod_subpartida == "32110105" ~ "",
                              cod_subpartida == "32110105" ~ "",
                              cod_subpartida == "32110105" ~ "",
                              cod_subpartida == "32110215" ~ "",
                              cod_subpartida == "32110218" ~ "",
                              cod_subpartida == "32110219" ~ "",
                              cod_subpartida == "32110220" ~ "",
                              cod_subpartida == "32110221" ~ "",
                              cod_subpartida == "32110222" ~ "",
                              cod_subpartida == "32110223" ~ "",
                              cod_subpartida == "32110224" ~ "",
                              cod_subpartida == "32110226" ~ "",
                              cod_subpartida == "32110227" ~ "",
                              cod_subpartida == "32110228" ~ "",
                              cod_subpartida == "32110302" ~ "",
                              cod_subpartida == "32110307" ~ "",
                              
                              cod_subpartida == "32110901" ~ "",
                              cod_subpartida == "32110902" ~ "",
                              cod_subpartida == "32110902" ~ "",
                              cod_subpartida == "32110902" ~ "",
                              
                              # Nuevos 
                              
                              cod_subpartida == "32110308"  ~ "",
                              cod_subpartida == "32110309" ~ "",
                              
                              ####### Nombre en la jerarquía, poner un "-" #######
                              
                              cod_subpartida == "32120010"  ~ "-",
                              cod_subpartida == "32120020"  ~ "-",
                              cod_subpartida == "32120300"  ~ "-",
                              cod_subpartida == "31310100"  ~ "-",
                              cod_subpartida == "31310200"  ~ "-",
                              cod_subpartida == "33210100"  ~ "-",
                              cod_subpartida == "33110100"  ~ "-",
                              cod_subpartida == "12190500"  ~ "-",
                              cod_subpartida == "12190400"  ~ "-",
                              cod_subpartida == "12190300"  ~ "-",
                              cod_subpartida == "12190600"  ~ "-",
                              cod_subpartida == "12190200"  ~ "-",
                              cod_subpartida == "12190700"  ~ "-",
                              cod_subpartida == "13210300"  ~ "-",
                              cod_subpartida == "13410000"  ~ "-",
                              cod_subpartida == "13490100"  ~ "-",
                              cod_subpartida == "13310100"  ~ "-",
                              cod_subpartida == "13310200"  ~ "Multas de tránsito. Ley No. 9078 COSEVI",
                              cod_subpartida == "13990000"  ~ "-",
                              cod_subpartida == "13990100"  ~ "-",
                              cod_subpartida == "13990200"  ~ "-",
                              cod_subpartida == "13990300"  ~ "-",
                              cod_subpartida == "13990400"  ~ "-",
                              cod_subpartida == "13910000"  ~ "-",
                              cod_subpartida == "11140100"  ~ "-",
                              cod_subpartida == "11120200"  ~ "-",
                              cod_subpartida == "11210100"  ~ "-",
                              cod_subpartida == "11220100"  ~ "-",
                              cod_subpartida == "11220200"  ~ "-",
                              cod_subpartida == "11230300"  ~ "-",
                              cod_subpartida == "11230100"  ~ "-",
                              cod_subpartida == "11240100"  ~ "-",
                              cod_subpartida == "11250100"  ~ "-",
                              cod_subpartida == "11910500"  ~ "-",
                              cod_subpartida == "11910800"  ~ "-",
                              cod_subpartida == "11910700"  ~ "-",
                              cod_subpartida == "11911200"  ~ "-",
                              cod_subpartida == "11910200"  ~ "-",
                              cod_subpartida == "11910900"  ~ "-",
                              cod_subpartida == "11911000"  ~ "-",
                              cod_subpartida == "11910100"  ~ "-",
                              cod_subpartida == "11910600"  ~ "-",
                              cod_subpartida == "11910400"  ~ "-",
                              cod_subpartida == "11911300"  ~ "-",
                              cod_subpartida == "11911100"  ~ "-",
                              cod_subpartida == "11910300"  ~ "-",
                              cod_subpartida == "11990100"  ~ "-",
                              cod_subpartida == "14310400"  ~ "-",
                              cod_subpartida == "14250000"  ~ "-",
                              cod_subpartida == "14260000"  ~ "-",
                              cod_subpartida == "14150800"  ~ "-",
                              cod_subpartida == "14150700"  ~ "-",
                              cod_subpartida == "14150600"  ~ "-",
                              cod_subpartida == "14150500"  ~ "-",
                              cod_subpartida == "14140600"  ~ "-",
                              cod_subpartida == "14140500"  ~ "-",
                              cod_subpartida == "14140400"  ~ "-",
                              cod_subpartida == "14140100"  ~ "-",
                              cod_subpartida == "14130200"  ~ "-",
                              cod_subpartida == "14130600"  ~ "-",
                              cod_subpartida == "14130400"  ~ "-",
                              cod_subpartida == "14130300"  ~ "-",
                              cod_subpartida == "14130900"  ~ "-",
                              cod_subpartida == "14131100"  ~ "-",
                              cod_subpartida == "14131300"  ~ "-",
                              cod_subpartida == "14131200"  ~ "-",
                              cod_subpartida == "14131000"  ~ "-",
                              cod_subpartida == "14131400"  ~ "-",
                              cod_subpartida == "14151000"  ~ "-",
                              cod_subpartida == "14160400"  ~ "-",
                              cod_subpartida == "14121400"  ~ "-",
                              cod_subpartida == "14121500"  ~ "-",
                              cod_subpartida == "14110200"  ~ "-",
                              cod_subpartida == "24130100"  ~ "-"
                              
                            ),            
                            
                            cod_renglon = substr(Ingresos_a$`PosPre`, 2, 10),
                            
                            renglon = case_when(
                              
                              #######  Asignaciones  ############
                              
                              
                              cod_renglon == "113101011" ~ "Impuesto de Ventas Ley N° 7543", # NS
                              
                              cod_renglon == "113101021" ~ "Impuesto de ventas bienes y servicios importados",
                              
                              
                              cod_renglon == "113102011" ~ "Impuesto selectivo de consumo de bienes internos",
                              
                              cod_renglon == "113102021" ~ "Impuesto selectivo de consumo de bienes importados",
                              
                              cod_renglon == "113201021" ~ "Licencia de caza y pesca continental",
                              cod_renglon == "113201022" ~ "Licencia de caza y pesca MIRENEM",
                              
                              cod_renglon == "113201031" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14",
                              
                              
                              cod_renglon == "113201041" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972",
                              cod_renglon == "113201042" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114",
                              cod_renglon == "113201043" ~ "Impuestos específicos sobre los jabones de tocador . Ley Nº 8114",
                              cod_renglon == "113201044" ~ "Impuesto al cemento",
                              cod_renglon == "113201045" ~ "Impuesto a los productos de tabaco", # Nuevo nombre
                              
                              cod_renglon == "113201091" ~ "Impuestos Ley No.6883 y Ley No. 8495 SENASA",
                              cod_renglon == "113202021" ~ "Permiso de Transporte carga liviana COSEVI",
                              cod_renglon == "113202031" ~ "Impuesto sobre casinos Ley N° 9050",
                              cod_renglon == "113202032" ~ "Impuesto sobre empresas de apuestas electrónicas Ley N° 9050",
                              
                              cod_renglon == "113202033" ~ "Impuestos espectáculos públicos leyes 3632 y 5780 Teatro Nacional",
                              
                              cod_renglon == "114309011" ~ "Fondo Social Migratorio L8764",
                              cod_renglon == "114309012" ~ "Fondo Especial de Migración",
                              cod_renglon == "114309013" ~ "Otros Impuestos Migratorios",
                              
                              cod_renglon == "131201011" ~ "Acarreo de vehículos detenidos COSEVI",
                              
                              cod_renglon == "131204011" ~ "Alquiler de edificios",
                              cod_renglon == "131209011" ~ "Servicios capacitación CGR",
                              cod_renglon == "131209021" ~ "Serv Metrolog MEIC",
                              cod_renglon == "131209022" ~ "Serv Amb SETENA",
                              cod_renglon == "131209091" ~ "Costo Doc Migrator.",
                              cod_renglon == "131209092" ~ "Venta Ser. IGN-MOPT",
                              
                              cod_renglon == "131301011" ~ "Licencia de Conducir Ley N° 7055",
                              cod_renglon == "131301012" ~ "Canon CTP Ley N° 7969",
                              cod_renglon == "131301031" ~ "Rev Barcos Cap Puert",
                              cod_renglon == "131301032" ~ "Der. Registro Naval",
                              cod_renglon == "131301033" ~ "Der Zarpe Embar Ext.",
                              cod_renglon == "131301034" ~ "Canon Cert Navegab.",
                              
                              cod_renglon == "131302031" ~ "Radio y TV Ley N° 1758",
                              cod_renglon == "131302032" ~ "Frecuencias de Radio Ley N° 1758",
                              cod_renglon == "131302034" ~ "Conc.explo.min L8246",
                              cod_renglon == "131302035" ~ "Derechos de Inscripción de Gestión de Residuos Ley 8839",
                              cod_renglon == "131302091" ~ "PERMISOS Y RENOVACION DE CEDULAS DE RESIDENCIA. (LEYES N? 7033 Y 7108 DE 13-8-86 Y 8-11-88).",
                              
                              cod_renglon == "132301061" ~ "Intereses bonos INVU",
                              cod_renglon == "132302051" ~ "Deuda renegociada AyA",
                              cod_renglon == "132302061" ~ "Deuda renegociada BCCR",
                              
                              # Siguientes son nuevos
                              
                              cod_renglon == "131201041" ~ "Servicio de transporte aeroportuario CTAC",
                              
                              cod_renglon == "131203091" ~ "Otros servicios financieros y de seguros COSEVI",
                              cod_renglon == "131204011" ~ "Alquiler de edificios",
                              cod_renglon == "131204012" ~ "Alquiler de edificios CTAC",
                              cod_renglon == "131204013" ~ "Alquiler de edificios Registro Nacional",
                              cod_renglon == "131204014" ~ "Alquiler de edificios Casa Cultura Puntarenas",
                              cod_renglon == "131204015" ~ "Alquiler de edificios Centro Nacional de la Música",
                              cod_renglon == "131204016" ~ "Alquiler de edificios Teatro Nacional",
                              cod_renglon == "131204017" ~ "Alquiler de edificios Teatro Melico Salazar",
                              cod_renglon == "131204091" ~ "Custodia de Vehículos COSEVI",
                              cod_renglon == "131204092" ~ "Apartados Registro Nacional",
                              cod_renglon == "131204093" ~ "Otros Alquileres Teatro Melico Salazar",
                              cod_renglon == "131209012" ~ "Venta Servicios Capacitación Archivo Nacional",
                              cod_renglon == "131209021" ~ "Venta de servicios metrológicos (MEIC) Ley Nº 8279",
                              cod_renglon == "131209022" ~ "Serv Amb SETENA",
                              cod_renglon == "131209024" ~ "Servicios de Análisis Laboratorio INTA",
                              cod_renglon == "131209025" ~ "Venta de servicios ambientales FONAFIFO",
                              cod_renglon == "131209041" ~ "Derechos de entrada a parques",
                              cod_renglon == "131209042" ~ "Servicios culturales y recreativos Centro Nacional de la Música",
                              cod_renglon == "131209043" ~ "Servicios culturales y recreativos Museo Nacional",
                              cod_renglon == "131209044" ~ "Venta de entradas y otros Teatro Nacional",
                              cod_renglon == "131209045" ~ "Venta de entradas y otros Teatro Melico Salazar",
                              cod_renglon == "131209046" ~ "Entradas a las Áreas Silvestres Protegidas SINAC",
                              cod_renglon == "131209061" ~ "Servicios de publicidad e impresión",
                              cod_renglon == "131209062" ~ "Servicios de publicidad e impresión Imprenta Nacional",
                              cod_renglon == "131209093" ~ "Honorarios por Servicios de Defensa Civil de la Víctima",
                              cod_renglon == "131209094" ~ "Venta de Servicio de Consulta de datos del TSE",
                              
                              # OJO A
                              
                              cod_renglon == "131209098" ~ "Venta de servicios varios",
                              cod_renglon == "131209099" ~ "Venta de servicios varios",
                              cod_renglon == "131301012" ~ "Canon Consejo de Transporte Público. Ley Nº 7969",
                              cod_renglon == "131301013" ~ "Derechos administrativos a los servicios de transp orte en carretera CNC- Concesión carretera San Jos é-Caldera",
                              cod_renglon == "131301014" ~ "Derechos administrativos a los servicios de transp orte en carretera CONAVI- Peajes carretera Florenc io Castillo",
                              cod_renglon == "131301015" ~ "Derechos administrativos a los servicios de transp orte en carretera CONAVI- Peajes carretera Braulio Carrillo",
                              cod_renglon == "131301031" ~ "Revisión de barcos por Capitanías de Puerto. Resol . Nº 40 del 13-6-52",
                              cod_renglon == "131301032" ~ "Derechos de inscripción en el Registro Naval. Art. 33 Ley Nº 8000",
                              cod_renglon == "131301033" ~ "Derechos de zarpe de embarcaciones extranjeras. Ar t.32 Ley Nº 8000",
                              cod_renglon == "131301034" ~ "Canon por certificado de navegabilidad. Art.31 Ley Nº 8000",
                              cod_renglon == "131301035" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                              cod_renglon == "131301041" ~ "Derechos administrativos a los servicios de transp orte aeroportuario CTAC",
                              cod_renglon == "131302013" ~ "Canon por aprovechamiento de aguas",
                              cod_renglon == "131302014" ~ "Canon ambiental por vertidos",
                              cod_renglon == "131302021" ~ "Matrícula Exámenes Teóricos y Pruebas Prácticas COSEVI",
                              cod_renglon == "131302022" ~ "Matrícula estudiantes Centro Nacional de la Música",
                              cod_renglon == "131302031" ~ "Radio y Televisión Ley 1758",
                              cod_renglon == "131302032" ~ "Frecuencias Radio Ley 1758",
                              cod_renglon == "131302034" ~ "Concesión de Explotación minera ley N°8246",
                              cod_renglon == "131302035" ~ "Derechos de Inscripción de Gestión de Residuos",
                              cod_renglon == "131302036" ~ "Cánones y Multas y Sanciones Administrativas PROHA B",
                              cod_renglon == "131302037" ~ "Derechos Administrativos a actividades comerciales SINAC",
                              cod_renglon == "131302092" ~ "Emisión de Licencias de Conducir COSEVI",
                              cod_renglon == "131302093" ~ "Registro e inscripción CANNON-CONIS",
                              cod_renglon == "131302094" ~ "Derecho de Filmación SINAC",
                              
                              
                              cod_renglon == "131109043" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                              
                              cod_renglon == "113201091" ~ "",
                              cod_renglon == "113202021" ~ "",
                              cod_renglon == "113202033" ~ "",
                              
                              cod_renglon == "131109043" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                              
                              cod_renglon == "131201011" ~ "Acarreo de vehículos detenidos COSEVI",
                              cod_renglon == "131201041" ~ "",
                              cod_renglon == "131203091" ~ "",
                              cod_renglon == "131204012" ~ "",
                              cod_renglon == "131204013" ~ "",
                              cod_renglon == "131204014" ~ "",
                              cod_renglon == "131204015" ~ "",
                              cod_renglon == "131204016" ~ "",
                              cod_renglon == "131204017" ~ "",
                              cod_renglon == "131204091" ~ "",
                              cod_renglon == "131204092" ~ "",
                              cod_renglon == "131204093" ~ "",
                              cod_renglon == "131209012" ~ "",
                              cod_renglon == "131209023" ~ "Ingresos por inspección",
                              cod_renglon == "131209024" ~ "",
                              cod_renglon == "131209025" ~ "",
                              cod_renglon == "131209041" ~ "",
                              cod_renglon == "131209042" ~ "",
                              cod_renglon == "131209043" ~ "",
                              cod_renglon == "131209044" ~ "",
                              cod_renglon == "131209045" ~ "",
                              cod_renglon == "131209046" ~ "",
                              cod_renglon == "131209061" ~ "",
                              cod_renglon == "131209062" ~ "",
                              cod_renglon == "131209093" ~ "",
                              cod_renglon == "131209094" ~ "",
                              cod_renglon == "131209098" ~ "Venta de servicios varios",
                              
                              cod_renglon == "131209099" ~ "Venta de servicios varios",
                              cod_renglon == "131301013" ~ "",
                              cod_renglon == "131301014" ~ "",
                              cod_renglon == "131301015" ~ "",
                              cod_renglon == "131301035" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                              cod_renglon == "131301041" ~ "",
                              cod_renglon == "131302013" ~ "",
                              cod_renglon == "131302014" ~ "",
                              cod_renglon == "131302021" ~ "",
                              cod_renglon == "131302022" ~ "",
                              cod_renglon == "131302036" ~ "",
                              cod_renglon == "131302037" ~ "",
                              cod_renglon == "131302092" ~ "",
                              cod_renglon == "131302093" ~ "",
                              cod_renglon == "131302094" ~ "",
                              cod_renglon == "133109010" ~ "Multas Sugeval",
                              cod_renglon == "133109011" ~ "Otras multas",
                              cod_renglon == "133109012" ~ "Multas por incumplimiento (2007)",
                              cod_renglon == "133109013" ~ "Multas de Aduanas (2007)",
                              cod_renglon == "133109014" ~ "1% SOBRE UTILIDADES PROTECCION AL CONSUMIDOR (2007)",
                              cod_renglon == "133109015" ~ "MULTA SEGUN ART. 158 INCISO 3 DE LA LEY REGULADORA DEL MERCADO DE VALORES (200 SALARIOS BASES)",
                              
                              ####### Nombre en la jerarquía, poner un "-" #######
                              
                              cod_renglon == "321200200" ~ "-",
                              cod_renglon == "321200100" ~ "-",
                              cod_renglon == "321203000" ~ "-",
                              cod_renglon == "321101050" ~ "-",
                              cod_renglon == "321101040" ~ "-",
                              cod_renglon == "321102220" ~ "-",
                              cod_renglon == "321102240" ~ "-",
                              cod_renglon == "321102210" ~ "-",
                              cod_renglon == "321102230" ~ "-",
                              cod_renglon == "321102270" ~ "-",
                              cod_renglon == "321102260" ~ "-",
                              cod_renglon == "321102150" ~ "-",
                              cod_renglon == "321102180" ~ "-",
                              cod_renglon == "321102190" ~ "-",
                              cod_renglon == "321102200" ~ "-",
                              cod_renglon == "321103020" ~ "-",
                              cod_renglon == "321103070" ~ "-",
                              cod_renglon == "321103080" ~ "-",
                              cod_renglon == "321109020" ~ "-",
                              cod_renglon == "321109010" ~ "-",
                              cod_renglon == "313101000" ~ "-",
                              cod_renglon == "313102000" ~ "-",
                              cod_renglon == "332101000" ~ "-",
                              cod_renglon == "331101000" ~ "-",
                              cod_renglon == "121303020" ~ "-",
                              cod_renglon == "121303010" ~ "-",
                              cod_renglon == "121301010" ~ "-",
                              cod_renglon == "121301020" ~ "-",
                              cod_renglon == "121302010" ~ "-",
                              cod_renglon == "121905000" ~ "-",
                              cod_renglon == "121904000" ~ "-",
                              cod_renglon == "121903000" ~ "-",
                              cod_renglon == "121906000" ~ "-",
                              cod_renglon == "121902000" ~ "-",
                              cod_renglon == "121907000" ~ "-",
                              cod_renglon == "132301060" & Año < 2021~ "-",
                              cod_renglon == "132301060" & Año >= 2021 ~ "Intereses por títulos y valores de Inst Púb Financ ICD",
                              cod_renglon == "132201010" ~ "-",
                              cod_renglon == "132209010" ~ "-",
                              
                              cod_renglon == "132303040" ~ "Diferencias por tipo de cambio",
                              cod_renglon == "132303010" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                              cod_renglon == "132202010" ~ "-",
                              cod_renglon == "132202020" ~ "-",
                              cod_renglon == "132209020" ~ "-",
                              cod_renglon == "132103000" ~ "-",
                              cod_renglon == "134100000" ~ "-",
                              cod_renglon == "134901000" ~ "-",
                              cod_renglon == "133101000" ~ "-",
                              cod_renglon == "133102000" ~ "-",
                              cod_renglon == "133102020" ~ "-",
                              cod_renglon == "133102010" ~ "-",
                              cod_renglon == "133109020" ~ "-",
                              cod_renglon == "133109040" ~ "-",
                              cod_renglon == "133109080" ~ "-",
                              cod_renglon == "133109050" ~ "-",
                              cod_renglon == "133109030" ~ "-",
                              cod_renglon == "133109070" ~ "-",
                              cod_renglon == "133109090" ~ "-",
                              cod_renglon == "133109100" ~ "-",
                              cod_renglon == "133104020" ~ "-",
                              cod_renglon == "133104010" ~ "-",
                              cod_renglon == "133201010" ~ "-",
                              cod_renglon == "139900000" ~ "-",
                              cod_renglon == "139901000" ~ "-",
                              cod_renglon == "139902000" ~ "-",
                              cod_renglon == "139903000" ~ "-",
                              cod_renglon == "139904000" ~ "-",
                              cod_renglon == "139100000" ~ "-",
                              cod_renglon == "131101010" ~ "-",
                              cod_renglon == "131109020" ~ "-",
                              cod_renglon == "131109050" ~ "-",
                              cod_renglon == "131109030" ~ "-",
                              cod_renglon == "131203040" ~ "Servicios de recaudación",
                              cod_renglon == "131203010" ~ "Servicios financieros",
                              cod_renglon == "111401000" ~ "-",
                              cod_renglon == "111102010" ~ "-",
                              cod_renglon == "111101010" ~ "-",
                              cod_renglon == "111103040" ~ "-",
                              cod_renglon == "111103010" ~ "-",
                              cod_renglon == "111103020" ~ "-",
                              cod_renglon == "111103030" ~ "-",
                              cod_renglon == "111202000" ~ "-",
                              cod_renglon == "111202040" ~ "-",
                              cod_renglon == "111202010" ~ "-",
                              cod_renglon == "111202020" ~ "-",
                              cod_renglon == "111202030" ~ "-",
                              cod_renglon == "111201050" ~ "-",
                              cod_renglon == "111201010" ~ "-",
                              cod_renglon == "111201020" ~ "-",
                              cod_renglon == "111201030" ~ "-",
                              cod_renglon == "113101020" ~ "-",
                              cod_renglon == "113101010" ~ "-",
                              cod_renglon == "113102020" ~ "-",
                              cod_renglon == "113102010" ~ "-",
                              cod_renglon == "114201010" ~ "-",
                              cod_renglon == "114209010" ~ "-",
                              cod_renglon == "114209020" ~ "-",
                              cod_renglon == "114101010" ~ "-",
                              cod_renglon == "114102010" ~ "-",
                              cod_renglon == "114102020" ~ "-",
                              cod_renglon == "114304010" ~ "-",
                              cod_renglon == "114303010" ~ "-",
                              cod_renglon == "114303020" ~ "-",
                              cod_renglon == "114309020" ~ "-",
                              cod_renglon == "112101000" ~ "-",
                              cod_renglon == "112201000" ~ "-",
                              cod_renglon == "112202000" ~ "-",
                              cod_renglon == "112303000" ~ "-",
                              cod_renglon == "112301000" ~ "-",
                              cod_renglon == "112401000" ~ "-",
                              cod_renglon == "112501000" ~ "-",
                              cod_renglon == "119105000" ~ "-",
                              cod_renglon == "119108000" ~ "-",
                              cod_renglon == "119107000" ~ "-",
                              cod_renglon == "119112000" ~ "-",
                              cod_renglon == "119102000" ~ "-",
                              cod_renglon == "119109000" ~ "-",
                              cod_renglon == "119110000" ~ "-",
                              cod_renglon == "119101000" ~ "-",
                              cod_renglon == "119106000" ~ "-",
                              cod_renglon == "119104000" ~ "-",
                              cod_renglon == "119113000" ~ "-",
                              cod_renglon == "119111000" ~ "-",
                              cod_renglon == "119103000" ~ "-",
                              cod_renglon == "119901000" ~ "-",
                              cod_renglon == "143104000" ~ "-",
                              cod_renglon == "142500000" ~ "-",
                              cod_renglon == "142600000" ~ "-",
                              cod_renglon == "141510000" ~ "-",
                              cod_renglon == "141501010" ~ "-",
                              cod_renglon == "141501050" ~ "-",
                              cod_renglon == "141501020" ~ "-",
                              cod_renglon == "141501060" ~ "-",
                              cod_renglon == "141501030" ~ "-",
                              cod_renglon == "141501040" ~ "-",
                              cod_renglon == "141508000" ~ "-",
                              cod_renglon == "141507000" ~ "-",
                              cod_renglon == "141506000" ~ "-",
                              cod_renglon == "141505000" ~ "-",
                              cod_renglon == "141406000" ~ "-",
                              cod_renglon == "141405000" ~ "-",
                              cod_renglon == "141404000" ~ "-",
                              cod_renglon == "141401000" ~ "-",
                              cod_renglon == "141302000" ~ "-",
                              cod_renglon == "141301100" ~ "-",
                              cod_renglon == "141301010" ~ "-",
                              cod_renglon == "141301030" ~ "-",
                              cod_renglon == "141301070" ~ "-",
                              cod_renglon == "141301130" ~ "-",
                              cod_renglon == "141301050" ~ "-",
                              cod_renglon == "141301110" ~ "-",
                              cod_renglon == "141301040" ~ "-",
                              cod_renglon == "141301120" ~ "-",
                              cod_renglon == "141301120" ~ "-",
                              cod_renglon == "141301020" ~ "-",
                              cod_renglon == "141301060" ~ "-",
                              cod_renglon == "141301090" ~ "-",
                              cod_renglon == "141306000" ~ "-",
                              cod_renglon == "141304000" ~ "-",
                              cod_renglon == "141303000" ~ "-",
                              cod_renglon == "141309000" ~ "-",
                              cod_renglon == "141311000" ~ "-",
                              cod_renglon == "141313000" ~ "-",
                              cod_renglon == "141312000" ~ "-",
                              cod_renglon == "141310000" ~ "-",
                              cod_renglon == "141314000" ~ "-",
                              cod_renglon == "141601010" ~ "-",
                              cod_renglon == "141601030" ~ "-",
                              cod_renglon == "141601060" ~ "-",
                              cod_renglon == "141601040" ~ "-",
                              cod_renglon == "141602020" ~ "-",
                              cod_renglon == "141604000" ~ "-",
                              cod_renglon == "141214000" ~ "-",
                              cod_renglon == "141215000" ~ "-",
                              cod_renglon == "141102000" ~ "-",
                              cod_renglon == "241301000" ~ "-"
                              
                            ),
                            
                            cod_subrenglon = substr(Ingresos_a$`PosPre`, 2, 11),
                            
                            subrenglon = case_when(
                              
                              # Existentes
                              
                              cod_subrenglon == "1132010311" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14 (interno)",
                              cod_subrenglon == "1132010312" ~ "Impuesto único a los combustibles. Art.1 Ley Nº 81 14 (importaciones)",
                              cod_subrenglon == "1132010411" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972 (Interno)",
                              cod_subrenglon == "1132010412" ~ "Impuestos específicos sobre bebidas alcoholicas. L ey 7972 (Importaciones)",
                              cod_subrenglon == "1132010421" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114 (Interno)",
                              cod_subrenglon == "1132010422" ~ "Impuestos específicos sobre bebidas envasadas sin contenido alcoholico. Ley Nº 8114 (Importaciones)",
                              cod_subrenglon == "1132010431" ~ "Impuestos específicos sobre los jabones de tocador . Ley Nº 8114 (Importaciones)",
                              
                              cod_subrenglon == "1132010452" ~ "Impuesto a los productos de tabaco (Importaciones)",
                              
                              cod_subrenglon == "1312030101" ~ "Costo Transferencias SWIFT" ,
                              cod_subrenglon == "1312030102" ~ "Servicio de Recaudación de Tesorería Nacional" ,
                              cod_subrenglon == "1312030401" ~ "Servicio Recaudación Tesorería Nacional" ,
                              cod_subrenglon == "1312030402" ~ "Servicios tributarios Ley N°9355" ,
                              cod_subrenglon == "1312030403" ~ "Servicios de recaudación Teatro Nacional" ,
                              
                              cod_subrenglon == "1312090980" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)" ,
                              cod_subrenglon == "1312090981" ~ "Venta de Servicios Registrales  Registro Nacional" ,
                              cod_subrenglon == "1312090982" ~ "Venta de Servicios Direccion Nacional Notariado" ,
                              cod_subrenglon == "1312090983" ~ "Venta de Servicios SENASA" ,
                              cod_subrenglon == "1312090984" ~ "Venta de servicios varios Casa de Cultura Puntarenas" ,
                              cod_subrenglon == "1312090985" ~ "Venta de servicios varios Archivo Nacional" ,
                              cod_subrenglon == "1312090986" ~ "Venta de servicios Museo Nacional" ,
                              cod_subrenglon == "1312090987" ~ "Cursos libres Teatro Melico Salazar" ,
                              cod_subrenglon == "1312090988" ~ "Uso de instalaciones en las Áreas Silvestres Protegidas SINAC" ,
                              cod_subrenglon == "1312090990" ~ "Venta de otros servicios SFE" ,
                              cod_subrenglon == "1312090991" ~ "Venta de otros servicios INCIENSA" ,
                              
                              cod_subrenglon == "1323030101" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos" ,
                              cod_subrenglon == "1323030102" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales COSEVI" ,
                              cod_subrenglon == "1323030103" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales FODESAF" ,
                              cod_subrenglon == "1323030104" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Patronato Construcciones" ,
                              cod_subrenglon == "1323030106" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Teatro Nacional" ,
                              cod_subrenglon == "1323030401" ~ "Diferencias por tipo de cambio SINAC" ,
                              
                              # Sin un código dado el nivel
                              
                              
                              cod_subrenglon == "1323030401" ~ "Diferencias por tipo de cambio SINAC",
                              
                              cod_subrenglon == "1323030101" ~ "Intereses sobre cuentas corrientes y otros depósitos en Bancos Públicos",
                              cod_subrenglon == "1323030102" ~ "",
                              cod_subrenglon == "1323030103" ~ "",
                              cod_subrenglon == "1323030104" ~ "",
                              cod_subrenglon == "1323030106" ~ "",
                              
                              cod_subrenglon == "1312090981" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)",
                              cod_subrenglon == "1312090981" ~ "",
                              cod_subrenglon == "1312090982" ~ "",
                              cod_subrenglon == "1312090983" ~ "",
                              cod_subrenglon == "1312090984" ~ "",
                              cod_subrenglon == "1312090985" ~ "",
                              cod_subrenglon == "1312090986" ~ "",
                              cod_subrenglon == "1312090987" ~ "",
                              cod_subrenglon == "1312090988" ~ "",
                              cod_subrenglon == "1312090990" ~ "",
                              
                              cod_subrenglon == "1312030401" ~ "",
                              cod_subrenglon == "1312030402" ~ "",
                              cod_subrenglon == "1312030403" ~ "",
                              cod_subrenglon == "1312030101" ~ "",
                              cod_subrenglon == "1312030102" ~ "Servicio de Recaudación de Tesorería Nacional",
                              
                              ####### Nombre en la jerarquía, poner un "-" #######
                              
                              cod_subrenglon == "3212001000" ~ "-",
                              cod_subrenglon == "3212002000" ~ "-",
                              cod_subrenglon == "3212030000" ~ "-",
                              cod_subrenglon == "3211010500" ~ "-",
                              cod_subrenglon == "3211010400" ~ "-",
                              cod_subrenglon == "3211022200" ~ "-",
                              cod_subrenglon == "3211022400" ~ "-",
                              cod_subrenglon == "3211022100" ~ "-",
                              cod_subrenglon == "3211022300" ~ "-",
                              cod_subrenglon == "3211022700" ~ "-",
                              cod_subrenglon == "3211022600" ~ "-",
                              cod_subrenglon == "3211021500" ~ "-",
                              cod_subrenglon == "3211021800" ~ "-",
                              cod_subrenglon == "3211021900" ~ "-",
                              cod_subrenglon == "3211022000" ~ "-",
                              cod_subrenglon == "3211030200" ~ "-",
                              cod_subrenglon == "3211030700" ~ "-",
                              cod_subrenglon == "3211030800" ~ "-",
                              cod_subrenglon == "3211090200" ~ "-",
                              cod_subrenglon == "3211090100" ~ "-",
                              cod_subrenglon == "3131010000" ~ "-",
                              cod_subrenglon == "3131020000" ~ "-",
                              cod_subrenglon == "3321010000" ~ "-",
                              cod_subrenglon == "3311010000" ~ "-",
                              cod_subrenglon == "1213030200" ~ "-",
                              cod_subrenglon == "1213030100" ~ "-",
                              cod_subrenglon == "1213010100" ~ "-",
                              cod_subrenglon == "1213010200" ~ "-",
                              cod_subrenglon == "1213020100" ~ "-",
                              cod_subrenglon == "1219050000" ~ "-",
                              cod_subrenglon == "1219040000" ~ "-",
                              cod_subrenglon == "1219030000" ~ "-",
                              cod_subrenglon == "1219060000" ~ "-",
                              cod_subrenglon == "1219020000" ~ "-",
                              cod_subrenglon == "1219070000" ~ "-",
                              cod_subrenglon == "1323010600" ~ "-",
                              cod_subrenglon == "1323030401" ~ "-",
                              cod_subrenglon == "1323030100" ~ "",
                              cod_subrenglon == "1322020100" ~ "-",
                              cod_subrenglon == "1322020200" ~ "-",
                              cod_subrenglon == "1322010100" ~ "-",
                              cod_subrenglon == "1322090100" ~ "-",
                              cod_subrenglon == "1322090200" ~ "-",
                              cod_subrenglon == "1321030000" ~ "-",
                              cod_subrenglon == "1341000000" ~ "-",
                              cod_subrenglon == "1349010000" ~ "-",
                              cod_subrenglon == "1331010000" ~ "-",
                              cod_subrenglon == "1331020000" ~ "-",
                              cod_subrenglon == "1331020200" ~ "-",
                              cod_subrenglon == "1331020100" ~ "-",
                              cod_subrenglon == "1331090200" ~ "-",
                              cod_subrenglon == "1331090400" ~ "-",
                              cod_subrenglon == "1331090800" ~ "-",
                              cod_subrenglon == "1331090500" ~ "-",
                              cod_subrenglon == "1331090300" ~ "-",
                              cod_subrenglon == "1331090700" ~ "-",
                              cod_subrenglon == "1331090900" ~ "-",
                              cod_subrenglon == "1331091000" ~ "-",
                              cod_subrenglon == "1331040200" ~ "-",
                              cod_subrenglon == "1331040100" ~ "-",
                              cod_subrenglon == "1332010100" ~ "-",
                              cod_subrenglon == "1399000000" ~ "-",
                              cod_subrenglon == "1399010000" ~ "-",
                              cod_subrenglon == "1399020000" ~ "-",
                              cod_subrenglon == "1399030000" ~ "-",
                              cod_subrenglon == "1399040000" ~ "-",
                              cod_subrenglon == "1391000000" ~ "-",
                              cod_subrenglon == "1313010410" ~ "-",
                              cod_subrenglon == "1313010120" ~ "-",
                              cod_subrenglon == "1313010130" ~ "-",
                              cod_subrenglon == "1313010150" ~ "-",
                              cod_subrenglon == "1313010140" ~ "-",
                              cod_subrenglon == "1313010110" ~ "-",
                              cod_subrenglon == "1313010340" ~ "-",
                              cod_subrenglon == "1313010350" ~ "-",
                              cod_subrenglon == "1313010330" ~ "-",
                              cod_subrenglon == "1313010320" ~ "-",
                              cod_subrenglon == "1313010310" ~ "-",
                              cod_subrenglon == "1313020140" ~ "-",
                              cod_subrenglon == "1313020130" ~ "-",
                              cod_subrenglon == "1313020360" ~ "-",
                              cod_subrenglon == "1313020340" ~ "-",
                              cod_subrenglon == "1313020370" ~ "-",
                              cod_subrenglon == "1313020350" ~ "-",
                              cod_subrenglon == "1313020320" ~ "-",
                              cod_subrenglon == "1313020310" ~ "-",
                              cod_subrenglon == "1313020220" ~ "-",
                              cod_subrenglon == "1313020210" ~ "-",
                              cod_subrenglon == "1313020940" ~ "-",
                              cod_subrenglon == "1313020920" ~ "-",
                              cod_subrenglon == "1313020930" ~ "-",
                              cod_subrenglon == "1311010100" ~ "-",
                              cod_subrenglon == "1311090200" ~ "-",
                              cod_subrenglon == "1311090500" ~ "-",
                              cod_subrenglon == "1311090430" ~ "Venta libros, revistas y folletos Museo de Arte Costarricense",
                              cod_subrenglon == "1311090300" ~ "-",
                              cod_subrenglon == "1312040110" ~ "-",
                              cod_subrenglon == "1312040140" ~ "-",
                              cod_subrenglon == "1312040150" ~ "-",
                              cod_subrenglon == "1312040120" ~ "-",
                              cod_subrenglon == "1312040130" ~ "-",
                              cod_subrenglon == "1312040170" ~ "-",
                              cod_subrenglon == "1312040160" ~ "-",
                              cod_subrenglon == "1312040920" ~ "-",
                              cod_subrenglon == "1312040910" ~ "-",
                              cod_subrenglon == "1312040930" ~ "-",
                              cod_subrenglon == "1312090220" ~ "-",
                              cod_subrenglon == "1312090210" ~ "-",
                              cod_subrenglon == "1312090240" ~ "-",
                              cod_subrenglon == "1312090250" ~ "-",
                              cod_subrenglon == "1312090410" ~ "-",
                              cod_subrenglon == "1312090460" ~ "-",
                              cod_subrenglon == "1312090420" ~ "-",
                              cod_subrenglon == "1312090430" ~ "-",
                              cod_subrenglon == "1312090450" ~ "-",
                              cod_subrenglon == "1312090440" ~ "-",
                              cod_subrenglon == "1312090120" ~ "-",
                              cod_subrenglon == "1312090610" ~ "-",
                              cod_subrenglon == "1312090620" ~ "-",
                              cod_subrenglon == "1312090980" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocop ias, certificaciones, etc.)",
                              cod_subrenglon == "1312090930" ~ "-",
                              cod_subrenglon == "1312090990" ~ "-",
                              cod_subrenglon == "1312090991" ~ "-",
                              cod_subrenglon == "1312090940" ~ "-",
                              cod_subrenglon == "1312010410" ~ "-",
                              cod_subrenglon == "1312010110" ~ "-",
                              cod_subrenglon == "1312030910" ~ "-",
                              cod_subrenglon == "1312030401" ~ "-",
                              cod_subrenglon == "1312030403" ~ "-",
                              cod_subrenglon == "1312030402" ~ "-",
                              cod_subrenglon == "1114010000" ~ "-",
                              cod_subrenglon == "1111020100" ~ "-",
                              cod_subrenglon == "1111010100" ~ "-",
                              cod_subrenglon == "1111030400" ~ "-",
                              cod_subrenglon == "1111030100" ~ "-",
                              cod_subrenglon == "1111030200" ~ "-",
                              cod_subrenglon == "1111030300" ~ "-",
                              cod_subrenglon == "1112020000" ~ "-",
                              cod_subrenglon == "1112020400" ~ "-",
                              cod_subrenglon == "1112020100" ~ "-",
                              cod_subrenglon == "1112020200" ~ "-",
                              cod_subrenglon == "1112020300" ~ "-",
                              cod_subrenglon == "1112010500" ~ "-",
                              cod_subrenglon == "1112010100" ~ "-",
                              cod_subrenglon == "1112010200" ~ "-",
                              cod_subrenglon == "1112010300" ~ "-",
                              cod_subrenglon == "1131010200" ~ "-",
                              cod_subrenglon == "1131010100" ~ "-",
                              cod_subrenglon == "1131020200" ~ "-",
                              cod_subrenglon == "1131020100" ~ "-",
                              cod_subrenglon == "1132010432" ~ "Impuestos específicos sobre los jabones de tocador. Ley Nº 8114 (Importaciones)",
                              cod_subrenglon == "1132010440" ~ "-",
                              cod_subrenglon == "1132010451" ~ "Impuesto a los productos de tabaco (Interno)",
                              cod_subrenglon == "1132010910" ~ "-",
                              cod_subrenglon == "1132020310" ~ "-",
                              cod_subrenglon == "1132020320" ~ "-",
                              cod_subrenglon == "1132020330" ~ "-",
                              cod_subrenglon == "1132020210" ~ "-",
                              cod_subrenglon == "1142010100" ~ "-",
                              cod_subrenglon == "1142090100" ~ "-",
                              cod_subrenglon == "1142090200" ~ "-",
                              cod_subrenglon == "1141010100" ~ "-",
                              cod_subrenglon == "1141020100" ~ "-",
                              cod_subrenglon == "1141020200" ~ "-",
                              cod_subrenglon == "1143040100" ~ "-",
                              cod_subrenglon == "1143030100" ~ "-",
                              cod_subrenglon == "1143030200" ~ "-",
                              cod_subrenglon == "1143090200" ~ "-",
                              cod_subrenglon == "1143090120" ~ "-",
                              cod_subrenglon == "1143090110" ~ "-",
                              cod_subrenglon == "1143090130" ~ "-",
                              cod_subrenglon == "1121010000" ~ "-",
                              cod_subrenglon == "1122010000" ~ "-",
                              cod_subrenglon == "1122020000" ~ "-",
                              cod_subrenglon == "1123030000" ~ "-",
                              cod_subrenglon == "1123010000" ~ "-",
                              cod_subrenglon == "1124010000" ~ "-",
                              cod_subrenglon == "1125010000" ~ "-",
                              cod_subrenglon == "1191050000" ~ "-",
                              cod_subrenglon == "1191080000" ~ "-",
                              cod_subrenglon == "1191070000" ~ "-",
                              cod_subrenglon == "1191120000" ~ "-",
                              cod_subrenglon == "1191020000" ~ "-",
                              cod_subrenglon == "1191090000" ~ "-",
                              cod_subrenglon == "1191100000" ~ "-",
                              cod_subrenglon == "1191010000" ~ "-",
                              cod_subrenglon == "1191060000" ~ "-",
                              cod_subrenglon == "1191040000" ~ "-",
                              cod_subrenglon == "1191130000" ~ "-",
                              cod_subrenglon == "1191110000" ~ "-",
                              cod_subrenglon == "1191030000" ~ "-",
                              cod_subrenglon == "1199010000" ~ "-",
                              cod_subrenglon == "1431040000" ~ "-",
                              cod_subrenglon == "1425000000" ~ "-",
                              cod_subrenglon == "1426000000" ~ "-",
                              cod_subrenglon == "1415100000" ~ "-",
                              cod_subrenglon == "1415010100" ~ "-",
                              cod_subrenglon == "1415010500" ~ "-",
                              cod_subrenglon == "1415010200" ~ "-",
                              cod_subrenglon == "1415010600" ~ "-",
                              cod_subrenglon == "1415010300" ~ "-",
                              cod_subrenglon == "1415010400" ~ "-",
                              cod_subrenglon == "1415080000" ~ "-",
                              cod_subrenglon == "1415070000" ~ "-",
                              cod_subrenglon == "1415060000" ~ "-",
                              cod_subrenglon == "1415050000" ~ "-",
                              cod_subrenglon == "1414060000" ~ "-",
                              cod_subrenglon == "1414050000" ~ "-",
                              cod_subrenglon == "1414040000" ~ "-",
                              cod_subrenglon == "1414010000" ~ "-",
                              cod_subrenglon == "1413011000" ~ "-",
                              cod_subrenglon == "1413010100" ~ "-",
                              cod_subrenglon == "1413010300" ~ "-",
                              cod_subrenglon == "1413010700" ~ "-",
                              cod_subrenglon == "1413011300" ~ "-",
                              cod_subrenglon == "1413010500" ~ "-",
                              cod_subrenglon == "1413011100" ~ "-",
                              cod_subrenglon == "1413010400" ~ "-",
                              cod_subrenglon == "1413011200" ~ "-",
                              cod_subrenglon == "1413010200" ~ "-",
                              cod_subrenglon == "1413010600" ~ "-",
                              cod_subrenglon == "1413010900" ~ "-",
                              cod_subrenglon == "1413060000" ~ "-",
                              cod_subrenglon == "1413040000" ~ "-",
                              cod_subrenglon == "1413030000" ~ "-",
                              cod_subrenglon == "1413090000" ~ "-",
                              cod_subrenglon == "1413110000" ~ "-",
                              cod_subrenglon == "1413130000" ~ "-",
                              cod_subrenglon == "1413120000" ~ "-",
                              cod_subrenglon == "1413100000" ~ "-",
                              cod_subrenglon == "1413020000" ~ "-",
                              cod_subrenglon == "1413140000" ~ "-",
                              cod_subrenglon == "1416010100" ~ "-",
                              cod_subrenglon == "1416010300" ~ "-",
                              cod_subrenglon == "1416010600" ~ "-",
                              cod_subrenglon == "1416010400" ~ "-",
                              cod_subrenglon == "1416020200" ~ "-",
                              cod_subrenglon == "1416040000" ~ "-",
                              cod_subrenglon == "1412140000" ~ "-",
                              cod_subrenglon == "1412150000" ~ "-",
                              cod_subrenglon == "1411020000" ~ "-",
                              cod_subrenglon == "2413010000" ~ "-"
                              
                            ),
                            
                            cod_fuentefinanciacion = substr(Ingresos_a$`PosPre`, 2, 14),
                            
                            cod_fuentefinanciacion_3 = as.numeric(substr(Ingresos_a$`PosPre`, 12, 14)),
                            
                            fuentefinanciacion = case_when(
                              
                              cod_fuentefinanciacion == "1000000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1100000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1110000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111010101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111020101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030102001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1111030400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010102001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112010400001" ~ "Ingresos corrientes",
                              #       cod_fuentefinanciacion == "1112020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112020101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112020102001" ~ "Ingresos corrientes",
                              #         cod_fuentefinanciacion == "1112020200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112020300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1112020400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1113000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1113010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1113010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1113020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1113020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114010110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114010120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1114010130001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1115000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1115010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1115010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1120000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1121000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1121010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1122000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1122010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1122010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1122010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1122020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1123000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1123010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1123020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1123030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1124000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1124010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1125000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1125010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1130000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131010110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131010210001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131020110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131020200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1131020210001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010200000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010210001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010220001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010300000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010310000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010311001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010312001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010400000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010410000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010411001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010412001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010420000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010421001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010422001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010430000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010431001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010432001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010440000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010440001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010450000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010451001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132010452001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020200000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020300000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020310001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1132020320001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1140000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141010110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141010120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1141090000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1142000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1142010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1142090100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1142090200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143030000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143030100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143030200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143040000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143040100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090130001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1143090200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1190000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1191000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1191010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199020100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199020200000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1199020200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1200000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213030100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213030200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1213040100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1300000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1310000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1311000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1311090000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1311090100001" ~ "Ingresos corrientes",
                              #cod_fuentefinanciacion == "1311090200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030101001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030102001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030401001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312030402001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312040100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312040110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312040900001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090200000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090210001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090220001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090230001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090600000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090610001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090900000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090910001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090920001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090930001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1312090940001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010100000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010300000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010310001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010320001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010330001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313010340001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020130001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020140001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020300000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020310001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020320001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020340001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020350001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1313020910001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1320000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1321000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1321010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1321020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1321030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1322000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1322020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1322020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323010600000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323010600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323010610001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323020500000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323020510001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323020600000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323020610001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323030000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1323030100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1330000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331020000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331020100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331020200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331040000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331040100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331040200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090130001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090140001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090150001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090700001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1331090900001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1332000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1332010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1341000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1390000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1391000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1391010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1391020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1392000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1392000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399040000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399080000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399090000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399100000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399110000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1399120000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1400000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1410000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1411000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1411010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1411020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010700001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010800001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412010900001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412011500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412012000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412013000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412040000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412050000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412060000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412070000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412080000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412090000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412110000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412120000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412130000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1412140000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010700001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010800001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413010900001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413011500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413040000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413050000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413060000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413070000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413080000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413090000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1413100000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1414000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1414010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1414020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1414030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415010600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1415040000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010100001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010110001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010120001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010130001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010210001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010220001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010230001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010300001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010400001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010500001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416010600001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1416020200001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1420000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1421000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1421010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1423000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1424000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1425000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1426000000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1430000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1431000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1431010000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1431020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1431030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1432000000000" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1432020000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1432030000001" ~ "Ingresos corrientes",
                              cod_fuentefinanciacion == "1432040000001" ~ "Ingresos corrientes",
                              
                              cod_fuentefinanciacion == "2412010000060" ~ "Proyecto de Desarrollo Agrícola Península de Nicoya (PRODAPEN)",
                              cod_fuentefinanciacion == "2412020000060" ~ "Proyecto de Desarrollo Agrícola Península de Nicoya (PRODAPEN)",
                              cod_fuentefinanciacion == "2412030000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412050000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412060000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412070000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412080000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412090000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412100000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412110000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412120000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412130000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2412140000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2413010000060" ~ "Transferencia de capital OD",
                              cod_fuentefinanciacion == "2416020100060" ~ "Transferencia de capital OD",
                              
                              cod_fuentefinanciacion == "2431020000065" ~ "Donac. BIRF Fondo p/ Medio Ambiente Mund",
                              
                              cod_fuentefinanciacion == "2431020000065" ~ "Donac. BIRF Fondo p/ Medio Ambiente Mund",
                              cod_fuentefinanciacion == "2432010000066" ~ "Donaciones de Relaciones Exteriores Fi",
                              cod_fuentefinanciacion == "2432030000068" ~ "Cancelación operaciones cuasifiscales al BCCR. Acuerdo de Cooperación Energética de Caracas. Ley N° 8116",
                              cod_fuentefinanciacion == "2432040000070" ~ "Donación República de China",
                              cod_fuentefinanciacion == "2510000000121" ~ "Reintegro e intereses del crédito externo PL-480",
                              cod_fuentefinanciacion == "3131010000280" ~ "Títulos valores de deuda interna",
                              cod_fuentefinanciacion == "3131010000281" ~ "Títulos valores de deuda interna (Deuda Política)",
                              cod_fuentefinanciacion == "3131010000282" ~ "Títulos valores de deuda interna (Caja Única)",
                              cod_fuentefinanciacion == "3131010100280" ~ "Títulos valores de deuda interna",
                              
                              cod_fuentefinanciacion == "3131020000280" ~ "Títulos valores de deuda interna",
                              cod_fuentefinanciacion == "3131020000283" ~ "Títulos valores de deuda interna (Fideicomiso agropecuario)",
                              
                              cod_fuentefinanciacion == "3211010100450" ~ "Crédito BCIE-CR-26 FDS, Ley N° 7639 Programa de Infraestructura Universidad Nacional",
                              cod_fuentefinanciacion == "3211010200451" ~ "Crédito BCIE-CR-1129, Ley N° 7659. Proyecto de Desarrollo Agrícola Península de Nicoya",
                              cod_fuentefinanciacion == "3211010300452" ~ "Crédito BCIE-1605, Ley N° 8359 Programa para completar el complejo vial Costanera Sur",
                              cod_fuentefinanciacion == "3211010400453" ~ "Crédito BCIE 1709, Ley N° 8685 Programa de Gestión Integrada de Recursos Hídricos",
                              cod_fuentefinanciacion == "3211010500454" ~ "Crédito BCIE 2198 Programa de Alcantarillado y Con trol de Inundaciones para Limón, autorizado median te Ley No.9690 del 27 de junio del 2019",
                              cod_fuentefinanciacion == "3211010500513" ~ "Crédito BCIE-2157, Ley 9327 Proyecto de Mercado Regional Mayorista de la Región Chorotega",
                              
                              cod_fuentefinanciacion == "3211020900498" ~ "Crédito BID 1284/OC-CR, Ley N° 8154 Programa de Regularización del Catastro y Registro",
                              cod_fuentefinanciacion == "3211021000499" ~ "Crédito BID 1377/OC-CR, Ley N° 8273 Segunda Etapa del Programa de Modernización de la Administración de Justicia",
                              cod_fuentefinanciacion == "3211021100500" ~ "Crédito BID 1451/OC-CR, Ley N° 8403 Programa de Desarrollo del Sector Salud",
                              cod_fuentefinanciacion == "3211021100501" ~ "Crédito BID 667, Ley N° 7315 Proyecto Mejoramiento de la Calidad de la Educación General Básica",
                              cod_fuentefinanciacion == "3211021300502" ~ "Crédito BID 1436/OC-CR, Ley N° 8408 Programa de Fomento de la Producción Agropecuaria Sostenible",
                              cod_fuentefinanciacion == "3211021400503" ~ "Crédito BID 1556/OC-CR. Programa de Desarrollo Sostenible de la Cuenca del Río Sixaola",
                              cod_fuentefinanciacion == "3211021500504" ~ "Crédito BID 2007/-OC-CR, Ley N° 8845 Programa de Infraestructura Vial (PIVI)",
                              cod_fuentefinanciacion == "3211021600505" ~ "Crédito BID 2098/-OC-CR, Ley N° 8982 Programa Red Vial Cantonal (PIV1)",
                              cod_fuentefinanciacion == "3211021700506" ~ "Credito BID 1824/OC-CR, Ley N° 8967 Programa de turismo en Áreas Silvestres Protegidas y su Contrato Modificatorio",
                              cod_fuentefinanciacion == "3211021800507" ~ "Crédito BID 2526/OC-CR, Ley N° 9025 Programa para la Prevención de la Violencia y Promoción de la Inclusión Social",
                              cod_fuentefinanciacion == "3211021900508" ~ "Crédito BID Nº 2852/OC-CR-Programa de Innovación y Capital Humano para la Competitividad, Ley N° 9218",
                              cod_fuentefinanciacion == "3211022000509" ~ "Crédito BID N° 3071/OC-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                              cod_fuentefinanciacion == "3211022100510" ~ "Crédito BID N° 3072/CH-CR-Programa de Infraestructura de Transporte (PIT) Ley N° 9283",
                              cod_fuentefinanciacion == "3211022800521" ~ "Crédito BID N°4871/OC-CR Programa de Seguridad Ciu dadana y Prevención de Violencia Ley 9968",
                              
                              cod_fuentefinanciacion == "3211022200515" ~ "Crédito BID N°3488/OC-CR-Programa de Integración Fronteriza (PIF) Ley N°9451",
                              cod_fuentefinanciacion == "3211022300516" ~ "Crédito BID Nº4433/OC-CR Programa de Emergencias en Respuesta a la Tormenta Tropical Nate Ley N°9595",
                              cod_fuentefinanciacion == "3211022400517" ~ "Crédito BID N° 4507/OC-CR Programa Red Vial Cantonal II (PIV2) Ley N° 8982",
                              cod_fuentefinanciacion == "3211022500518" ~ "Crédito BID N° 4819/OC-CR Programa de Apoyo a la Sostenibilidad Fiscal Ley N° 9754",
                              cod_fuentefinanciacion == "3211022600519" ~ "Crédito BID N.°4988/OC-CR Programa hacia una Economía Verde: Apoyo al Plan de Descarbonización de Costa Rica. Ley Nº 9846",
                              cod_fuentefinanciacion == "3211030000000" ~ "Banco Mundial",
                              cod_fuentefinanciacion == "3211030200514" ~ "Crédito BIRF 8593-CR, Ley No 9396, Programa por Resultados para el Fortalecimiento del Seguro Universal de Salud en Costa Rica",
                              cod_fuentefinanciacion == "3211030200531" ~ "Crédito BIRF 4557-CR, Ley N° 8058 Programa de Pagos de Servicios Ambientales.",
                              cod_fuentefinanciacion == "3211030300532" ~ "Crédito BIRF 70-68-CR, Ley N° 8269 Proyecto de Fortalecimiento y Modernización del Sector Salud",
                              cod_fuentefinanciacion == "3211030400533" ~ "Crédito BIRF 7284-CR Proyecto Equidad y Eficiencia de la Educación",
                              cod_fuentefinanciacion == "3211030500534" ~ "Crédito BIRF 7388-CR Proyecto Introducción Instrumentos Financieros para Gestión Ambiental",
                              cod_fuentefinanciacion == "3211030500535" ~ "Crédito BIRF 7594-CR Opción de Desembolsos Diferido Ante el Riesgo de Catástrofes (CAT-DDO)",
                              cod_fuentefinanciacion == "3211030500536" ~ "Crédito BIRF 7498-CR, Ley N° 8725 Proyecto de Limón Cuidad-Puerto",
                              cod_fuentefinanciacion == "3211030600537" ~ "Crédito BIRF 7686-CR, Ley N° 8843 Apoyo de Políticas de Desarrollo de las Finanzas Públicas y la Competitividad",
                              cod_fuentefinanciacion == "3211030700538" ~ "Crédifo BIRF N° 8194-CR para el Proyecto de Mejoramiento de la Educación Superior",
                              cod_fuentefinanciacion == "3211050100610" ~ "Crédito FIDA 371-CR, Ley N° 7659 Proyecto de Desarrollo Agrícola Península de Nicoya",
                              cod_fuentefinanciacion == "3211090100650" ~ "Crédito Banco Japonés de Cooperación Internacional CR-P4, Ley N° 8559 Proyecto de Mejoramiento del Medio Ambiente del Area Metropolitana de San José",
                              cod_fuentefinanciacion == "3211090200660" ~ "Crédito Corporación Andina De Fomento, Ley N° 8844 Proyecto Bajos de Chilamate - Vuelta Kooper",
                              cod_fuentefinanciacion == "3211090200661" ~ "Contrato Préstamo para financiar el Programa de Apoyo para el Fortalecimiento de las Finanzas Públicas",
                              cod_fuentefinanciacion == "3211090200662" ~ "AFD Contrato de Préstamo N.° CCR 1011 01F Ley Nº 9846. Programa de Apoyo Presupuestario para el Fortalecimiento de las Políticas",
                              cod_fuentefinanciacion == "3211090200663" ~ "FMI Crédito Instrumento de Financiamiento Rápido ( IFR) para apoyo presupuestario en la atención de l a emergencia COVID-19",
                              cod_fuentefinanciacion == "3211090200664" ~ "FMI Crédito Servicio Ampliado del Fondo (SAF)",
                              cod_fuentefinanciacion == "3211010500456" ~ "Crédito BCIE-2270 Ley 10056 Proyecto adqyuisición y aplicación de vacunas COVID-19",
                              cod_fuentefinanciacion == "3211030900540" ~ "CREDITO BIRF No. 9146-CR Primer préstamo de polít ica de desarrollo fiscal y de descarbonización Le y 9988",
                              
                              
                              cod_fuentefinanciacion == "3212001000511" ~ "Cred EXIMBANK Ley 9293Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32, Tramo: Ruta N° 4 - Limón",
                              cod_fuentefinanciacion == "3212002000512" ~ "Cred EXIMBANK Ley 9293Proyecto Rehabilitación y Ampliación de la Ruta Nacional N° 32, Tramo: Ruta N° 4 - Limón",
                              cod_fuentefinanciacion == "3212010000690" ~ "Crédito del Gobierno de los EEUU, Convenio PL-480 Leyes N° 6945, 6978, 7019, 7059, 7098, 7203 y 7307",
                              cod_fuentefinanciacion == "3212030000692" ~ "Crédito KFW 2002-65-066, Leyes N° 6979, 7132 y 7109 Programa de Rehabilitación y Mantenimiento de la Red Vial Cantonal",
                              cod_fuentefinanciacion == "3212030000693" ~ "Créditos KFW, Ley N° 7132 Programa de Agua Potable y Saneamiento Básico Rural II",
                              cod_fuentefinanciacion == "3212030000694" ~ "Crédito KFW N° 28568 Programa de Saneamiento en Zo nas Prioritarias, Ley N° 9723",
                              
                              cod_fuentefinanciacion == "3213010000730" ~ "Crédito Export - Import Bank de la República de China, Ley N° 7624 Proyecto Construcción Carretera Florencia - Naranjo.",
                              
                              cod_fuentefinanciacion == "3232010000890" ~ "Colocación de títulos valores de deuda externa",
                              
                              cod_fuentefinanciacion == "3311010000900" ~ "Superávit Libre Museo Histórico Cultural Juan Santamaria",
                              cod_fuentefinanciacion == "3311010000901" ~ "Superávit Libre Instituto sobre Alcoholismo y Farm acodependencia (IAFA)",
                              cod_fuentefinanciacion == "3311010000902" ~ "Superávit Libre Instituto Nacional de Innovación T ecnológica Agropecuaria (INTA)",
                              cod_fuentefinanciacion == "3311010000906" ~ "Superávit Libre Consejo Nacional de la Persona Adu lta Mayor (CONAPAM)",
                              cod_fuentefinanciacion == "3311010000908" ~ "Superávit Libre Instituto Costarricense de Investi gación y Enseñanza en Nutrición y Salud (INCIENSA)",
                              cod_fuentefinanciacion == "3311010000910" ~ "Superávit Libre Consejo Nacional de Concesiones (C NC)",
                              cod_fuentefinanciacion == "3311010000911" ~ "Superávit Libre Teatro Nacional (TN)",
                              cod_fuentefinanciacion == "3311010000912" ~ "Superávit Libre Consejo Nacional de Investigación en Salud (CONIS)",
                              cod_fuentefinanciacion == "3311010000913" ~ "Superávit Libre Junta Administrativa del Registro Nacional (JARN)",
                              cod_fuentefinanciacion == "3311010000914" ~ "Superávit Libre Dirección Nacional de Notariado (D NN)",
                              cod_fuentefinanciacion == "3311010000915" ~ "Superávit Libre Tribunal Registral Administrativo (TRA)",
                              cod_fuentefinanciacion == "3311010000916" ~ "Superávit Libre Museo Nacional (MN)",
                              cod_fuentefinanciacion == "3311010000917" ~ "Superávit Libre Casa de la Cultura de Puntarenas ( CCP)",
                              cod_fuentefinanciacion == "3311010000918" ~ "Superávit Libre Teatro Melico Salazar (TPMS)",
                              cod_fuentefinanciacion == "3311010000919" ~ "Superávit Libre Archivo Nacional (JAAN)",
                              cod_fuentefinanciacion == "3311010000920" ~ "Superávit Libre Consejo de la Persona Joven (CPJ)",
                              
                              cod_fuentefinanciacion == "3311030000980" ~ "Superávit Defensoría",
                              cod_fuentefinanciacion == "3311040000980" ~ "Superávit Poder Judicial",
                              cod_fuentefinanciacion == "3311050000980" ~ "Supervávit Asamblea Legislativa (2009)",
                              cod_fuentefinanciacion == "3311060000905" ~ "Superávit Contraloría",
                              cod_fuentefinanciacion == "3311060000906" ~ "Superávit Asamblea Legislativa",
                              cod_fuentefinanciacion == "3321010000922" ~ "Superávit Específico Colocación Títulos en el Exterior",
                              cod_fuentefinanciacion == "3321010000923" ~ "Superávit Específico Donación UE",
                              cod_fuentefinanciacion == "3321010000924" ~ "Superávit Específico de la Donación República de orea Iniciativa de Cooperación entre Corea y Améri ca Latina para la Alimentación y la Agricultura (K",
                              cod_fuentefinanciacion == "3321010000925" ~ "Superávit Específico préstamo CAF Bajos de Chilama te - Vuelta Kooper",
                              cod_fuentefinanciacion == "3321020000923" ~ "Superávit Específico de la donación de la Unión Europea",
                              cod_fuentefinanciacion == "3211022700520" ~ "Crédito BID N.°4864/OC-CR Programa de Infraestruct ura Vial y Promoción de Asociaciones Público Privadas. Ley Nº 9899",
                              cod_fuentefinanciacion == "3211030800539" ~ "Crédito BIRF N° 9075-CR Modernizar y Digitalizar los Sistemas Tecnológicos del Ministerio de Hacien da Ley N° 9922",
                              
                              cod_fuentefinanciacion == "3321010000943" ~ "Superávit Específico Consejo Nacional de Concesion es (CNC)",
                              cod_fuentefinanciacion == "3321010000947" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 7972",
                              cod_fuentefinanciacion == "3321010000948" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 9028",
                              cod_fuentefinanciacion == "3321010000949" ~ "Superávit Específico Instituto sobre Alcoholismo y Farmacodependencia (IAFA) Ley 8204",
                              cod_fuentefinanciacion == "3321010000950" ~ "Superávit Especifico IAFA Ley 9036",
                              cod_fuentefinanciacion == "3321010000952" ~ "Superávit Específico Sistema Nacional de Áreas de Conservación (SINAC)",
                              cod_fuentefinanciacion == "3321010000957" ~ "Superávit Específico Consejo Nacional de Viabilida d (CONAVI)",
                              cod_fuentefinanciacion == "3321010000958" ~ "Superávit Específico Comisión Nacional de Vacunaci ón y Epidemiología (CNVE)",
                              cod_fuentefinanciacion == "3321010000959" ~ "Superávit Específico Fondo de Desarrollo Social y Asignaciones Familiares (FODESAF)",
                              cod_fuentefinanciacion == "3321010000960" ~ "Superávit Específico Instituto Costarricense de In vestigación y Enseñanza en Nutrición y Salud (INCI ENSA)",
                              cod_fuentefinanciacion == "3321010000961" ~ "Superávit Específico Consejo de Seguridad Vial (CO SEVI)",
                              cod_fuentefinanciacion == "3321010000962" ~ "Superávit Específico Dirección General de Migración y Extranjería (DGME)",
                              
                              
                              # Desentralizadas 
                              
                              cod_fuentefinanciacion == "1112010500001" ~ "Impuesto a los ingresos y utilidades artículo 46 Ley 8488 CNE",
                              cod_fuentefinanciacion == "1132010910001" ~ "Impuestos Ley No.6883 y Ley No. 8495 SENASA",
                              cod_fuentefinanciacion == "1132020210001" ~ "Permiso de Transporte carga liviana COSEVI",
                              cod_fuentefinanciacion == "1132020330001" ~ "Impuestos espectáculos públicos leyes 3632 y 5780 Teatro Nacional",
                              cod_fuentefinanciacion == "1141020200001" ~ "1,5% valor CIF de productos agroquímicos SFE",
                              cod_fuentefinanciacion == "1191020000001" ~ "Timbre del Registro Nacional",
                              cod_fuentefinanciacion == "1191030000001" ~ "Timbre Propiedad Intelectual - Registro Nacional",
                              cod_fuentefinanciacion == "1191040000001" ~ "Timbre Portal Web - Registro Nacional",
                              cod_fuentefinanciacion == "1191050000001" ~ "Placas - Registro Nacional",
                              cod_fuentefinanciacion == "1191060000001" ~ "Timbre Gobierno Digital - Registro Nacional",
                              
                              cod_fuentefinanciacion == "1191070000001" ~ "Timbre Colegio Abogados - Direccion Nacional de Notariado",
                              cod_fuentefinanciacion == "1191080000001" ~ "Timbre Archivo Nacional",
                              cod_fuentefinanciacion == "1191090000001" ~ "Timbre Educacion y Cultura inciso c) artículo 10 Ley 5923 y sus reformas Museo Nacional",
                              cod_fuentefinanciacion == "1191100000001" ~ "Timbre Educacion y Cultura inciso d) artículo 10 Ley 5923 y sus reformas SINEM",
                              cod_fuentefinanciacion == "1191110000001" ~ "Timbre Pro-Parques Nacionales SINAC",
                              cod_fuentefinanciacion == "1191120000001" ~ "Timbre de Vida Silvestre SINAC",
                              cod_fuentefinanciacion == "1191130000001" ~ "Timbre Pro-Parques Nacionales CONAGEBIO",
                              cod_fuentefinanciacion == "1219020000001" ~ "Contrib. Patronal sobre la Nómina de Organos Desconcentrados",
                              cod_fuentefinanciacion == "1219030000001" ~ "Contrib. Patronal sobre la Nómina de Instittuciones Descentralizadas no Empresariales",
                              cod_fuentefinanciacion == "1219040000001" ~ "Contrib. Patronal sobre la Nómina de Gobiernos Locales",
                              
                              cod_fuentefinanciacion == "1219050000001" ~ "Contrib. Patronal sobre la Nómina de Empresas Públicas no Financieras",
                              cod_fuentefinanciacion == "1219060000001" ~ "Contrib. Patronal sobre la Nómina de Instituciones Públicas Financieras",
                              cod_fuentefinanciacion == "1219070000001" ~ "Contribución Patronal sobre la Nómina de Empresas del Sector Privado",
                              cod_fuentefinanciacion == "1311010100001" ~ "Venta de Bienes Estaciones Experimentales INTA",
                              cod_fuentefinanciacion == "1311090200001" & Año < 2021 ~ "Venta de Bienes -FONAM- Ley 7554",
                              cod_fuentefinanciacion == "1311090200001" & Año >= 2021 ~ "Venta de Bienes SENASA",
                              cod_fuentefinanciacion == "1311090300001" ~ "Venta Libros, revistas y folletos Museo Nacional",
                              cod_fuentefinanciacion == "1311090430001" ~ "Venta Libros, revistas y folletos Museo de Arte Costarricense",
                              cod_fuentefinanciacion == "1311090500001" ~ "Venta de producción planteles y otros bienes Imprenta Nacional",
                              cod_fuentefinanciacion == "1312010110001" ~ "Acarreo de Vehículos detenidos COSEVI",
                              cod_fuentefinanciacion == "1312010410001" ~ "Servicio de transporte aeroportuario CTAC",
                              
                              cod_fuentefinanciacion == "1312030403001" ~ "Servicios de recaudación Teatro Nacional",
                              cod_fuentefinanciacion == "1312030910001" ~ "Otros servicios financieros y de seguros COSEVI",
                              cod_fuentefinanciacion == "1312040120001" ~ "Alquiler de edificios CTAC ",
                              cod_fuentefinanciacion == "1312040130001" ~ "Alquiler de edificios Registro Nacional",
                              cod_fuentefinanciacion == "1312040140001" ~ "Alquiler de edificios Casa Cultura Puntarenas",
                              cod_fuentefinanciacion == "1312040150001" ~ "Alquiler de edificios Centro Nacional de la Música",
                              cod_fuentefinanciacion == "1312040160001" ~ "Alquiler de edificios Teatro Nacional",
                              cod_fuentefinanciacion == "1312040170001" ~ "Alquiler de edificios Teatro Melico Salazar",
                              cod_fuentefinanciacion == "1312040910001" ~ "Custodia de Vehículos COSEVI",
                              cod_fuentefinanciacion == "1312040920001" ~ "Apartados Registro Nacional",
                              
                              cod_fuentefinanciacion == "1312040930001" ~ "Otros Alquileres Teatro Melico Salazar",
                              cod_fuentefinanciacion == "1312090120001" ~ "Venta Servicios Capacitación Archivo Nacional",
                              cod_fuentefinanciacion == "1312090240001" ~ "Servicios de Análisis Laboratorio INTA",
                              cod_fuentefinanciacion == "1312090250001" ~ "Venta de servicios ambientales FONAFIFO",
                              cod_fuentefinanciacion == "1312090410001" ~ "Servicios culturales y recreativos Centro Costarricense de Producción Cinematográfica",
                              cod_fuentefinanciacion == "1312090420001" ~ "Servicios culturales y recreativos Centro Nacional de la Música",
                              cod_fuentefinanciacion == "1312090430001" ~ "Servicios culturales y recreativos Museo Nacional",
                              cod_fuentefinanciacion == "1312090440001" ~ "Venta de entradas y otros Teatro Nacional",
                              cod_fuentefinanciacion == "1312090450001" ~ "Venta de entradas y otros Teatro Melico Salazar",
                              cod_fuentefinanciacion == "1312090460001" ~ "Entradas a las Áreas Silvestres Protegidas SINAC",
                              
                              cod_fuentefinanciacion == "1312090620001" ~ "Servicios de publicidad e impresión de Imprenta Nacional",
                              cod_fuentefinanciacion == "1312090980001" ~ "Servicios varios COSEVI (Alquiler de Soda, fotocopias, certificaciones, etc.)",
                              cod_fuentefinanciacion == "1312090981001" ~ "Venta de Servicios Registrales  Registro Nacional",
                              cod_fuentefinanciacion == "1312090982001" ~ "Venta de Servicios Direccion Nacional Notariado",
                              cod_fuentefinanciacion == "1312090983001" ~ "Venta de Servicios SENASA",
                              cod_fuentefinanciacion == "1312090984001" ~ "Venta de servicios varios Casa de Cultura Puntarenas",
                              cod_fuentefinanciacion == "1312090985001" ~ "Venta de servicios varios Archivo Nacional",
                              cod_fuentefinanciacion == "1312090986001" ~ "Venta de servicios Museo Nacional",
                              cod_fuentefinanciacion == "1312090987001" ~ "Cursos libres Teatro Melico Salazar",
                              cod_fuentefinanciacion == "1312090988001" ~ "Uso de instal. en las Áreas Silvestres Protegidas SINAC",
                              
                              cod_fuentefinanciacion == "1312090990001" ~ "Venta de otros servicios Servicio Fitosanitario del Estado (SFE)",
                              cod_fuentefinanciacion == "1313010130001" ~ "Derechos administrativos a los servicios de transporte en carretera CNC- Concesión carretera San José-Caldera",
                              cod_fuentefinanciacion == "1313010140001" ~ "Derechos administrativos a los servicios de transporte en carretera CONAVI- Peajes carretera Florencio Castillo",
                              cod_fuentefinanciacion == "1313010150001" ~ "Derechos administrativos a los servicios de transporte en carretera CONAVI- Peajes carretera Braulio Carrillo",
                              cod_fuentefinanciacion == "1313010350001" ~ "Derechos administrativos a los servicios de transporte portuario CNC concesión Terminal Contenedores Moín",
                              cod_fuentefinanciacion == "1313010410001" ~ "Derechos administrativos a los servicios de transporte aeroportuario CTAC",
                              cod_fuentefinanciacion == "1313020210001" ~ "Matrícula Exámenes Teóricos y Pruebas Prácticas COSEVI",
                              cod_fuentefinanciacion == "1313020220001" ~ "Matrícula estudiantes Centro Nacional de la Música",
                              cod_fuentefinanciacion == "1313020360001" ~ "Cánones y Multas y Sanciones Administrativas PROHAB",
                              cod_fuentefinanciacion == "1313020370001" ~ "Derechos Administrativos a actividades comerciales SINAC",
                              
                              cod_fuentefinanciacion == "1313020920001" ~ "Emisión de Licencias de Conducir COSEVI",
                              cod_fuentefinanciacion == "1313020930001" ~ "Registro e inscripción CANNON-CONIS",
                              cod_fuentefinanciacion == "1313020940001" ~ "Derecho de Filmación SINAC",
                              cod_fuentefinanciacion == "1322010100001" ~ "Concesión de obra pública CTAC",
                              cod_fuentefinanciacion == "1322020100001" ~ "Alquiler de terrenos CTAC",
                              cod_fuentefinanciacion == "1322020200001" ~ "Alquiler de Terrenos SINAC",
                              cod_fuentefinanciacion == "1322090100001" ~ "Concesión Soda Comedor Registro Nacional",
                              cod_fuentefinanciacion == "1322090200001" ~ "Otros Ingresos de la Renta de la Propiedad SINAC",
                              cod_fuentefinanciacion == "1323010600001" ~ "Intereses sobre títulos valores de Instituciones Públicas Financieras",
                              cod_fuentefinanciacion == "1323030102001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales COSEVI",
                              
                              cod_fuentefinanciacion == "1323030103001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales FODESAF",
                              cod_fuentefinanciacion == "1323030104001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Patronato Construcciones",
                              cod_fuentefinanciacion == "1323030106001" ~ "Intereses sobre cuentas corrientes y Operaciones Bancos Estatales Teatro Nacional",
                              cod_fuentefinanciacion == "1323030401001" ~ "Diferencias por tipo de cambio SINAC",
                              cod_fuentefinanciacion == "1331020000001" ~ "Multas de tránsito. Ley No. 9078 COSEVI",
                              cod_fuentefinanciacion == "1331090800001" ~ "Multas Código Penal Ley 5386  Patronato de Construcciones",
                              cod_fuentefinanciacion == "1331091000001" ~ "Multas Sanciones Remanentes Confiscaciones Teatro Nacional",
                              cod_fuentefinanciacion == "1349010000001" ~ "Sobre multas de tránsito COSEVI",
                              cod_fuentefinanciacion == "1399020000001" ~ "Ingresos varios no especificados COSEVI",
                              cod_fuentefinanciacion == "1399030000001" ~ "Ingresos varios no especificados Instituto Costarricense sobre Drogas",
                              
                              cod_fuentefinanciacion == "1399040000001" ~ "Otros ingresos varios no especificados Artículo 44 Ley 8488 CNE",
                              cod_fuentefinanciacion == "1413090000001" ~ "Servicio Nacional de Aguas Subterráneas Riego y Avenamiento artículo 3 inciso h) Ley 8149 - INTA",
                              cod_fuentefinanciacion == "1413110000001" ~ "Instituto Costarricense de Pesca y Acuicultura artículo 3 inciso h) Ley 8149 - INTA",
                              cod_fuentefinanciacion == "1413120000001" ~ "Transf. corrientes Instit. Desce. No Empresariales CTAC",
                              cod_fuentefinanciacion == "1413130000001" ~ "Instituto de Desarrollo Rural Ley N° 9036 a IAFA",
                              cod_fuentefinanciacion == "1414040000001" ~ "De Gobierno Locales Ley 9303 CONAPDIS",
                              cod_fuentefinanciacion == "1414050000001" ~ "De Gobierno Locales Ley 7509 Registro Nacional",
                              cod_fuentefinanciacion == "1414060000001" ~ "De Gobierno Locales Casa Cultura Puntarenas",
                              cod_fuentefinanciacion == "1415050000001" ~ "Junta de  Protección Social Ley 9379 CONAPDIS",
                              cod_fuentefinanciacion == "1415060000001" ~ "Junta de Protección Social Ley 8718  a CTAMS",
                              
                              cod_fuentefinanciacion == "1415070000001" ~ "Junta de Protección Social Ley 8111 a CNVE",
                              cod_fuentefinanciacion == "1415080000001" ~ "Fábrica Nacional de Licores (Ley N° 8289) a IAFA",
                              cod_fuentefinanciacion == "1416040000001" ~ "Transferencia INS - Consejo Salud Ocupacional",
                              cod_fuentefinanciacion == "1431040000001" ~ "Transferencias de organismos internacionales Archivo Nacional",
                              cod_fuentefinanciacion == "3211010500455" ~ "Adenda al crédito público N.° 2080 BCIE/CONAVI",
                              cod_fuentefinanciacion == "3311010000901" ~ "Superávit Libre Instituto sobre Alcoholismo y Farm acodependencia (IAFA)",
                              cod_fuentefinanciacion == "3311010000911" ~ "Superávit Libre Teatro Nacional (TN)",
                              cod_fuentefinanciacion == "3321010000940" ~ "Superávit Específico Comisión Nacional para la Ges tión de la Biodiversidad (CONAGEBIO)",
                              cod_fuentefinanciacion == "3321010000942" ~ "Superávit Específico Instituto Costarricense sobre Drogas (ICD)",
                              cod_fuentefinanciacion == "3321010000945" ~ "Superávit Específico Consejo de Transporte Público (CTP)",
                              cod_fuentefinanciacion == "3321010000951" ~ "Superávit Específico Teatro Nacional (TN)",
                              cod_fuentefinanciacion == "3321010000953" ~ "Superávit Específico Patronato de Construcciones, Instalaciones y Adquisición de Bienes (PCIAB)"
                              
                            ), 
                            
)

names(Ingresos_a)

Ingresos_a <- dplyr::mutate(Ingresos_a,
                            
                            cod_sector = case_when(
                              
                              # Gobierno Central 
                              
                              cod_fuentefinanciacion == "1000000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1100000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1110000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111010101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111020101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1111030400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112010400001" ~ 'Gobierno Central',
                              #      cod_fuentefinanciacion == "1112020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020102001" ~ 'Gobierno Central',
                              #      cod_fuentefinanciacion == "1112020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1113020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1114010130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1115010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1120000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1121000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1121010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1122020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1123030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1124000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1124010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1125000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1125010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1130000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1131020210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010310000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010311001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010312001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010400000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010410000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010411001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010412001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010420000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010421001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010422001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010430000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010431001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010432001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010440000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010440001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010450000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010451001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132010452001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1132020320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1140000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1141090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1142090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143040000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1143090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1190000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1191000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1191010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1199020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1200000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213030200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1213040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1300000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1310000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090200001" & Año == 2007 ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1311090200001" & Año >= 2021 ~ 'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030102001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030401001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312030402001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312040900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090200000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090230001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090600000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090900000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090910001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090920001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090930001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1312090940001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010100000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010330001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313010340001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020140001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020300000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020310001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020320001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020340001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020350001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1313020910001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1320000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1321030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1322020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010600000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010600001" & Año < 2021 ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323010600001" & Año >= 2021 ~ 'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323010610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020500000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020510001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020600000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323020610001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1330000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331040200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090140001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090150001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1331090900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1341000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1390000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1391020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1392000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1392000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399110000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1399120000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1400000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1410000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1411020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010800001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412010900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412011500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412012000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412013000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412050000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412060000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412070000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412110000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412120000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412130000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412140000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010700001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010800001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413010900001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413011500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413050000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413060000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413070000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413080000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413090000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1414030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010110001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010120001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010130001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010210001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010220001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010230001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010300001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010400001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010500001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416010600001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1416020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1420000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1421000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1421010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1423000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1424000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1425000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1426000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1430000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431010000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1431030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432000000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432030000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1432040000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412010000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412020000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412030000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412050000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412060000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412070000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412080000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412090000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412100000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412110000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412120000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412130000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2412140000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2413010000060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2416020100060" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2431020000065" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2431020000065" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432010000066" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432030000068" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2432040000070" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "2510000000121" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000281" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010000282" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131010100280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131020000280" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3131020000283" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010100450" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010200451" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010300452" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010400453" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500454" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500513" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211020900498" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021000499" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021100500" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021100501" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021300502" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021400503" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021500504" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021600505" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021700506" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021800507" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211021900508" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022000509" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022100510" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022200515" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022300516" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022400517" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022500518" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022600519" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030000000" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030200514" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030200531" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030300532" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030400533" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500534" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500535" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030500536" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030600537" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030700538" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211050100610" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090100650" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200660" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200661" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200662" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200663" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212001000511" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212002000512" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212010000690" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000692" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000693" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3212030000694" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3213010000730" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3232010000890" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311030000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311040000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311050000980" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311060000905" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3311060000906" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000922" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000923" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000924" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321010000925" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3321020000923" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1112020200001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1323030101001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1332010100001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1342000000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1412150000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1413140000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "1415100000001" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030800539" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022700520" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211022800521" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211030900540" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211090200664" ~ 'Gobierno Central',
                              cod_fuentefinanciacion == "3211010500456" ~ 'Gobierno Central',
                              
                              
                              
                              # Desentralizadas 
                              
                              cod_fuentefinanciacion == "1112010500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1112010500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132010910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132020210001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132020330001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1141020200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191080000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191090000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191100000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191120000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1191130000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1219070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311010100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090300001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090430001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090500001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312010110001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312010410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312030403001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312030910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040120001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040130001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040140001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040150001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040160001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040170001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040910001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040920001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040930001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090120001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090240001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090250001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090420001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090430001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090440001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090450001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090460001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090620001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090980001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090981001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090982001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090983001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090984001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090985001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090986001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090987001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090988001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090990001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010130001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010140001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010150001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010350001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313010410001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020210001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020220001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020360001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020370001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020920001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020930001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1313020940001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322010100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322020100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322020200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322090100001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1322090200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323010600001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030102001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030103001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030104001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030106001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1323030401001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331090800001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331091000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1349010000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399020000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399030000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1399040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413090000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413120000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1413130000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1414060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415050000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415060000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415070000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415080000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1416040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1431040000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312090991001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3211010500455" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000901" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000911" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000940" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000942" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000945" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000951" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000953" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311010200001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1132010230001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1331040300001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1415110000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1427000000001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000900" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000902" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000906" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000908" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000910" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000912" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000913" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000914" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000915" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000916" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000917" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000918" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000919" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3311010000920" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000943" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000947" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000948" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000949" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000950" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000952" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000957" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000958" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000959" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000960" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000961" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "3321010000962" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090600001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1311090700001" ~'Órganos desconcentrados',
                              cod_fuentefinanciacion == "1312040180001" ~'Órganos desconcentrados'
                              
                              
                            )
                            
)


  
#Crear el 
# 1. ingreso ajustado 

Ingresos_a <-  dplyr::mutate (Ingresos_a, 
                              'Presupuesto ajustado' = case_when(
                                Ingresos_a$cod_fuentefinanciacion_3 <=452 ~ Ingresos_a$`Presupuesto Actual`,
                                Ingresos_a$cod_fuentefinanciacion_3 >=890 ~ Ingresos_a$`Presupuesto Actual`,
                                Ingresos_a$cod_fuentefinanciacion_3 >=453 | Ingresos_a$cod_fuentefinanciacion_3 <=694 ~ Ingresos_a$'Devengado Acumulado'
                              ),
                              'Presupuesto ajustado especial' = if_else(
                                Ingresos_a$cod_fuentefinanciacion_3 == 518 | 
                                Ingresos_a$cod_fuentefinanciacion_3 == 519 | 
                                Ingresos_a$cod_fuentefinanciacion_3 == 661 |
                                Ingresos_a$cod_fuentefinanciacion_3 == 662 |
                                Ingresos_a$cod_fuentefinanciacion_3 == 663 , `Presupuesto Actual`,`Presupuesto ajustado` 
                              )

)


Ingresos_a <- Ingresos_a %>% 
                              dplyr::rename(
                                             'Descripción'='Desc.Pos.presupuestaria',
                                             'Presupuesto actual' ='Presupuesto Actual',
                                                     'Enero'      = 'Devengado Enero',
                                                     'Febrero'    = 'Devengado Febrero',
                                                     'Marzo'      = 'Devengado Marzo',
                                                     'Abril'      = 'Devengado Abril',
                                                     'Mayo'       = 'Devengado Mayo',
                                                     'Junio'      = 'Devengado Junio',
                                                     'Julio'      = 'Devengado Julio',
                                                     'Agosto'     = 'Devengado Agosto', 
                                                     'Setiembre'  = 'Devengado Setiembre',      
                                                     'Octubre'    = 'Devengado Octubre',
                                                     'Noviembre'  = 'Devengado Noviembre',
                                                     'Diciembre'  = 'Devengado Diciembre',
                                                     'Acumulado'  = 'Devengado Acumulado'
                                    )


Ingresos_a <-  dplyr::mutate(Ingresos_a,
                             
                             Descripción = if_else (
                               
                               Ingresos_a$PosPre == "I1413020000001",  "Instituto Nacional de Aprendizaje. Ley Nº7372 / 22 -11-93", Ingresos_a$Descripción
                            )
)



#   Ingresos_a <-  dplyr::mutate (Ingresos_a, 
#                              'Presupuesto ajustado' = case_when(
#                                Ingresos_a$cod_fuentefinanciacion_3 <=452 ~ Ingresos_a$`Presupuesto Actual`,
#                                Ingresos_a$cod_fuentefinanciacion_3 >=890 ~ Ingresos_a$`Presupuesto Actual`,
#                                Ingresos_a$cod_fuentefinanciacion_3 >=453 | Ingresos_a$cod_fuentefinanciacion_3 <=694 ~ Ingresos_a$'Devengado Acumulado'
#                              ),
#                              'Presupuesto ajustado especial' = if_else(
#                                Ingresos_a$cod_fuentefinanciacion_3 == 518 | 
#                                  Ingresos_a$cod_fuentefinanciacion_3 == 519 | 
#                                  Ingresos_a$cod_fuentefinanciacion_3 == 661 |
#                                  Ingresos_a$cod_fuentefinanciacion_3 == 662 |
#                                  Ingresos_a$cod_fuentefinanciacion_3 == 663 , `Presupuesto Actual`,`Presupuesto ajustado` 
#                              )
# )


# 2. ingreso inicial

# Ajuste normal

# conteo2 <- Ingresos_a  %>%  %>% dplyr::group_by(Pospreanual) %>% dplyr::summarise('Conteo' = n()) %>% dplyr::arrange(desc(Conteo))
# conteo2 %>% dplyr::summarise('Suma' = sum(Conteo,na.rm = TRUE))

# dim(Ingresos_a)

Ingresos_a <- Ingresos_a %>%  dplyr::full_join(PPI_a) %>% dplyr::filter(Año >= 2000)

# dim(Ingresos_a)

# Selección de variabeles y posicionamiento

Ingresos_a <- Ingresos_a  %>%
  dplyr:: select(Año, Nivel, n_Nivel,cod_sector,PosPre,Descripción,'Presupuesto actual',  'Presupuesto inicial', 'Presupuesto ajustado','Presupuesto ajustado especial', Acumulado, cod_clase,clase,cod_subclase,subclase,cod_grupo,grupo,cod_subgrupo,subgrupo,
                 cod_partida,partida,cod_subpartida,subpartida,cod_renglon,renglon,cod_subrenglon,subrenglon,
                 cod_fuentefinanciacion, cod_fuentefinanciacion_3,fuentefinanciacion, Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Setiembre,
                 Octubre, Noviembre, Diciembre)

# ARchivos sin los "0" en el cod_fuentefinanciacion_3

Ingresos_a_s0 <- Ingresos_a  %>%
                      dplyr::filter(cod_fuentefinanciacion_3!=0)  
 



###############################
# Eliminar archivos de datos  #
###############################

remove(PPI)
remove(PPI_a)

remove(ingresos_2007_2021)
remove(ingresos_2007_2021_a)
remove(ingresos_2007_2021_m)
remove(SIGAF_Ingresos_actual_2022)
remove(SIGAF_Ingresos_actual_2022_a)
remove(SIGAF_Ingresos_actual_2022_m)


#####
# 4 #  
########################################################################################################################
########################################################################################################################
#                                            Creación de tablas                                                        #
########################################################################################################################
########################################################################################################################


###############################
#  Evolución de los ingresos  #
###############################

# Anual

tabla_1 <- Ingresos_a %>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año) %>% 
  dplyr::summarise (  "Actual"    = sum(`Presupuesto actual`, na.rm = TRUE),
                      "Ajustado"   = sum(`Presupuesto ajustado`, na.rm = TRUE),
                      "Acumulado"  = sum(`Acumulado`, na.rm = TRUE),
                      "Inicial" = sum(`Presupuesto inicial`,na.rm=TRUE)
                      )


# Mensual

tabla_2 <-  Ingresos_m %>%
                 dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                 dplyr::group_by(Año, mes.cod, mes) %>% 
                 dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE))  %>%
                 mutate(
                   Fecha = paste(Año,mes,sep ="-"))  


tabla_2 <- as.data.frame(tabla_2)

Ingresos_mensual <- tabla_2

Ingresos_mensual <- Ingresos_mensual %>%
  dplyr::mutate(
    Ingresos = round(Ingresos/Millones,1)
  )

Ingresos_mensual  <- highchart() %>% 
  hc_title(text = "",
           margin = 20, align = "center",
           style = list(color = "#129", useHTML = TRUE)) %>% 
  hc_subtitle(text = "",
              align = "right",
              style = list(color = "#634", fontWeight = "bold")) %>%
  hc_credits(enabled = TRUE, # add credits
             text = "" # href = "www.cgr.go.cr"
  ) %>%
  hc_legend(align = "left", verticalAlign = "top",
            layout = "vertical", x = 0, y = 100) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_xAxis(categories = Ingresos_mensual$Fecha) %>% 
  hc_add_series(name = "Ingresos", data = Ingresos_mensual$Ingresos) %>% 
  hc_chart(zoomType = "xy")

Ingresos_mensual


tabla_2_var <- tabla_2 %>% mutate( 
                                   var.Ingresos = round((Ingresos/lag(Ingresos,n = 12)-1)*100,2),
                                   acum_12 = roll_sum(Ingresos, 12, align = "right", fill = NA),
                                   var.acum_12  = round((acum_12/lag(acum_12,n = 12)-1)*100,1)
                                 )

tabla_2_var <- tabla_2_var  %>% 
               dplyr::group_by(Año) %>% 
               dplyr::mutate(  cum_ano_Ingresos     = cumsum(Ingresos)
                              )
tabla_2_var <- data.frame(tabla_2_var)

tabla_2_var <-  tabla_2_var %>% dplyr::mutate(
                                       var.cum_ano_Ingresos =  round((cum_ano_Ingresos/lag(cum_ano_Ingresos,n = 12)-1)*100,1) 
                                       )

# datos <- datos %>% dplyr::group_by(Año)  %>% dplyr::mutate(
#   cum_ano_Ingresos     = cumsum(Ingresos)
# )  
# datos <- data.frame(datos)
# 
# datos <- datos %>% dplyr::mutate(var.cum_ano_Ingresos =  round((cum_ano_Ingresos/lag(cum_ano_Ingresos,n = 12)-1)*100,1) )


########################################################################################################################
########################################################################################################################
#                                         Principales indicadores                                                      #
########################################################################################################################
########################################################################################################################

#####################
# Ingresos globales #
#####################

#Recaudación acumulada.

indicador.1 <- Ingresos_a %>%
  dplyr::filter(Año==2021) %>% 
  dplyr::filter(cod_subclase==11)  %>% 
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::summarise ("Ingresos"  = sum(Acumulado, na.rm = TRUE)) 

#Carga tributaria

ind.PIB <- PIB %>% 
  dplyr::filter(Año==2021) %>% 
  dplyr::select('PIB corriente, a precios de mercado')

ind.PIB <-ind.PIB*1000000

indicador.2 <- round(indicador.1/ind.PIB*100,1)


# Ejecución

Presu.ajustado <- Ingresos_a %>%
  dplyr::filter(Año==2021) %>% 
  dplyr::filter(cod_subclase==11)  %>% 
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::summarise ("Presupuesto ajustado"  = sum(`Presupuesto ajustado`, na.rm = TRUE)) 

indicador.3 <- round((indicador.1/Presu.ajustado)*100,1)


#Var% acumulado a 12 meses

indicador.4 <-  tabla_2_var  %>% select(var.Ingresos) %>% slice(c( n()))
                                                  
#Var% interanual (mensual y acumulada)

indicador.5 <- tabla_2_var  %>% select(var.acum_12) %>% slice(c( n()))

###############################
#  Ingresos por Clasificador  #
###############################

###############
#     Clase   #
###############

tabla_clasi_c_1_1 <- Ingresos_m  %>%
                       dplyr::filter(Nivel==1)%>%
                       dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                       dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
                       dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
                       mutate(
                         Fecha = paste(Año,mes,sep ="-")) %>%
                       dplyr::arrange(Descripción, Año)  %>% 
                       pivot_wider(
                         names_from = Descripción,
                         values_from = Ingresos
                       )  
                         

###############
# Subclase
###############

tabla_clasi_c_2_1 <- Ingresos_m  %>%
                                  dplyr::filter(Nivel==2)%>%
                                  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                                  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
                                  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
                                  mutate(
                                    Fecha = paste(Año,mes,sep ="-")) %>%
                                  dplyr::arrange(Descripción, Año)  %>% 
                                  pivot_wider(
                                    names_from = Descripción,
                                    values_from = Ingresos
                                  )  

###############
# Grupo
###############

tabla_clasi_c_3_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==3)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  

###############
# Subgrupo
###############

tabla_clasi_c_4_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==4)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  

###############
# Partida
###############

tabla_clasi_c_5_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==5)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  

###############
# Subpartida
###############

tabla_clasi_c_6_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==6)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  

###############
# Renglón
###############

tabla_clasi_c_7_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==7)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  

###############
# Subrenglón
###############

tabla_clasi_c_8_1 <- Ingresos_m  %>%
  dplyr::filter(Nivel==8)%>%
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
  mutate(
    Fecha = paste(Año,mes,sep ="-")) %>%
  dplyr::arrange(Descripción, Año)  %>% 
  pivot_wider(
    names_from = Descripción,
    values_from = Ingresos
  )  


#########################
# Fuente de Financiación
#########################

tabla_clasi_c_9_1 <- Ingresos_m  %>%
                                     dplyr::filter(Nivel==9)%>%
                                     dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                                     dplyr::group_by(Año, Descripción, mes.cod, mes) %>%
                                     dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) %>%
                                     mutate(
                                       Fecha = paste(Año,mes,sep ="-")) %>%
                                     dplyr::arrange(Descripción, Año)  %>% 
                                     pivot_wider(
                                       names_from = Descripción,
                                       values_from = Ingresos
                                     )  
                                   

###################################
# Tablas de las estacioanalidades #
###################################

#############
#  General  #
#############

# Pasado

Estacionalidad_sin_AC <-  Ingresos_m  %>% dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                                                                  dplyr::filter(Año!=Ano_actual)  %>%
                                                                  dplyr::group_by(Año,mes.cod,mes) %>%
                                                                  dplyr::summarise ("Ingresos"  = sum(`Ingresos`, na.rm = TRUE))



  
Estacionalidad_sin_AC <-   Estacionalidad_sin_AC  %>%
                                                        dplyr::group_by(Año) %>%                                                            
                                                        dplyr::mutate( 
                                                                        Ingreso_acumulado = cumsum(Ingresos), 
                                                                        Sumarecaudacion = sum(`Ingresos`),
                                                                        Estacionalidad = Ingresos/Sumarecaudacion,
                                                                        Estacionalidad_acumulada = cumsum(Estacionalidad),
                                                                        Fecha = paste(Año,mes,sep ="-"))  %>%
                                                        dplyr::arrange(Año,mes.cod)

 
# Actual

Ingresos_total_actual <- Ingresos_a  %>% 
                                         dplyr::filter(cod_fuentefinanciacion_3!=0)  %>%                                                                  
                                         dplyr::filter(Año==Ano_actual) %>% 
                                         dplyr::summarise ("Ingresos_total"  = sum(`Presupuesto actual`, na.rm = TRUE))  %>%
                                         dplyr:: mutate ( `Ingresos_total` = as.numeric (Ingresos_total) )

Ingresos_total_actual <- as.numeric(Ingresos_total_actual)


Estacionalidad_AC <-  Ingresos_m  %>% 
                          dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
                          dplyr::filter(Año==Ano_actual)  %>%
                          dplyr::group_by(Año,mes.cod,mes) %>% 
                          dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) 


Estacionalidad_AC <-  Estacionalidad_AC %>% 
                      dplyr::group_by(Año) %>%                                                            
                      dplyr::mutate( 
                        Ingreso_acumulado = cumsum(Ingresos), 
                        Sumarecaudacion = Ingresos_total_actual,
                        Estacionalidad = Ingresos/Sumarecaudacion,
                        Estacionalidad_acumulada = cumsum(Estacionalidad),
                        Fecha = paste(Año,mes,sep ="-"))  %>%
                      dplyr::arrange(Año,mes.cod) 


Estacionalidad <-  dplyr::bind_rows(Estacionalidad_sin_AC,Estacionalidad_AC)

remove(Estacionalidad_sin_AC)
remove(Estacionalidad_AC)

Estacioanalidades_promedio <-  Estacionalidad %>% 
                                           dplyr::group_by(mes) %>%  
                                           dplyr::summarise('Estacionalidad promedio' =  mean(Estacionalidad, na.rm = TRUE),
                                                            'Estacionalidad acumulada promedio' = mean(Estacionalidad_acumulada, na.rm = TRUE))



Estacionalidad <- Estacionalidad %>%  dplyr::full_join(Estacioanalidades_promedio) 

Estacionalidad <- as.data.frame(Estacionalidad)

#   HC_Estacionalidad  <- highchart() %>% 
#     hc_title(text = "",
#              margin = 20, align = "center",
#              style = list(color = "#129", useHTML = TRUE)) %>% 
#     hc_subtitle(text = "",
#                 align = "right",
#                 style = list(color = "#634", fontWeight = "bold")) %>%
#     hc_credits(enabled = TRUE, # add credits
#                text = "" # href = "www.cgr.go.cr"
#     ) %>%
#     hc_legend(align = "left", verticalAlign = "top",
#               layout = "vertical", x = 0, y = 100) %>%
#     hc_exporting(enabled = TRUE) %>% 
#     hc_xAxis(categories = Estacionalidad$Fecha) %>% 
#     hc_add_series(name = "Estacionalidad", data = Estacionalidad$Estacionalidad) %>% 
#     hc_add_series(name = "Estacionalidad promedio", data = Estacionalidad$`Estacionalidad promedio`) %>% 
#     hc_chart(zoomType = "xy")
#   
#   HC_Estacionalidad
#   
#   
#   HC_Estacionalidad_acumulada  <- highchart() %>% 
#     hc_title(text = "",
#              margin = 20, align = "center",
#              style = list(color = "#129", useHTML = TRUE)) %>% 
#     hc_subtitle(text = "",
#                 align = "right",
#                 style = list(color = "#634", fontWeight = "bold")) %>%
#     hc_credits(enabled = TRUE, # add credits
#                text = "" # href = "www.cgr.go.cr"
#     ) %>%
#     hc_legend(align = "left", verticalAlign = "top",
#               layout = "vertical", x = 0, y = 100) %>%
#     hc_exporting(enabled = TRUE) %>% 
#     hc_xAxis(categories = Estacionalidad$Fecha) %>% 
#     hc_add_series(name = "Estacionalidad acumulada", data = Estacionalidad$`Estacionalidad_acumulada`) %>% 
#     hc_add_series(name = "Estacionalidad acumulada promedio", data = Estacionalidad$`Estacionalidad acumulada promedio`) %>% 
#     hc_chart(zoomType = "xy")
#     
#     
#   HC_Estacionalidad_acumulada   

# HC_Estacionalidad  <- highchart() %>% 
#   hc_title(text = "",
#            margin = 20, align = "center",
#            style = list(color = "#129", useHTML = TRUE)) %>% 
#   hc_subtitle(text = "",
#               align = "right",
#               style = list(color = "#634", fontWeight = "bold")) %>%
#   hc_credits(enabled = TRUE, # add credits
#              text = "" # href = "www.cgr.go.cr"
#   ) %>%
#   hc_legend(align = "left", verticalAlign = "top",
#             layout = "vertical", x = 0, y = 100) %>%
#   hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
#              shared = TRUE, borderWidth = 5) %>% 
#   hc_exporting(enabled = TRUE) %>% 
#   hc_xAxis(categories = Estacionalidad$Fecha) %>% 
#   hc_add_series(name = "Estacionalidad", data = Estacionalidad$Estacionalidad) %>% 
#     hc_chart(zoomType = "xy")
#
# HC_Estacionalidad


###########################
#  Impuestos tributarios  #
###########################

# Ingresos_m  %>% dplyr::group_by(cod_subclase, subclase) %>% dplyr::summarise( 'total' = n())

# Pasado

IT_Estacionalidad_sin_AC <-  Ingresos_m  %>% dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::filter(cod_subclase==11) %>% 
  dplyr::filter(Año!=Ano_actual)  %>%
  dplyr::group_by(Año,mes.cod,mes) %>%
  dplyr::summarise ("Ingresos"  = sum(`Ingresos`, na.rm = TRUE))



IT_Estacionalidad_sin_AC <-   IT_Estacionalidad_sin_AC  %>%
  dplyr::group_by(Año) %>%                                                            
  dplyr::mutate( 
    Ingreso_acumulado = cumsum(Ingresos), 
    Sumarecaudacion = sum(`Ingresos`),
    Estacionalidad = Ingresos/Sumarecaudacion,
    Estacionalidad_acumulada = cumsum(Estacionalidad),
    Fecha = paste(Año,mes,sep ="-"))  %>%
  dplyr::arrange(Año,mes.cod)

# Actual

IT_Ingresos_total_actual <- Ingresos_a  %>% 
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>%  
  dplyr::filter(cod_subclase==11) %>% 
  dplyr::filter(Año==Ano_actual) %>% 
  dplyr::summarise ("Ingresos_total"  = sum(`Presupuesto actual`, na.rm = TRUE))  %>%
  dplyr:: mutate ( `Ingresos_total` = as.numeric (Ingresos_total) )

IT_Ingresos_total_actual <- as.numeric(Ingresos_total_actual)


IT_Estacionalidad_AC <-  Ingresos_m  %>% 
  dplyr::filter(cod_subclase==11) %>% 
  dplyr::filter(cod_fuentefinanciacion_3!=0)  %>% 
  dplyr::filter(Año==Ano_actual)  %>%
  dplyr::group_by(Año,mes.cod,mes) %>% 
  dplyr::summarise ("Ingresos"  = sum(Ingresos, na.rm = TRUE)) 


IT_Estacionalidad_AC <-  IT_Estacionalidad_AC %>% 
  dplyr::group_by(Año) %>%                                                            
  dplyr::mutate( 
    Ingreso_acumulado = cumsum(Ingresos), 
    Sumarecaudacion = Ingresos_total_actual,
    Estacionalidad = Ingresos/Sumarecaudacion,
    Estacionalidad_acumulada = cumsum(Estacionalidad),
    Fecha = paste(Año,mes,sep ="-"))  %>%
  dplyr::arrange(Año,mes.cod) 


IT_Estacionalidad <-  dplyr::bind_rows(IT_Estacionalidad_sin_AC,IT_Estacionalidad_AC)

remove(IT_Estacionalidad_sin_AC)
remove(IT_Estacionalidad_AC)

IT_Estacionalidad_promedio <-  IT_Estacionalidad %>% 
                         dplyr::group_by(mes) %>%  
                         dplyr::summarise('Estacionalidad promedio' =  mean(Estacionalidad, na.rm = TRUE),
                                          'Estacionalidad acumulada promedio' = mean(Estacionalidad_acumulada, na.rm = TRUE))



IT_Estacionalidad <- IT_Estacionalidad %>%  dplyr::full_join(IT_Estacionalidad_promedio) 

IT_Estacionalidad <- as.data.frame(IT_Estacionalidad)


#####
# 5 # 
#################################################################################################### 
####################################################################################################
####################################################################################################
#                                              Exportación                                         #
####################################################################################################
####################################################################################################
#################################################################################################### 

# Directorio de exportación de las tablas #

setwd("C:/Users/oscar/Desktop/Ingresos - SIGAF/AI ---/AI_WEB/Scripts_tablas")


###### Indicadores ######

write.xlsx(indicador.1,"indicador.1.xlsx", overwrite = TRUE)
write.xlsx(indicador.2,"indicador.2.xlsx", overwrite = TRUE)
write.xlsx(indicador.3,"indicador.3.xlsx", overwrite = TRUE)
write.xlsx(indicador.4,"indicador.4.xlsx", overwrite = TRUE)
write.xlsx(indicador.5,"indicador.5.xlsx", overwrite = TRUE)

######    Tablas: anual + mensual del presupuesto   ######  

write.xlsx(Ingresos_a, "ingresos_anual.xlsx", overwrite = TRUE)
write.xlsx(Ingresos_m, "ingresos_mensual.xlsx", overwrite = TRUE)
write.xlsx(Ingresos_a_s0, "Ingresos_anual_s0.xlsx", overwrite = TRUE)

######    Tabla de impuestos   ######  


names(Impuestos)

write.xlsx(Impuestos, "Impuestos.xlsx", overwrite = TRUE)
#write.xlsx(Impuestos_0, "Impuestos_0.xlsx")



######    Tablas: evolucioón del presupuesto anual   ######  

write.xlsx(tabla_1, "tabla_1.xlsx", overwrite = TRUE)

######   Tablas: evolucioón del ingreso mensual             ######  

write.xlsx(tabla_2, "tabla_2.xlsx", overwrite = TRUE)
write.xlsx(tabla_2_var, "tabla_2_var.xlsx", overwrite = TRUE)

######   Tablas:  Clasificador del ingreso   ####


# Clase
write.xlsx(tabla_clasi_c_1_1, "tabla_clasi_c_1_1.xlsx", overwrite = TRUE)

# Subclase
write.xlsx(tabla_clasi_c_2_1, "tabla_clasi_c_2_1.xlsx", overwrite = TRUE)

# Grupo
write.xlsx(tabla_clasi_c_3_1, "tabla_clasi_c_3_1.xlsx", overwrite = TRUE)

# Subgrupo
write.xlsx(tabla_clasi_c_4_1, "tabla_clasi_c_4_1.xlsx", overwrite = TRUE)

# Partida
write.xlsx(tabla_clasi_c_5_1, "tabla_clasi_c_5_1.xlsx", overwrite = TRUE)

# Subpartida
write.xlsx(tabla_clasi_c_6_1, "tabla_clasi_c_6_1.xlsx", overwrite = TRUE)

# Renglón
write.xlsx(tabla_clasi_c_7_1, "tabla_clasi_c_7_1.xlsx", overwrite = TRUE)

# Subrenglón 
write.xlsx(tabla_clasi_c_8_1, "tabla_clasi_c_8_1.xlsx", overwrite = TRUE)

# Fuente de Financiación
write.xlsx(tabla_clasi_c_9_1, "tabla_clasi_c_9_1.xlsx", overwrite = TRUE)

# Estacionalidades 

write.xlsx(Estacionalidad, "Estacionalidad.xlsx", overwrite = TRUE) 
write.xlsx(IT_Estacionalidad, "IT_Estacionalidad.xlsx", overwrite = TRUE) 



################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
