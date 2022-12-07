############################################################
#  Prueba para hacer el match entre dos archivos de datos  #
############################################################

######################
# Opciones generales #
###################### 

options(encoding="utf-8")
options(scipen=999)

###############
#  Librerías  #
###############

suppressMessages(if(!require(readxl)){ install.packages("readxl")}) 
suppressMessages(if(!require(dplyr)){ install.packages("dplyr")})
suppressMessages(if(!require(DT)){ install.packages("DT")})
suppressMessages(if(!require(plyr)){ install.packages("plyr")})
suppressMessages(if(!require(readr)){ install.packages("readr")})
suppressMessages(if(!require(janitor)){ install.packages("janitor")})
suppressMessages(if(!require(shiny)){ install.packages("shiny")})
suppressMessages(if(!require(shinydashboard)){ install.packages("shinydashboard")})
suppressMessages(if(!require(shinydashboardPlus)){ install.packages("shinydashboardPlus")}) 
suppressMessages(if(!require(highcharter)){ install.packages("highcharter")})
suppressMessages(if(!require(formattable)){ install.packages("formattable")})
suppressMessages(if(!require(highcharter)){ install.packages("highcharter")})
suppressMessages(if(!require(viridisLite)){ install.packages("viridisLite")})
suppressMessages(if(!require(stringi)){ install.packages("stringi")})
suppressMessages(if(!require(data.table)){ install.packages("data.table")})
suppressMessages(if(!require(data.table)){ install.packages("tidyr")})
suppressMessages(if(!require(data.table)){ install.packages("forecast")})
suppressMessages(if(!require(data.table)){ install.packages("kableExtra")})
suppressMessages(if(!require(data.table)){ install.packages("shinyWidgets")})
suppressMessages(if(!require(data.table)){ install.packages("png")})
suppressMessages(if(!require(data.table)){ install.packages("scales")})
suppressMessages(if(!require(data.table)){ install.packages("gt")})
suppressMessages(if(!require(data.table)){ install.packages("reactable")})
suppressMessages(if(!require(data.table)){ install.packages("RcppRoll")})
suppressMessages(if(!require(data.table)){ install.packages("sunburstR")})
suppressMessages(if(!require(data.table)){ install.packages("htmltools")})
suppressMessages(if(!require(data.table)){ install.packages("d3r")})
suppressMessages(if(!require(data.table)){ install.packages("openxlsx")})

suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(readr))
suppressMessages(library(janitor))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinydashboardPlus))
suppressMessages(library(highcharter))
suppressMessages(library(formattable))
suppressMessages(library(highcharter))
suppressMessages(library(viridisLite))
suppressMessages(library(stringi))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))
suppressMessages(library(forecast))
suppressMessages(library(kableExtra))
suppressMessages(library(shinyWidgets))
suppressMessages(library(png))
suppressMessages(library(scales))
suppressMessages(library(gt))
suppressMessages(library(reactable))
suppressMessages(library(RcppRoll))
suppressMessages(library(sunburstR))
suppressMessages(library(htmltools))
suppressMessages(library(d3r))

suppressMessages(library(openxlsx))


##################
#  Data frames   # 
##################

clasificador <- data.frame(
                               clasi_original   = c("Caja Costarricense de Seguro Social", "Refineria de Costa Rica", "Contraloría") ,
                               clasi_arreglado  = c("CCSS", "RECOPE", "CGR")
                           
)

actual <- data.frame( 
                                           id = c(1,2,3,4,5,6,7),
                               clasi_original = c("Caja Costarricense de Seguro Social", "Refineria de Costa Rica", "Contraloría",
                                                  "Refineria de Costa Rica", "Contraloría","","")
)


#####################
#  Hacer el match   #  
#####################

library(dplyr)

actual_1 <- dplyr::inner_join(actual, clasificador, by = "clasi_original")

actual_2 <- actual %>% dplyr::full_join(clasificador)


