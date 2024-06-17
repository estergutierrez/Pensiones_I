library("readxl")
library(dplyr)
library(lubridate)
library(MESS)

Activos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/Activos3.csv")

## Agregar edad a pensionados
Pensionados<-read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/BD Pensionados.xlsx",sheet = "Fondo B")

#Depuracion_Pensionados
Pensionados<-Pensionados[,-c(7,9:11)]
Pensionados<-Pensionados[-c(366:378),]


fecha_corte<-as.Date("2023-12-31")
edad<-age(as.Date(Pensionados$FEC_NAC),fecha_corte)
Pensionados<-cbind(Pensionados[1:4],data.frame(Edad= edad), Pensionados[,5:ncol(Pensionados)])




# Creacion de bases pensionados invalidez
Pensionados_invalidez<-Pensionados[Pensionados$COD_TIPO_PENSION=="Invalidez",]
Pensionados_invalidez<-Pensionados_invalidez[,-3]
write.csv(Pensionados_invalidez,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_Invalidez.csv" , row.names = FALSE)


# Creacion de bases pensionados vejez
Pensionados_vejez<-Pensionados[Pensionados$COD_TIPO_PENSION=="Vejez",]
Pensionados_vejez<-Pensionados_vejez[,-3]
write.csv(Pensionados_vejez,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_vejez.csv" , row.names = FALSE)

# Creacion de bases pensionados huerfanitos
Pensionados_huerfanos<-Pensionados[Pensionados$COD_TIPO_PENSION=="Sucesión" & Pensionados$COD_PARENTESCO=='H',]
Pensionados_huerfanos_vigente<-Pensionados_huerfanos[Pensionados_huerfanos$Edad<25,]
write.csv(Pensionados_huerfanos,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_huerfanos.csv" , row.names = FALSE)
write.csv(Pensionados_huerfanos_vigente,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_huerfanos_vigente.csv" , row.names = FALSE)
# Creacion de bases pensionados forever alones
Pensionados_viudos<-Pensionados[Pensionados$COD_TIPO_PENSION=="Sucesión" & Pensionados$COD_PARENTESCO=='C',]
write.csv(Pensionados_viudos,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_viudos.csv" , row.names = FALSE)



### Inactivos 
Inactivos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/Inactivos3.csv")
Inactivos<-Inactivos[,c(1,2,3,4,9,11:370)]
Indicadora_salario<-Inactivos[,5:364]>0
Inactivos$num_cuotas<-rowSums(Indicadora_salario)

Inactivos_mas180<-Inactivos[Inactivos$num_cuotas>=180,]
Inactivos_menos180<-Inactivos[Inactivos$num_cuotas<180,]

write.csv(Inactivos_mas180,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Inactivo_180.csv" , row.names = FALSE)
write.csv(Inactivos_menos180,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Inactivo_menos180.csv" , row.names = FALSE)
