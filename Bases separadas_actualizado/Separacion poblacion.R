library("readxl")
library(dplyr)
library(lubridate)
library(MESS)

Activos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/Activos3.csv")
## Numero de cuotas salarios
Indicadora_salario<-Activos[,11:370]>0
Activos$num_cuotas<-rowSums(Indicadora_salario)
IPC<- read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Bases de datos/Inflacion mensual historica.xlsx")
Salarios<-Activos[,-c(1:10,371,372,373)]
inflacion<-(1+IPC[,2]/100)
inflacion<-inflacion$Inflacion
inflacion<-rev(cumprod(rev(inflacion)))


Salarios_ajustado<-sapply(1:360, function(i) Salarios[,i]*inflacion[i])
colnames(Salarios_ajustado)<-colnames(Salarios)
Salarios_ajustado<-as.data.frame(Salarios_ajustado)
Activos[11:370]<-Salarios_ajustado
Promedios<-numeric(nrow(Activos))
for (i in 1:nrow(Activos)) {
  aux<-Salarios_ajustado[i,349:360]
  aux<-aux[aux>0]
  Promedios[i]<-mean(aux)
}

Activos$Salario_prom<-Promedios
Activos<-Activos[,-374]


write.csv(Activos,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Activos_ciclo.csv" , row.names = FALSE)


## Agregar edad a pensionados
Pensionados<-read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/BD Pensionados.xlsx",sheet = "Fondo B")

#Depuracion_Pensionados
Pensionados<-Pensionados[,-c(7,9:11)]
#Pensionados<-Pensionados[-c(366:378),]


fecha_corte<-as.Date("2023-12-31")
edad<-age(as.Date(Pensionados$FEC_NAC),fecha_corte)
Pensionados<-cbind(Pensionados[1:4],data.frame(Edad= edad), Pensionados[,5:ncol(Pensionados)])




# Creacion de bases pensionados invalidez
Pensionados_invalidez<-Pensionados[Pensionados$COD_TIPO_PENSION=="Invalidez",]
Pensionados_invalidez<-Pensionados_invalidez[,-3]
write.csv(Pensionados_invalidez,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Pensionados_Invalidez.csv" , row.names = FALSE)


# Creacion de bases pensionados vejez
Pensionados_vejez<-Pensionados[Pensionados$COD_TIPO_PENSION=="Vejez",]
Pensionados_vejez<-Pensionados_vejez[,-3]
write.csv(Pensionados_vejez,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Pensionados_vejez.csv" , row.names = FALSE)

# Creacion de bases pensionados huerfanitos
Pensionados_huerfanos<-Pensionados[Pensionados$COD_TIPO_PENSION=="Sucesión" & Pensionados$COD_PARENTESCO=='H',]
Pensionados_huerfanos_vigente<-Pensionados_huerfanos[Pensionados_huerfanos$Edad<25,]
write.csv(Pensionados_huerfanos,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_huerfanos.csv" , row.names = FALSE)
write.csv(Pensionados_huerfanos_vigente,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Pensionados_huerfanos_vigente.csv" , row.names = FALSE)
# Creacion de bases pensionados forever alones
Pensionados_viudos<-Pensionados[Pensionados$COD_TIPO_PENSION=="Sucesión" & Pensionados$COD_PARENTESCO=='C',]
write.csv(Pensionados_viudos,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Pensionados_viudos.csv" , row.names = FALSE)



### Inactivos 
Inactivos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases de datos/Inactivos3.csv")
Inactivos<-Inactivos[,c(1,2,3,4,9,11:370)]
Indicadora_salario<-Inactivos[,5:364]>0
Inactivos$num_cuotas<-rowSums(Indicadora_salario)


Salarios<-Inactivos[,-c(1:5,366)]
Salarios_ajustado<-sapply(1:360, function(i) Salarios[,i]*inflacion[i])
colnames(Salarios_ajustado)<-colnames(Salarios)
Salarios_ajustado<-as.data.frame(Salarios_ajustado)
Inactivos[6:365]<-Salarios_ajustado


Inactivos$Salario_prom<-0




Inactivos_mas180<-Inactivos[Inactivos$num_cuotas>=180,]
Inactivos_menos180_menor48<-Inactivos[Inactivos$num_cuotas<180 & Inactivos$Edad<48,]
Inactivos_menos180_mas48<-Inactivos[Inactivos$num_cuotas<180 & Inactivos$Edad>=48,]

write.csv(Inactivos_mas180,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Inactivo_180.csv" , row.names = FALSE)
write.csv(Inactivos_menos180_menor48,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Inactivo_menos180_menor48.csv" , row.names = FALSE)
write.csv(Inactivos_menos180_mas48,"C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas_actualizado/Inactivo_menos180_mas48.csv" , row.names = FALSE)
