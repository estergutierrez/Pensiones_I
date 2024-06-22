## El fondo dura 97 años más
## Funciones para programar:

# Invalidez [SI]
# Viudez [SI]
# Orfandad [SI]
# Vejez [SI]
# Activo
# Inactivo con más de 180 cuotas y mayor de 48
# Inactivo con menos de 180 cuotas y menor de 48

# Librerias
library(dplyr)
library(readxl)
library(tictoc)
# Probabilidades de transición
Mortalidad <- read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Probabilidades/transicion.xls", 
                         sheet = "Mortalidad")
Mortalidad <- Mortalidad[,-c(5,7)]
Mortalidad$sex <- as.integer(Mortalidad$sex)
Mortalidad$edad <- as.integer(Mortalidad$edad)
Mortalidad$ynac <- as.integer(Mortalidad$ynac)
Mortalidad$year <- as.integer(Mortalidad$year)
Mortalidad$qx <- as.numeric(Mortalidad$qx)
Mortalidad$px <- 1 - Mortalidad$qx
Invalidez <- read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Probabilidades/transicion.xls", 
                        sheet = "Invalidez")
Invalidez$ix <- as.numeric(Invalidez$ix)

sobrevivencia <- list()

for (i in 1:67) {
  sobrevivencia[[i]] <- as.data.frame(Mortalidad[Mortalidad$ynac == 1934+i,])
  sobrevivencia[[i]]$ix <- Invalidez$ix[c(which(Invalidez$Edad == sobrevivencia[[i]]$edad[1])[1]:116,which(Invalidez$Edad == sobrevivencia[[i]]$edad[1])[2]:232)]
  sobrevivencia[[i]]$r00_pen <- 0.1*sobrevivencia[[i]]$px*(1-sobrevivencia[[i]]$ix)
  sobrevivencia[[i]]$r01_pen <- 1-sobrevivencia[[i]][,5]-sobrevivencia[[i]][,7]-sobrevivencia[[i]][,8]
  sobrevivencia[[i]]$r00_act <- 1-sobrevivencia[[i]][,5]-sobrevivencia[[i]][,7]
}

for(i in 68:97){
  sobrevivencia[[i]] <- as.data.frame(Mortalidad[Mortalidad$ynac == 1934+i,])
  sobrevivencia[[i]]$ix <- Invalidez$ix
  sobrevivencia[[i]]$r00_pen <- 0.1*sobrevivencia[[i]]$px*(1-sobrevivencia[[i]]$ix)
  sobrevivencia[[i]]$r01_pen <- 1-sobrevivencia[[i]][,5]-sobrevivencia[[i]][,7]-sobrevivencia[[i]][,8]
  sobrevivencia[[i]]$r00_act <- 1-sobrevivencia[[i]][,5]-sobrevivencia[[i]][,7]
}

rm(Mortalidad,Invalidez,i)
# Poblaciones
Activos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Activos_ciclo.csv")
Inactivo_180<- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Inactivo_180.csv")
Inactivo_180_mas48 <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Inactivo_menos180_mas48.csv")
Inactivo_180_menos48 <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Inactivo_menos180_menor48.csv")
Pensionados_huerfanos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_huerfanos_vigente.csv")
Pensionados_invalidez <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_Invalidez.csv")
Pensionados_vejez <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_vejez.csv")
Pensionados_viudos <- read.csv("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Proyecciones/Bases separadas/Pensionados_viudos.csv")

## Agregar variable 
Inactivo_180_menos48$num_cuotas2022<-rowSums(Inactivo_180_menos48[,c(342:353)]>0)
Inactivo_180_mas48$Cuotas3anios<-rowSums(Inactivo_180_mas48[,c(330:353)]>0)
Inactivo_180_mas48$Cuotas4anios<-rowSums(Inactivo_180_mas48[,c(318:353)]>0)
Inactivo_180_mas48$Cuotas2anios<-rowSums(Inactivo_180_mas48[,c(342:353)]>0)


# Funcion de orfandad
Sim_orfandad <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(25-edad)
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(edad+1):25]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  if( !is.na(anio_muerte)){
    resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])
  }else{resultado[(momento-2022):(momento-2022+24-edad)] <- rep(1, 25-edad)}
  return(resultado)
}

# Momento debe ser el año en el que se está calculando la vida

Sim_viudez <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(116-edad)
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(aux):length(sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo])]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  resultado[(momento-2022):(momento-2022+anio_muerte-1)] <- rep(1, anio_muerte)
  return(resultado)
}

Sim_invalidez <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(116-edad)
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(aux):length(sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo])]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])
  
  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
  orfandad <- c(ID, rep(NA, 97))
  viudez <- c(ID, rep(NA, 97))
  if(edad+anio_muerte < 50 & edad + anio_muerte >= 25){
    sexo3 <- round(runif(1,1,2))
    orfandad <- Sim_orfandad(edad+anio_muerte-25, sexo3, ID, momento + anio_muerte)
  }
  if(edad+anio_muerte < 116){
    sexo2 <- 3 - sexo
    viudez <- Sim_viudez(edad+anio_muerte, sexo2, ID, anio_muerte+momento)
  }
  resultado <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
                      ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
                             ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                    resultado)))
  resultado<-list(resultado, edad+anio_muerte)
  names(resultado)<-c("resultado", "edad_viudez")
  return(resultado)
}

Sim_vejez <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(116-edad)
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(aux):length(sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo])]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])
  
  # Estados: 1-Pensionado vivo, 2-Viudez
  viudez <- c(ID, rep(NA, 97))
  if(edad+anio_muerte < 116){
    sexo2 <- 3 - sexo
    viudez <- Sim_viudez(edad+anio_muerte, sexo2, ID, anio_muerte+momento)
  }
  
  resultado <- ifelse(!is.na(viudez) & viudez==1, 2, resultado)
  resultado<-list(resultado, edad+anio_muerte)
  names(resultado)<-c("resultado", "edad_viudez")
  return(resultado)
}

Sim_inactivo_menos180_menor48 <- function(edad, sexo, ID, momento, cuotas2022, cuotas){
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  ix <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  r00_act <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  aleatorio <- runif(1)
  temp1<-seq(12,120, 4)
  temp1<-c(rep(temp1[1], 6), temp1, rep(temp1[length(temp1)], 98-(length(temp1)+6)))

  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad

  if(aleatorio > r00_act & aleatorio <= r00_act + ix){
    if(cuotas2022==12 & cuotas>=temp1[edad-17]){
      temp<-Sim_invalidez(edad+1,sexo, ID, momento)
      resultado <- c(temp$resultado, temp$edad_viudez, cuotas)
    }else{
      resultado <- c(ID, rep(NA, 97), 0, cuotas)
    }

  }else{
      # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
    print("perra estoy entrando al ciclo este, porque me morí")
    if(cuotas2022==12){
      ### retorna un huerfanito y una viuda
      resultado <- c(ID, rep(NA, 97))
      orfandad <- c(ID, rep(NA, 97))
      viudez <- c(ID, rep(NA, 97))
      if(edad+1 < 50 & edad >= 25){
        sexo3 <- round(runif(1,1,2))
        orfandad <- Sim_orfandad(edad+1-25, sexo3, ID, momento + 1)
      }
      if(edad+1 < 116){
        sexo2 <- 3 - sexo
        viudez <- Sim_viudez(edad+1, sexo2, ID, 1+momento)
      }
      resultado <- c(ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
                          ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
                                 ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                        resultado))), edad+1, cuotas)
      
    

    }else{ resultado <- c(ID, rep(NA, 97), 0, cuotas)}

  }
  return(resultado)
}


Sim_inactivo_menos180_mayor48 <- function(edad, sexo, ID, momento, cuotas24, cuotas12, cuotas36,cuotas){
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  ix <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  r00_act <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  qx <- sobrevivencia[[momento-edad-1934]]$qx[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  ix1 <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  r00_act_1 <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  qx1 <- sobrevivencia[[momento-edad-1934]]$qx[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  temp1<-seq(12,120, 4)
  temp1<-c(rep(temp1[1], 6), temp1, rep(temp1[length(temp1)], 98-(length(temp1)+6)))
  
  
  aleatorio <- runif(2)
  
  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
  
  if(aleatorio[1] <= r00_act){
    
    if(aleatorio[2] <= r00_act_1){
      ## Se mantiene vivo, no pasa ni pinga
      resultado <- c(ID, rep(NA,97), 0, cuotas)
    }
    
    if(r00_act_1<aleatorio[2] & aleatorio[2] <= r00_act_1 + ix1){
      ## Se invalida
      if(cuotas24==24 & cuotas>=temp1[edad-17]){
        temp<-Sim_invalidez(edad+2, sexo, ID, momento+2)
        resultado <- c(temp$resultado, temp$edad_viudez, cuotas)
      }else{
        resultado <- c(ID, rep(NA,97), 0, cuotas)
      }
      
    }
    
    if(r00_act_1 + ix1 < aleatorio[2] & aleatorio[2] <= 1){
      ## la palmo, confirmar si efectivamente se jode
      resultado <- c(ID, rep(NA,97), 0, cuotas)
      ## agregar vector de te jodiste puta
      # resultado <- c(ID, rep(NA, 97))
      # orfandad <- c(ID, rep(NA, 97))
      # viudez <- c(ID, rep(NA, 97))
      # if(edad+2 < 50){
      #   ## huerfanito
      #   sexo3 <- round(runif(1,1,2))
      #   orfandad <- Sim_orfandad(edad+2-25, sexo3, ID, momento + 2)
      # }
      # if(edad+2 < 116){
      #   ## viuda
      #   sexo2 <- 3 - sexo
      #   viudez <- Sim_viudez(edad+2, sexo2, ID, 2+momento)
      # }
      # resultado <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
      #                     ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
      #                            ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
      #                                   resultado)))
      
    }
    
  }
  ## Invalido primer anio
  if(r00_act < aleatorio[1] & aleatorio[1] <= r00_act+ix){
    if(cuotas36>=24 & cuotas>=temp1[edad-17]){
      temp<-Sim_invalidez(edad+1, sexo, ID, momento +1)
      resultado <- c(temp$resultado, temp$edad_viudez, cuotas)
      
    }else{
      resultado <- c(ID, rep(NA,97),0,cuotas)
      ## saquese a la verga de mi fondo
    }
    
  }
  ## piruja la palma primer anio
  if(r00_act+ix < aleatorio[1] & aleatorio[1] <= 1){
    
    if(cuotas12==12){
      # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
      resultado <- c(ID, rep(NA, 97))
      orfandad <- c(ID, rep(NA, 97))
      viudez <- c(ID, rep(NA, 97))
      if(edad+1 < 50 & edad >= 25){
        sexo3 <- round(runif(1,1,2))
        orfandad <- Sim_orfandad(edad+1-25, sexo3, ID, momento + 1)
      }
      if(edad+1 < 116){
        sexo2 <- 3 - sexo
        viudez <- Sim_viudez(edad+1, sexo2, ID, 1+momento)
      }
      resultado <- c(ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
                          ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
                                 ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                        resultado))), edad+1, cuotas)
      
    }else{
      resultado <- c(ID, rep(NA,97),0, cuotas)
      ## saquese a la verga de mi fondo
    }
   
    
  }
  
  return(resultado)
}



Sim_inactivo180 <- function(edad, sexo, ID, momento, cuotas) {
  
  # Estados:
  # 0 - Sigue vivo
  # 1 - Pensionado por invalidez
  # 2 - Viudez
  # 3 - Viudez y orfandad
  # 4 - Orfandad
  # 5 - Pensionado por vejez
  
  resultado <- c(ID)
  temp1 <- c(rep(12, 6), seq(12, 120, 4), rep(120, 98 - (length(seq(12, 120, 4)) + 6)))
  while (edad < 115) {
    momento_actual <- momento - edad - 1934
    aux <- which(sobrevivencia[[momento_actual]]$edad[sobrevivencia[[momento_actual]]$sex == sexo] == edad)
    ix <- sobrevivencia[[momento_actual]]$ix[sobrevivencia[[momento_actual]]$sex == sexo][aux]
    r00_act <- sobrevivencia[[momento_actual]]$r00_act[sobrevivencia[[momento_actual]]$sex == sexo][aux]
    aleatorio <- runif(1)
    
    #print(cuotas)
      if (r00_act >= aleatorio) {
        # sigue vivo este hijueputa
        if(edad>=65 & cuotas>=300){
          # se pensiona
          aux1 <- Sim_vejez(edad, sexo, ID, momento)
          aux1$resultado<-aux1$resultado[(momento-2022):98]
          aux1$resultado[aux1$resultado==1] <- 5
          resultado <- c(resultado, aux1$resultado, aux1$edad_viudez,cuotas)
          break
        }else{
          edad <- edad + 1
          momento <- momento + 1
          resultado <- c(resultado, 0)
        }
      } else if (r00_act + ix >= aleatorio) {
        print("me invalide putas")
          aux2 <- Sim_invalidez(edad, sexo, ID, momento)
          aux2$resultado<-aux2$resultado[(momento - 2022):98]
          aux2$resultado[aux2$resultado==2] <- 3
          aux2$resultado[aux2$resultado==3] <- 2
          resultado <- c(resultado, aux2$resultado, aux2$edad_viudez, cuotas)
          break
          #       print("Quedó inválido")
      } else {
        viudez <- c(ID, rep(NA, 97))
        orfandad <- c(ID, rep(NA, 97))
        if (edad + 1 < 116) {
          sexo2 <- 3 - sexo
          viudez <- Sim_viudez(edad, sexo2, ID, momento)
          #         viudez[viudez==1] <- 2
        }
        if(edad<50 & edad >26){
          sexo2 <- round(runif(1))
          orfandad <- Sim_orfandad(edad-25, sexo2, ID, momento)
          #         orfandad[orfandad==1] <- 4
        }
        #print("El codigo esta mal hecho putas no entro a ninguno")
        aux3<-c(resultado, rep(NA,(98-length(resultado))))
        aux3 <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 3,
                       ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 2,
                              ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                     aux3)))
       
        
        resultado <- c(aux3, edad, cuotas)
        #       print("Murió")
        break
      }
  }
  return(resultado)
}

Densidad<-read_excel("C:/Users/Ana/Desktop/Universidad/I-2024/Pensiones/Metodologia/Densidad.xlsx")
Densidad<-unname(unlist(Densidad))
Densidad<-12*Densidad


Sim_activo<-function(edad, sexo, ID, momento, cuotas) {
  
  # Estados:
  # 0 - Cotizando
  # 1 - Pensionado por invalidez
  # 2 - Viudez
  # 3 - Viudez y orfandad
  # 4 - Orfandad
  # 5 - Pensionado por vejez
  
  resultado <- c(ID)
  cuotas_pen<-cuotas
  cuotas_act<-cuotas
  temp1 <- c(rep(12, 6), seq(12, 120, 4), rep(120, 98 - (length(seq(12, 120, 4)) + 6)))
  while (edad < 115) {
    momento_actual <- momento - edad - 1934
    aux <- which(sobrevivencia[[momento_actual]]$edad[sobrevivencia[[momento_actual]]$sex == sexo] == edad)
    
    ix <- sobrevivencia[[momento_actual]]$ix[sobrevivencia[[momento_actual]]$sex == sexo][aux]
    r00_act <- sobrevivencia[[momento_actual]]$r00_act[sobrevivencia[[momento_actual]]$sex == sexo][aux]
    aleatorio <- runif(1)
    
    if (edad < 65) {
      if (r00_act >= aleatorio) {
        
        edad <- edad + 1
        momento <- momento + 1
        # engrendo cotiza
        cuotas_act<-cuotas_act+Densidad[edad-17]
        resultado <- c(resultado, 0)
      } else if (r00_act + ix >= aleatorio) {
        #print("perra el problema es cuando se invalida")
        if(cuotas_act>=180){
          aux2 <- Sim_invalidez(edad, sexo, ID, momento)
          aux2$resultado<-aux2$resultado[(momento - 2022):98]
          aux2$resultado[aux2$resultado==2] <- 3
          aux2$resultado[aux2$resultado==3] <- 2
          resultado <- c(resultado, aux2$resultado, aux2$edad_viudez,cuotas_act)
          #       print("Quedó inválido")
          break
        }else if(cuotas_act>=temp1[edad-17]){
          # aux2 <- Sim_invalidez(edad, sexo, ID, momento)[(momento - 2022):98]
          # aux2[aux2==2] <- 3
          # aux2[aux2==3] <- 2
          # resultado <- c(resultado, aux2)
          aux2 <- Sim_invalidez(edad, sexo, ID, momento)
          aux2$resultado<-aux2$resultado[(momento - 2022):98]
          aux2$resultado[aux2$resultado==2] <- 3
          aux2$resultado[aux2$resultado==3] <- 2
          resultado <- c(resultado, aux2$resultado, aux2$edad_viudez,cuotas_act)
          break
        }else{

          resultado <- c(ID, rep(NA, 97), 0, cuotas_act)
          break
          ## Copiar las pingas que cotizo y NA, no deja pinga
          ## Liquidado, ni pinga que hacer
          
        }
        
      } else {
        
        viudez <- c(ID, rep(NA, 97))
        orfandad <- c(ID, rep(NA, 97))
        if (edad + 1 < 116) {
          sexo2 <- 3 - sexo
          viudez <- Sim_viudez(edad, sexo2, ID, momento)
          #         viudez[viudez==1] <- 2
        }
        if(edad<50 & edad >26){
          sexo2 <- round(runif(1))
          orfandad <- Sim_orfandad(edad-25, sexo2, ID, momento)
          #         orfandad[orfandad==1] <- 4
        }
        
        aux3<-c(resultado, rep(NA,(98-length(resultado))))
        
        aux3 <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 3,
                       ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 2,
                              ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                     aux3)))
        
        resultado <- c(aux3, edad, cuotas_act)
        #       print("Murió")
        #print(paste("Resultado despues de simular viudas menos 65:", length(resultado)))
        break
      }
      
    }else {
      r00_pen <- sobrevivencia[[momento_actual]]$r00_pen[sobrevivencia[[momento_actual]]$sex == sexo][aux]
      r01_pen <- sobrevivencia[[momento_actual]]$r01_pen[sobrevivencia[[momento_actual]]$sex == sexo][aux]
      
      if (r00_pen >= aleatorio) {
        edad <- edad + 1
        momento <- momento + 1
        cuotas_pen<-cuotas_pen+Densidad[edad-17]
        #        print("Se mantiene trabajando")
        resultado <- c(resultado, 0)
      } else if (r00_pen + r01_pen >= aleatorio) {
       
        if(cuotas_pen<300){
          edad <- edad + 1
          momento <- momento + 1
          cuotas_pen<-cuotas_pen+Densidad[edad-17]
          resultado <- c(resultado, 0)
        }else{
          aux1 <- Sim_vejez(edad, sexo, ID, momento)
          aux1$resultado<-aux1$resultado[(momento-2022):98]
          aux1$resultado[aux1$resultado==1] <- 5
          resultado <- c(resultado, aux1$resultado, aux1$edad_viudez, cuotas_pen)
          break 
        }
        
        #        print("Se pensionó")
        
      } else if (r00_pen + r01_pen + ix >= aleatorio) {
        if(cuotas_pen>=300){# Si tiene opcion de vejez, no se mama
          # aux1 <- Sim_vejez(edad, sexo, ID, momento)[(momento-2022):98]
          # aux1[aux1==1] <- 5
          # resultado <- c(resultado, aux1)
          aux1 <- Sim_vejez(edad, sexo, ID, momento)
          aux1$resultado<-aux1$resultado[(momento-2022):98]
          aux1$resultado[aux1$resultado==1] <- 5
          resultado <- c(resultado, aux1$resultado, aux1$edad_viudez, cuotas_pen)
          break
          # temp <- paste("Estoy aqui, rompi el ciclo", cuotas_pen)
          # cat(temp)
        }else if(cuotas_pen>=120){
          aux2 <- Sim_invalidez(edad, sexo, ID, momento)
          aux2$resultado<-aux2$resultado[(momento - 2022):98]
          aux2$resultado[aux2$resultado==2] <- 3
          aux2$resultado[aux2$resultado==3] <- 2
          resultado <- c(resultado, aux2$resultado, aux2$edad_viudez,cuotas_pen)
          # aux2 <- Sim_invalidez(edad, sexo, ID, momento)[(momento - 2022):98]
          # aux2[aux2==2] <- 3
          # aux2[aux2==3] <- 2
          # resultado <- c(resultado, aux2)
          break
        }else{
          resultado <- c(ID, rep(NA, 97),0,cuotas_pen)
          #print("llegue aqui por eso se rompe mas de 65")
          ## C mamo
          break
        }
        ## Todos los mayores a 65 anios invalidos gozan de este beneficio dado que en el 2023 todos cotizaron al menos 2
        
      }else{
        
        viudez <- c(ID, rep(NA, 97))
        
        if (edad + 1 < 116) {
          sexo2 <- 3 - sexo
          viudez <- Sim_viudez(edad, sexo2, ID, momento)
          viudez[viudez==1] <- 2
        }
        
        resultado <- c(resultado,viudez[(momento-2022):98], edad, cuotas_pen)
        #       print("Murió")
        break
      }
    }
  }
  
  return(resultado)
  
  
}




######### SIMULACIONES
tic()
#### Los mal nacidos huerfanos

huerfanitos <- data.frame(Edad<- Pensionados_huerfanos$Edad, Sexo <- Pensionados_huerfanos$SEXO, ID <- Pensionados_huerfanos$ID_Pensionado, Momento <- 2024)
colnames(huerfanitos) <- c("Edad", "Sexo", "ID", "Momento")
huerfanitos <- huerfanitos %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

orfandad <- data.frame(matrix(nrow=nrow(huerfanitos), ncol = 98))
colnames(orfandad) <- c("ID", 2024:(2024+96))

for (a in 1:nrow(huerfanitos)) {
  orfandad[a,] <- as.integer(Sim_orfandad(huerfanitos[a,1],
                                          huerfanitos[a,2],
                                          0,
                                          huerfanitos[a,4]))
  orfandad[a,1] = huerfanitos[a,3]
}
rm(huerfanitos)

#### Los insufribles viudos

viudx <- data.frame(Edad<- Pensionados_viudos$Edad, Sexo <- Pensionados_viudos$SEXO, ID <- Pensionados_viudos$ID_Pensionado, Momento <- 2024)
colnames(viudx) <- c("Edad", "Sexo", "ID", "Momento")
viudx <- viudx %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

viudez <- data.frame(matrix(nrow=nrow(viudx), ncol = 98))
colnames(viudez) <- c("ID", 2024:(2024+96))

for (i in 1:nrow(viudx)) {
  viudez[i,] <- as.integer(Sim_viudez(viudx[i,1], viudx[i,2],
                                      0, viudx[i,4]))
  viudez[i,1] = viudx[i,3]
}
rm(viudx,i)

#### Los viejos 

viejos <- data.frame(Edad<- Pensionados_vejez$Edad, Sexo <- Pensionados_vejez$SEXO, ID <- Pensionados_vejez$ID_Pensionado, Momento <- 2024)
colnames(viejos) <- c("Edad", "Sexo", "ID", "Momento")
viejos <- viejos %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))
viejos$Sexo <- as.integer(viejos$Sexo)
vejez <- data.frame(matrix(nrow=nrow(viejos), ncol = 98))
colnames(vejez) <- c("ID", 2024:(2024+96))

for (i in 1:nrow(viejos)) {
  vejez[i,] <- as.integer(Sim_vejez(viejos[i,1], viejos[i,2],
                                    0, viejos[i,4]))
  vejez[i,1] = viejos[i,3]
}
rm(viejos,i)


#### Los invalidos

invalidos <- data.frame(Edad<- Pensionados_invalidez$Edad, Sexo <- Pensionados_invalidez$SEXO, ID <- Pensionados_invalidez$ID_Pensionado, Momento <- 2024)
colnames(invalidos) <- c("Edad", "Sexo", "ID", "Momento")
invalidos <- invalidos %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

invalidos$Sexo <- as.integer(invalidos$Sexo)
invalidez <- data.frame(matrix(nrow=nrow(invalidos), ncol = 98))
colnames(invalidez) <- c("ID", 2024:(2024+96))

for (i in 1:nrow(invalidez)) {
  invalidez[i,] <- as.integer(Sim_invalidez(invalidos[i,1], invalidos[i,2],
                                            0, invalidos[i,4]))
  invalidez[i,1] = invalidos[i,3]
}
rm(invalidos,i)
### INACTIVOS MAYORES DE 48 CON -180 CUOTAS
inactivos_mas48 <- data.frame(Edad<- Inactivo_180_mas48$Edad, Sexo <- Inactivo_180_mas48$Sexo, ID <- Inactivo_180_mas48$ID,
                              Momento <- 2024, Cuotas3anios<-Inactivo_180_mas48$Cuotas3anios,Cuotas2anios<-Inactivo_180_mas48$Cuotas2anios, 
                              Cuotas4anios<-Inactivo_180_mas48$Cuotas4anios, Cuotas<-Inactivo_180_mas48$num_cuotas)
colnames(inactivos_mas48) <- c("Edad", "Sexo", "ID", "Momento","Cuotas3anios", "Cuotas2anios","Cuotas4anios","Cuotas")
inactivos_mas48 <- inactivos_mas48 %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

inactivos_mas48$Sexo <- as.integer(inactivos_mas48$Sexo)
inactivos_mas48_sim <- data.frame(matrix(nrow=nrow(inactivos_mas48), ncol = 98))
colnames(inactivos_mas48_sim) <- c("ID", 2024:(2024+96))
tic()
for (i in 1:nrow(inactivos_mas48_sim)) {
  inactivos_mas48_sim[i,] <- as.integer(Sim_inactivo_menos180_mayor48(inactivos_mas48[i,1], inactivos_mas48[i,2],
                                                                      0, inactivos_mas48[i,4],inactivos_mas48[i,5],
                                                                      inactivos_mas48[i,6], inactivos_mas48[i,7],
                                                                      inactivos_mas48[i,8]))
  inactivos_mas48_sim[i,1] <- inactivos_mas48[i,3]
}
rm(inactivos_mas48,i)
toc()
### INACTIVOS MENORES DE 48 CON -180 CUOTAS

inactivos_menos48 <- data.frame(Edad<- Inactivo_180_menos48$Edad, Sexo <- Inactivo_180_menos48$Sexo, ID <- Inactivo_180_menos48$ID, Momento <- 2024,
                                Cuotas2022<-Inactivo_180_menos48$num_cuotas2022, Inactivo_180_menos48$num_cuotas)
colnames(inactivos_menos48) <- c("Edad", "Sexo", "ID", "Momento", "Cuotas2022", "Cuotas")
inactivos_menos48 <- inactivos_menos48 %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

inactivos_menos48$Sexo <- as.integer(inactivos_menos48$Sexo)
inactivos_menos48_sim <- data.frame(matrix(nrow=nrow(inactivos_menos48), ncol = 100))
colnames(inactivos_menos48_sim) <- c("ID", 2024:(2024+96), "Edad_viudez", "Cuotas")

for (i in 1:nrow(inactivos_menos48)) {
  print(i)
  inactivos_menos48_sim[i,] <- as.integer(Sim_inactivo_menos180_menor48(inactivos_menos48[i,1], inactivos_menos48[i,2],
                                                                        0, inactivos_menos48[i,4], inactivos_menos48[i,5], inactivos_menos48[i,6]))
  inactivos_menos48_sim[i,1] <- inactivos_menos48[i,3]
}
rm(inactivos_menos48,i)


### INACTIVOS 180
tic()
inactivos_180 <- data.frame(Edad<- Inactivo_180$Edad, Sexo <- Inactivo_180$Sexo, ID <- Inactivo_180$ID, Momento <- 2024, Cuotas<-Inactivo_180$num_cuotas)
colnames(inactivos_180) <- c("Edad", "Sexo", "ID", "Momento", "Cuotas")
inactivos_180 <- inactivos_180 %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

inactivos_180$Sexo <- as.integer(inactivos_180$Sexo)
inactivos_180_sim <- data.frame(matrix(nrow=nrow(inactivos_180), ncol = 100))
colnames(inactivos_180_sim) <- c("ID", 2024:(2024+96))

for (i in 1:nrow(inactivos_180_sim)) {
  print(i)
  inactivos_180_sim[i,] <- as.integer(Sim_inactivo180(inactivos_180[i,1], inactivos_180[i,2],
                                                                        0, inactivos_180[i,4], inactivos_180[i,5]))
  inactivos_180_sim[i,1] <- inactivos_180[i,3]
}
toc()
### ACTIVOS
tic()
activos <- data.frame(Edad<- Activos$Edad, Sexo <- Activos$Sexo, ID <- Activos$ID, Momento <- 2024, Cuotas<-Activos$num_cuotas)
colnames(activos) <- c("Edad", "Sexo", "ID", "Momento", "Cuotas")
activos <- activos %>%
  mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))

activos$Sexo <- as.integer(activos$Sexo)
activos_sim <- data.frame(matrix(nrow=nrow(activos), ncol = 100))
colnames(activos_sim) <- c("ID", 2024:(2024+96), "Edad_viudez", "cuotas")
toc()
# tic()
# for (i in 1:nrow(activos_sim)) { 
#   print(i)
#   activos_sim[i,] <- as.integer(Sim_activo(activos[i,1], activos[i,2],
#                                            0, activos[i,4], activos[i,5]))
#   activos_sim[i,1] <- activos[i,3]
# }
# rm(activos,i)
# toc()

tic()
activos_sim<-t(sapply(1:nrow(Activos), function(i) Sim_activo(activos[i,1], activos[i,2],
                                   0, activos[i,4], activos[i,5])))
activos_sim<-as.data.frame(activos_sim)[,-1]
activos_sim<-cbind(activos[,3],activos_sim)
colnames(activos_sim) <- c("ID", 2024:(2024+96), "Edad_viudez", "cuotas")
toc()

















