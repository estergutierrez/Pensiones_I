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

# Probabilidades de transición
Mortalidad <- read_excel("Desktop/CA0412 Pensiones I/Codigo/transicion.xls", 
                         sheet = "Mortalidad")
Mortalidad <- Mortalidad[,-c(5,7)]
Mortalidad$sex <- as.integer(Mortalidad$sex)
Mortalidad$edad <- as.integer(Mortalidad$edad)
Mortalidad$ynac <- as.integer(Mortalidad$ynac)
Mortalidad$year <- as.integer(Mortalidad$year)
Mortalidad$qx <- as.numeric(Mortalidad$qx)
Mortalidad$px <- 1 - Mortalidad$qx
Invalidez <- read_excel("Desktop/CA0412 Pensiones I/Codigo/transicion.xls", 
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

rm(Mortalidad,Invalidez)
# Poblaciones
Activos <- read.csv("~/Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Activos_ciclo.csv")
Inactivo_180_mas48 <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Inactivo_menos180_mas48.csv")
Inactivo_180_menos48 <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Inactivo_menos180_menor48.csv")
Pensionados_huerfanos <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_huerfanos_vigente.csv")
Pensionados_invalidez <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_Invalidez.csv")
Pensionados_vejez <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_vejez.csv")
Pensionados_viudos <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_viudos.csv")


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
  
  return(resultado)
}

Sim_inactivo_menos180_menor48 <- function(edad, sexo, ID, momento){
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  ix <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  r00_act <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  aleatorio <- runif(1)
  
  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
  
  if(aleatorio > r00_act & aleatorio <= r00_act + ix){
    resultado <- Sim_invalidez(edad+1,sexo, ID, momento)
  }else{
    resultado <- c(ID, rep(NA, 97))
  }
  
  return(resultado)
}

Sim_inactivo_menos180_mayor48 <- function(edad, sexo, ID, momento){
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  ix <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  r00_act <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  qx <- sobrevivencia[[momento-edad-1934]]$qx[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux]
  ix1 <- sobrevivencia[[momento-edad-1934]]$ix[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  r00_act_1 <- sobrevivencia[[momento-edad-1934]]$r00_act[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  qx1 <- sobrevivencia[[momento-edad-1934]]$qx[sobrevivencia[[momento-edad-1934]]$sex == sexo][aux+1]
  
  aleatorio <- runif(2)
  
  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
  
  if(aleatorio[1] <= r00_act){
    
    if(aleatorio[2] <= r00_act_1){
      resultado <- c(ID, rep(NA,97))
    }
    
    if(r00_act_1<aleatorio[2] & aleatorio[2] <= r00_act_1 + ix1){
      resultado <- Sim_invalidez(edad+2, sexo, ID, momento+2)
    }
    
    if(r00_act_1 + ix1 < aleatorio[2] & aleatorio[2] <= 1){
      resultado <- c(ID, rep(NA, 97))
      orfandad <- c(ID, rep(NA, 97))
      viudez <- c(ID, rep(NA, 97))
      if(edad+2 < 50){
        sexo3 <- round(runif(1,1,2))
        orfandad <- Sim_orfandad(edad+2-25, sexo3, ID, momento + 2)
      }
      if(edad+2 < 116){
        sexo2 <- 3 - sexo
        viudez <- Sim_viudez(edad+2, sexo2, ID, 2+momento)
      }
      resultado <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
                          ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
                                 ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                        resultado)))
      
    }
    
  }
  
  if(r00_act < aleatorio[1] & aleatorio[1] <= r00_act+ix){
    resultado <- Sim_invalidez(edad+1, sexo, ID, momento +1)
  }
  
  if(r00_act+ix < aleatorio[1] & aleatorio[1] <= 1){
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
    resultado <- ifelse(!is.na(orfandad) & !is.na(viudez) & orfandad == 1 & viudez == 1, 2,
                        ifelse(!is.na(viudez) & viudez == 1 & (is.na(orfandad) | orfandad != 1), 3,
                               ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viudez) | viudez != 1), 4,
                                      resultado)))
    
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
  ### INACTIVOS MAYORES DE 48 CON +180 CUOTAS
  inactivos_mas48 <- data.frame(Edad<- Inactivo_180_mas48$Edad, Sexo <- Inactivo_180_mas48$Sexo, ID <- Inactivo_180_mas48$ID, Momento <- 2024)
  colnames(inactivos_mas48) <- c("Edad", "Sexo", "ID", "Momento")
  inactivos_mas48 <- inactivos_mas48 %>%
    mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))
  
  inactivos_mas48$Sexo <- as.integer(inactivos_mas48$Sexo)
  inactivos_mas48_sim <- data.frame(matrix(nrow=nrow(inactivos_mas48), ncol = 98))
  colnames(inactivos_mas48_sim) <- c("ID", 2024:(2024+96))
  
  for (i in 1:nrow(inactivos_mas48_sim)) {
    inactivos_mas48_sim[i,] <- as.integer(Sim_inactivo_menos180_mayor48(inactivos_mas48[i,1], inactivos_mas48[i,2],
                                                                        0, inactivos_mas48[i,4]))
    inactivos_mas48_sim[i,1] <- inactivos_mas48[i,3]
  }
  rm(inactivos_mas48,i)
  
  ### INACTIVOS MENORES DE 48 CON +180 CUOTAS
  
  inactivos_menos48 <- data.frame(Edad<- Inactivo_180_menos48$Edad, Sexo <- Inactivo_180_menos48$Sexo, ID <- Inactivo_180_menos48$ID, Momento <- 2024)
  colnames(inactivos_menos48) <- c("Edad", "Sexo", "ID", "Momento")
  inactivos_menos48 <- inactivos_menos48 %>%
    mutate(Sexo = ifelse(Sexo == "M", 1, ifelse(Sexo == "F", 2, Sexo)))
  
  inactivos_menos48$Sexo <- as.integer(inactivos_menos48$Sexo)
  inactivos_menos48_sim <- data.frame(matrix(nrow=nrow(inactivos_menos48), ncol = 98))
  colnames(inactivos_menos48_sim) <- c("ID", 2024:(2024+96))
  
  for (i in 1:nrow(inactivos_menos48_sim)) {
    inactivos_menos48_sim[i,] <- as.integer(Sim_inactivo_menos180_menor48(inactivos_menos48[i,1], inactivos_menos48[i,2],
                                                                          0, inactivos_menos48[i,4]))
    inactivos_menos48_sim[i,1] <- inactivos_menos48[i,3]
  }
  rm(inactivos_menos48,i)

  toc()