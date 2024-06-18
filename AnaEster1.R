## El fondo dura 97 años más
## Funciones para programar:

# Invalidez [SI]
# Viudez [SI]
# Orfandad [SI]
# Vejez [SI]
# Activo
# Inactivo

# Probabilidades de transición
Mortalidad <- read_excel("Desktop/CA0412 Pensiones I/Codigo/transicion.xls", 
                         sheet = "Mortalidad")
Mortalidad <- Mortalidad[,-c(5,7)]
Mortalidad$sex <- as.numeric(Mortalidad$sex)
Mortalidad$edad <- as.numeric(Mortalidad$edad)
Mortalidad$ynac <- as.numeric(Mortalidad$ynac)
Mortalidad$year <- as.numeric(Mortalidad$year)
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

# Poblaciones
Activos <- read.csv("~/Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Activos3.csv")
Inactivo_180 <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Inactivo_180.csv")
Inactivo_menos180 <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Inactivo_menos180.csv")
Pensionados_huerfanos <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_huerfanos.csv")
Pensionados_invalidez <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_Invalidez.csv")
Pensionados_vejez <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_vejez.csv")
Pensionados_viudos <- read_csv("Desktop/CA0412 Pensiones I/Codigo/Bases separadas/Pensionados_viudos.csv")


# Funcion de orfandad
Sim_orfandad <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(26-edad)
  aux<-which(sobrevivencia[[momento-edad-1934]]$edad[sobrevivencia[[momento-edad-1934]]$sex == sexo]==edad)
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(aux):length(sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo])]
  anio_muerte <- which(aleatorio>probabilidades)
  if(length(anio_muerte) > 0){
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
  #anio_muerte<-3
  resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])

  # Estados: 1-Pensionado vivo, 2-Viudez y orfandad, 3-Solo viudez, 4-Solo orfandad
  orfandad <- c(ID, rep(NA, 97))
  viudez <- c(ID, rep(NA, 97))
  print(edad+anio_muerte<50)
  if(edad+anio_muerte < 50){
    sexo3 <- round(runif(1,1,2))
    orfandad <- Sim_orfandad(edad+anio_muerte-25, sexo3, ID, momento + anio_muerte)
  }
  if(round(runif(1))==1 & edad+anio_muerte < 116){
    sexo2 <- 3 - sexo
    viuda <- Sim_viudez(edad+anio_muerte, sexo2, ID, anio_muerte+momento)
  }
  print(viuda)
  print(orfandad)
  resultado <- ifelse(!is.na(orfandad) & !is.na(viuda) & orfandad == 1 & viuda == 1, 2,
                      ifelse(!is.na(viuda) & viuda == 1 & (is.na(orfandad) | orfandad != 1), 3,
                             ifelse(!is.na(orfandad) & orfandad == 1 & (is.na(viuda) | viuda != 1), 4,
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
  if(runif(1)< 0.5 & edad+anio_muerte < 116){
    sexo2 <- 3 - sexo
    print(edad+anio_muerte)
    viudez <- Sim_viudez(edad+anio_muerte, sexo2, ID, anio_muerte+momento)
  }
  
  resultado <- ifelse(!is.na(viudez) & viudez==1, 2, resultado)
  
  return(resultado)
}

