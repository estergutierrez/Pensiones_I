## El fondo dura 97 años más
## Funciones para programar:

# Invalidez
# Viudez [SI]
# Orfandad [SI]
# Vejez
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
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(edad+1):116][(edad+1):26]
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
  probabilidades <- sobrevivencia[[momento-edad-1934]]$px[sobrevivencia[[momento-edad-1934]]$sex == sexo][(edad+1):116]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])
  return(resultado)
}

Sim_invalidez <- function(edad, sexo, ID, momento){
  resultado <- c(ID, rep(NA, 97))
  aleatorio <- runif(116-edad)
  probabilidades <- sobrevivencia$px[sobrevivencia$ynac == momento-edad & sobrevivencia$sex == sexo][(edad+1):116]
  anio_muerte <- which(aleatorio>probabilidades)[1]
  
  # FALTA CALCULAR VIUDXS Y HUERFANXS
  resultado[(momento-2022):(momento-2022+anio_muerte[1]-1)] <- rep(1, anio_muerte[1])
}

