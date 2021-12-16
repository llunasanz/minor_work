# Librerías necesarias para el funcionamiento del código.
library(zoo)
library(xts)
library(TTR)
library(quantmod)

# Lista de símbolos bursátiles de títulos pertenecientes al mercado contínuo español.
tickers.IBEX <- c('ABG.MC', 'ANA.MC', 'ACX.MC', 'ACS.MC', 'ADZ.MC', 'AEDAS.MC', 'AENA.MC', 'AIR.MC', 'AI.MC', 'ALNT.MC', 'ALM.MC', 'AMS.MC', 'AMP.MC', 'EAT.MC', 'APAM.MC', 'APPS.MC', 'MTS.MC', 'ARM.MC', 'A3M.MC', 'ADX.MC', 'AZK.MC', 'BBVA.MC', 'SAB.MC', 'SAN.MC', 'BKIA.MC', 'BKT.MC', 'BDL.MC', 'BKY.MC', 'BIO.MC', 'RIO.MC', 'BME.MC', 'BAIN.MC', 'CABK.MC', 'CLNX.MC', 'CEV.MC', 'CIE.MC', 'CCEP.MC', 'CDR.MC', 'CAF.MC', 'ALB.MC', 'OLE.MC', 'DIA.MC', 'MDF.MC', 'EBRO.MC', 'EDR.MC', 'ENO.MC', 'ENG.MC', 'ENC.MC', 'ELE.MC', 'ECR.MC', 'EKT.MC', 'FAE.MC', 'FCC.MC', 'FER.MC', 'FDR.MC', 'GCO.MC', 'GRF.MC', 'CBAV.MC', 'GSJ.MC', 'EZE.MC', 'IAG.MC', 'IBE.MC', 'IBG.MC', 'ITX.MC', 'IDR.MC', 'COL.MC', 'ISUR.MC', 'ROVI.MC', 'RJF.MC', 'LRE.MC', 'LBK.MC', 'LGT.MC', 'LOG.MC', 'MAP.MC', 'MAS.MC', 'TL5.MC', 'MEL.MC', 'MRL.MC', 'MVC.MC', 'MCM.MC', 'MTB.MC', 'NTGY.MC', 'NTH.MC', 'HOME.MC', 'NHH.MC', 'NEA.MC', 'NXT.MC', 'NYE.MC', 'OHL.MC', 'ORY.MC', 'PVA.MC', 'PHM.MC', 'PRM.MC', 'PRS.MC', 'CASH.MC', 'PSG.MC', 'QBT.MC', 'RLIA.MC', 'REE.MC', 'RDM.MC', 'R4.MC', 'REN.MC', 'REP.MC', 'SCYR.MC', 'SGRE.MC', 'SLR.MC', 'SPK.MC', 'TLGO.MC', 'TRE.MC', 'TEF.MC', 'TUB.MC', 'TRG.MC', 'UNI.MC', 'UBS.MC', 'VER.MC', 'VID.MC', 'VIS.MC', 'VOC.MC', 'ZOT.MC')

# Descarga la información histórica de todas las cotizaciones de la lista del mercado continuo.
  # Los datos van desde el 2007 hasta el día de la descarga (15 de agosto del 2020), con un periodo de un día.
for(i in 1:length(tickers.IBEX)){
  titulo <- getSymbols(tickers.IBEX[i])
}

# Descarga los datos históricos del IBEX 35. Se guardan en la variable IBEX.
  # Los datos van desde el 2007 hasta el día de la descarga (15 de agosto del 2020), con un periodo de un día.
ibex_Datos <- getSymbols('^IBEX',src='yahoo')

# Extración de la cotización histórica y diaria del IBEX al precio de cierre.
IBEX.Clo <- ibex_datos[,4]

# Reemplaza los NAs (valores del tipo null o no disponibles) por valores interpolados.
IBEX.Clo <- na.approx(IBEX.Clo)

# Obtener los datos del IBEX de forma numérica y con un índice distinto al de la fecha para facilitar el trabajo.
IBEX.Clo.n <- coredata(IBEX.Clo)


# Calibración de los parámetros del MACD.
  # Los valores predeterminados son 12, 26 y 9.
macd_fast <- 12
macd_slow <- 29
macd_signal <- 12

# Cálculo del MACD para el IBEX 35.
macd_ibex_extended <- MACD(IBEX.Clo,macd_fast,macd_slow,macd_signal)

# Buscar cruces entre las dos señales del MACD que estén por debajo de 0.2 y con una diferencia inferior de 0,1.
cruces_inf_macd <- which(abs(macd_ibex_extended[,1] - macd_ibex_extended[,2]) < 0.1 & macd_ibex_extended[,1] < 0.2)


## Estrategia del CFD
contador <- 1
rent_IBEX_pre <- 0
rent_IBEX <- 0
cruce_inicio <- TRUE
fechas.rent <- "0"
# Spread del CFD
spread.ibex <- 5
fechas.ibex.CFD <- "0"

for(i in 1:length(IBEX.Clo)){
  if(!is.na(cruces_inf_macd[contador])){ # contador < length(cruces_inf_macd) & length(rent_IBEX_pre) <= contador
    # Si hay cruce del MACD.
    if(cruces_inf_macd[contador] == i){
      # Si es el cruce de inicio de la operación.
      if(cruce_inicio){
        # Se almacena la cotización del IBEX en el momento i.
        rent_IBEX_pre[contador] <- IBEX.Clo[i]
        # Se almacena el valor de la fecha que ha tenido la operación.
        fechas.rent[contador] <- toString(index(IBEX.Clo[i]))
        contador <- contador + 1
        cruce_inicio <- FALSE
      }
      else{
        # En el caso de que la operación ya se inició.
        rent_IBEX_pre[contador] <- IBEX.Clo[i]
        fechas.rent[contador] <- toString(index(IBEX.Clo[i]))
        contador <- contador + 1
      }
    }
    # Si la rentabilidad alcanzada es mayor del 45% o menor del 3%.
    if(rent_IBEX_pre[1] > 0 & !cruce_inicio){
      if(((IBEX.Clo.n[i] / rent_IBEX_pre[length(rent_IBEX_pre)]) > 1.045 & IBEX.Clo.n[i - 1] > IBEX.Clo.n[i]) | (IBEX.Clo.n[i] / rent_IBEX_pre[length(rent_IBEX_pre)] < 0.968)){
        rent_IBEX_pre[contador] <- IBEX.Clo[i]
        fechas.rent[contador] <- toString(index(IBEX.Clo[i]))
        contador <- contador + 1
        cruce_inicio <- TRUE
      }
    }
  }
}

# Se calcula las rentabilidades de cada operación.
for(i in 1:(length(rent_IBEX_pre) %/% 2)){
  rent_IBEX[i] <- (rent_IBEX_pre[2*i] - spread.ibex) / (rent_IBEX_pre[2*i - 1] + spread.ibex)
}

# Rentabilidad promedio por operación.
mean(rent_IBEX)-1

# Mínima rentabilidad del CFD.
min(rent_IBEX)-1

# Número de operacionnes en el CFD.
length(rent_IBEX)


## Estrategia de la cartera
# Obtener las betas en cada cruce inferior del MACD.
  # Las filas son las betas en cada cruce del MACD y las columnas a las titulaciones que corresponde cada beta.
coefic.beta <- matrix(0, nrow = length(cruces_inf_macd), ncol = length(tickers.IBEX))
rent.prom <- matrix(0, nrow = length(cruces_inf_macd), ncol = length(tickers.IBEX))
titulo.rent <- 0

# Tarda unos 3 minutos en ejercutarse.
for(i in 1:length(cruces_inf_macd)){
  for(k in 1:length(tickers.IBEX)){
    
    # Se guarda el título a analizar en una variable.
    titulo.temp <- eval(parse(text = tickers.IBEX[k]))
    
    # Extracción de la cotización de cierre.
    titulo <- titulo.temp[,4]
    
    # Comprobar que la posición que se toma del IBEX tiene más de 100 datos históricos.
    if(cruces_inf_macd[i] > 100 & any(which(index(titulo) == index(IBEX.Clo[cruces_inf_macd[i]])))){
      titulo.n <- coredata(titulo)
      titulo.n <- na.approx(titulo.n)
      
      # Cálculo de la rentabilidad del título.
      for(j in 1:length(titulo.n) - 1){
        titulo.rent[j] <- log(titulo.n[j + 1] / titulo.n[j],base = exp(1))
      }
      titulo.rent <- na.approx(titulo.rent)
      
      # Comprobar que el título cotiza en la fecha que se está evaluando. Los títulos más recientes no tendrán cotización histórica en determinada fecha.
      if(any(index(titulo) == index(IBEX.Clo[cruces_inf_macd[i]]))){
        pos.temp <- which(index(titulo) == index(IBEX.Clo[cruces_inf_macd[i]]))
        
        # Comprobar que el título tiene más de 100 sesiones/rentabilidades en la posición del IBEX.
        if(pos.temp > 100){
          
          # Se halla los parámetros alfa y beta del modelo CAPM del título respecto al mercado.
          coefic.temp <- lm(titulo.rent[(pos.temp - 100):pos.temp] ~ IBEX.Clo.n.rent[(cruces_inf_macd[i] - 100):cruces_inf_macd[i]])
          
          # Extracción del parámetro beta.
          coefic.beta[i,k] <- coefic.temp$coefficients[2]
          
          # Rentabilidad promedio del título de las últimas 100 sesiones.
          rent.prom[i,k] <- mean(titulo.rent[(pos.temp - 100):pos.temp])
        }
      }
    }
    else{}
  }
}
beta.ibex <- matrix(0, nrow = length(coefic.beta[,1]), ncol = 10)

# Valores beta. Primer valor -> ticker. Segundo valor -> beta.
rent.p.pre <- matrix(0, nrow = length(coefic.beta[,1]), ncol = 5)

for(i in 1:length(coefic.beta[,1])){
  if(max(coefic.beta[i,]) > 0){
    beta.temp <- coefic.beta[i,]
    
    # Incluir aquellos títulos cuya rentabilidad promedio haya sido positiva en las últimas 100 sesiones.
    k <- 1
    while(k <= 5 & any(beta.temp > 0)){
      if(rent.prom[i,which(beta.temp == max(beta.temp))][1] > 0){
        if(max(beta.temp) == 0){
          # Eliminación del título con una beta máxima porque no cumple los requisitos.
          beta.ibex[i,2*k - 1] <- "0"
        }
        else{
          # Se almacena los códigos bursátiles en las columnas impares.
          beta.ibex[i,2*k - 1] <- tickers.IBEX[which(beta.temp == max(beta.temp))][1]
        }
        
        # Se almacena el máximo valor de beta en las columnas pares.
        beta.ibex[i,2*k] <- max(beta.temp)
        rent.p.pre[i,k] <- rent.prom[i,which(beta.temp == max(beta.temp))][1]
        
        # Se elimina la beta máxima para la siguiente interacción.
        beta.temp[which(beta.temp == max(beta.temp))] <- 0
        k <- k + 1
      }
      else{
        beta.temp[which(beta.temp == max(beta.temp))] <- 0
      }
    }
  }
}


# Parámetro que servirá para mantener la cartera durante un determinado periodo.
h <- 0
f <- 1

# Vector que almacenará las cotizaciones de la cartera en el momento previo.
cartera.pre <- 0

# Vector que almacenará las cotizaciones de la cartera en el momento posterior.
cartera.pos <- 0
cartera.rent.pre <- 0
cartera.rent <- 1
rentabilidades <- 0
pos <- 1
fechas.ibex <- "0"

# Tarda unos 14 segundos en ejecutarse.
for(i in 1:(length(beta.ibex[,1])-1)){
  # Asignación de títulos inicial.
  if(beta.ibex[i,1] != "0" & cartera.pre[1] == 0){
    k <- 1
    while(k <= 5){
      if(beta.ibex[i,2*k-1] != "0"){
        titulo.p <- matrix(0, nrow = length(eval(parse(text = tickers.IBEX[which(tickers.IBEX == beta.ibex[i,2*k - 1])]))[,4]), ncol = 1)
        titulo.p <- eval(parse(text = tickers.IBEX[which(tickers.IBEX == beta.ibex[i,2*k - 1])]))[,4]
        titulo.p <- na.approx(titulo.p)
        j <- which(index(IBEX.Clo[cruces_inf_macd[i]]) == index(titulo.p))
        titulo.p <- coredata(titulo.p)
        cartera.pre[k] <- titulo.p[j]
      }
      k <- k + 1
      fechas.ibex[2*pos-1] <- toString(index(IBEX.Clo[cruces_inf_macd[i]]))
    }
    if(cartera.pre[1] > 0){
      # Para la cartera 1: take profit = 1.48 y stop loss = 0.955
      # Para la cartera 2: take profit = 1.23 y stop loss = 0.955
      # Para la cartera 3: take profit = 1.10 y stop loss = 0.955
      while((cruces_inf_macd[i] + f <= cruces_inf_macd[i + 1]) & cartera.rent <= 1.48 & cartera.rent >= 0.955){
        # while((cruces_inf_macd[i] + f <= cruces_inf_macd[i + 1]) & cartera.rent <= 1.48 & cartera.rent >= 0.955)
        k <- 1
        while(k <= 5){
          if(beta.ibex[i,2*k - 1] != "0"){
            titulo.p <- matrix(0, nrow = length(eval(parse(text = tickers.IBEX[which(tickers.IBEX == beta.ibex[i,2*k - 1])]))[,4]), ncol = 1)
            titulo.p <- eval(parse(text = tickers.IBEX[which(tickers.IBEX == beta.ibex[i,2*k - 1])]))[,4]
            titulo.p <- na.approx(titulo.p)
            j <- which(index(IBEX.Clo[cruces_inf_macd[i]]) == index(titulo.p))
            titulo.p <- coredata(titulo.p)
            cartera.pos[k] <- titulo.p[j + f]
          }
          k <- k + 1
          fechas.ibex[2*pos] <- toString(index(IBEX.Clo[cruces_inf_macd[i] + f]))
        }
        k <- 1
        for (k in 1:length(cartera.pos)) {
          cartera.rent.pre[k] <- cartera.pos[k] / cartera.pre[k]
        }
        if(length(cartera.pos) == 2){
          cartera.rent <- 0.15*cartera.rent.pre[1] + 0.85*cartera.rent.pre[2]
        }
        if(length(cartera.pos) == 3){
          cartera.rent <- 0.1*cartera.rent.pre[1] + 0.3*cartera.rent.pre[2] + 0.6*cartera.rent.pre[3]
        }
        if(length(cartera.pos) == 4){
          cartera.rent <- 0.05*cartera.rent.pre[1] + 0.15*cartera.rent.pre[2] + 0.35*cartera.rent.pre[3] + 0.45*cartera.rent.pre[4]
        }
        if(length(cartera.pos) == 5){
          cartera.rent <- 0.05*cartera.rent.pre[1] + 0.1*cartera.rent.pre[2] + 0.2*cartera.rent.pre[3] + 0.3*cartera.rent.pre[4] + 0.35*cartera.rent.pre[5]
        }
        else{
          cartera.rent <- cartera.rent.pre[1]
        }
        f <- f + 1
      }
      rentabilidades[pos] <- cartera.rent
      pos <- pos + 1
      cartera.pre <- 0
      cartera.pos <- 0
      cartera.rent.pre <- 0
      cartera.rent <- 1
      f <- 1
    }
  }
  if(h == 5){
    h <- 0
  }
  else{
    h <- h + 1
  }
}


# Se corrige el contrasplit de Tubos Reunidos.
rentabilidades[15] <- 1.13423
# Varianza de la cartera hasta 2019.
var(rentabilidades[1:225])

# Rentabilidades de la cartera hasta 2019.
mean(rentabilidades[1:225])-1
# Número de operaciones de la cartera.
length(rentabilidades)