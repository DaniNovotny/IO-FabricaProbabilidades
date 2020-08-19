# probabilidades
p_MdO <-runif(1,0,1)
p_c_mat <-runif(1,0,1)
p_dem <- runif(1,0,1)

# cantidades mas tabla de referencia 
tabla <- matrix(c(43,44,45,46,47,0,0.1,0.3,0.7,0.9,0.1,0.3,0.7,0.9,1),5,3,FALSE)
tabla

MdO <- if (0 < p_MdO & p_MdO <= 0.1) { 43 
  } else if (0.1 < p_MdO & p_MdO <= 0.3) { 44
  } else if (0.3 < p_MdO & p_MdO <= 0.7) { 45
  } else if (0.7 < p_MdO & p_MdO <= 0.9) { 46
  } else if (0.9 < p_MdO & p_MdO <= 1) { 47
  }
c_mat <- 80+p_c_mat*(100-80)
demanda <- qnorm(p_dem,150000,4500)

# utilidad
utilidad <- demanda*(149 - MdO - c_mat) - 400000 - 600000

# todo junto en un solo vector
# vector <- c(p_MdO,MdO,p_c_mat,c_mat,p_dem,demanda, utilidad)
# vector


# convertirlo en una funcion que una todo y cree una matriz, 
# en la que cada uno elige la cantidad de filas que quiere correr
# esta simulacion.
filas <- 1

funcion <- function (filas) {
  matriz <- matrix(data = NA,filas,7)
  colnames(matriz) <- c("Prob 1","Costo MdO","Prob 2","Costo Materiales","Prob 3","Demanda","Utilidades")
  
  i <- 1
  while (i <= filas) {
    p_MdO <-runif(1,0,1)
    p_c_mat <-runif(1,0,1)
    p_dem <- runif(1,0,1)
    MdO <- if (0 < p_MdO & p_MdO <= 0.1) { 43 
    } else if (0.1 < p_MdO & p_MdO <= 0.3) { 44
    } else if (0.3 < p_MdO & p_MdO <= 0.7) { 45
    } else if (0.7 < p_MdO & p_MdO <= 0.9) { 46
    } else if (0.9 < p_MdO & p_MdO <= 1) { 47
    }
    c_mat <- 80+p_c_mat*(100-80)
    demanda <- qnorm(p_dem,150000,4500)
    utilidad <- demanda*(149 - MdO - c_mat) - 400000 - 600000
    
    vector <- c(p_MdO,MdO,p_c_mat,c_mat,p_dem,demanda, utilidad)
    
    matriz[i,] <- vector
    i <- i +1
  }
  return(matriz)
}

simulacion <- funcion(5000)
ut_prom <- mean(simulacion[,7])
