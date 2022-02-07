Viral_Automata3 <- function(){
  K <- 100
  Get_Parms3 <- function(){
    G <- as.numeric(readline("Número de filas y columnas: "))
    N <- as.numeric(readline("Número de individuos: "))
    M <- as.numeric(readline("Número de movimientos: "))
    GNM <- c(G,N,M)
    return(GNM)
  }
  StartWorld <- function(Parameters) {
    N <- Parameters[2]
    p <- c(0.4,0.3,0.2,0.1)
    
    x <- sample(1:Parameters[1],Parameters[2],replace = T,prob = NULL)
    y <- sample(1:Parameters[1],Parameters[2],replace = T,prob = NULL)
    id <- 1:N
    tiempo <- (rep(0,N))
    Age_Group <- sample(1:4,N,replace =TRUE,prob = p)
    Health <- sample(c("S","E","G","R","F"),N,replace = T)
    
    Pobla <- c(x,y,id,tiempo,Age_Group,Health)
    return(Pobla)
  }
  PlotPopulation3 <- function(Parameters,Poblacion){
    N <- Parameters[2]
    Age_Group <- Poblacion[((N*4)+1):(N*5)]
    Health    <- Poblacion[((N*5)+1):(N*6)]
    G         <- Parameters[1]
    
    plot(as.numeric(Poblacion[1:N]),as.numeric(Poblacion[(N+1):(2*N)]),
         xlim=c(1,G),
         ylim=c(1,G),
         xlab="x",
         ylab="y",
         col=c("green","orange","red","purple","black")[as.factor(Health)],
         pch=c(15,17,19,23)[as.numeric(Age_Group)])
         
    
  }
  Move_Automata3 <- function(Parameters,Poblacion){
    M  <- Parameters[3]
    N  <- Parameters[2]
    G  <- Parameters[1]
    x  <- as.numeric((Poblacion[1:N]))
    y  <- as.numeric(Poblacion[(1+N):(N*2)])
    id <- 1:N
    t  <- Poblacion[((3*N)+1):(4*N)]
    Ag <- Poblacion[((N*4)+1):(N*5)]
    H  <- Poblacion[((N*5)+1):(N*6)]
    
    DX <- sample(-M:M,N,replace = T)
    DY <- sample(-M:M,N,replace = T)
    
    Nueva_x <- x+DX
    Nueva_y <- y+DY
    
    NewX <- ifelse (Nueva_x <= 0,G+(Nueva_x%%G), ifelse (Nueva_x > G,Nueva_x%%G,Nueva_x))
    NewY <- ifelse (Nueva_y <= 0,G+(Nueva_y%%G), ifelse (Nueva_y > G,Nueva_y%%G,Nueva_y))
    
    Nueva_Pobla <- c(NewX,NewY,id,t,Ag,H)
    return(Nueva_Pobla)
  }
  Update_State3 <- function(Parameters,Poblacion_update){
    N <- Parameters[2]
    x  <- as.numeric((Poblacion_update[1:N]))
    y  <- as.numeric(Poblacion_update[(1+N):(N*2)])
    id <- 1:N
    tiempo <- as.numeric(Poblacion[((3*N)+1):(4*N)])
    Ag <- Poblacion_update[((N*4)+1):(N*5)]
    H  <- Poblacion_update[((N*5)+1):(N*6)]
    update_t <- tiempo+1
    
    comb <- c(x,y,id,as.character(update_t),Ag,H)
    return(comb)
  }
  Animate_History <- function(Historia,Parametros,step){
    N <- Parametros[2]
    t=0
    b <- readline("¿Querés recalcular el modelo un periodo mas? Si/No:  ")
    while (t<=K && b=="Si") {
      NuevaH <- Historia[t + 1, 1: (N * 6)]
      PlotPopulation3(Parameters,NuevaH)
      t <- t + step
      b <- readline("¿Querés recalcular el modelo un periodo mas? Si/No:  ")
    }
    
    
  }
  Update_Population <- function(Parameters,Poblacion){
    Poblacion_update <- Move_Automata3(Parameters,Poblacion)
    salida           <- Update_State3(Parameters,Poblacion_update)
    return(salida)
  }
  
  
  
  Parameters       <- Get_Parms3()
  Poblacion        <- StartWorld(Parameters)
  Historia         <- vector()

  for(t in 0:K){
    Poblacion           <- Update_Population(Parameters,Poblacion)
    Historia            <- rbind(Historia,Poblacion)
  }
  
  Animate_History(Historia, Parameters, 1)
}
