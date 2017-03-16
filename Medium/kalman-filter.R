
KF<-function(Z,X,F,P,H,Q,R){
  
  if(length(Z)==0) stop("Length of Z is Zero")
  
  #check Z
  stopifnot(is.numeric(Z[[1]]))
  if(length(Z)>1)
    for(i in 2:length(Z)){
    stopifnot(is.numeric(Z[[i]]))
    if((nrow(Z[[i]])!=nrow(Z[[i-1]])) || (ncol(Z[[i]])!=ncol(Z[[i-1]])) ){
      stop("Dimensions across Z are inconsistant")
    }
  }
  
  #check Inputs
  stopifnot(is.numeric(X) && is.matrix(X))
  stopifnot(is.numeric(F) && is.matrix(F))
  stopifnot(is.numeric(P) && is.matrix(P))
  stopifnot(is.numeric(H) && is.matrix(H))
  stopifnot(is.numeric(Q) && is.matrix(Q))
  stopifnot(is.numeric(R) && is.matrix(R))
  
  #check Dimensions
  stopifnot(ncol(F)==nrow(X))
  stopifnot(nrow(F)==nrow(X))
  stopifnot(nrow(P)==nrow(F))
  stopifnot(ncol(P)==ncol(F))
  stopifnot(nrow(Q)==nrow(F))
  stopifnot(ncol(Q)==ncol(F))
  stopifnot(ncol(H)==nrow(X))
  stopifnot(nrow(H)==nrow(Z[[1]]))
  stopifnot(ncol(R)==nrow(H))
  stopifnot(nrow(R)==nrow(H))
  
  obj <- list("Z"=Z,"X"=X,"F"=F,"P"=P,"H"=H,"Q"=Q,"R"=R,"predicted_states"=list())
  
  #set name for class
  class(obj)<-append(class(obj),"KF")
  return(obj)
}

kalmanUpdate<-function(obj){
  UseMethod("kalmanUpdate",obj)
}

kalmanUpdate.KF<-function(obj){
  
  obj$predicted_states[[1]]<-obj$X
  for(i in 1:length(obj$Z)){
    
    if(i!=1){
      x_<-obj$predicted_states[[i-1]]  
    }
    else{
      x_<-obj$X
    }

    #Predict Step
    x <- obj$F%*%x_
    obj$P <- obj$F %*% obj$P %*% t(obj$F) + obj$Q
    
    #Update Step
    y <- obj$Z[[i]] - obj$H %*% x
    s <- (obj$H %*% obj$P %*% t(obj$H)) + R
    k <- obj$P %*% t(obj$H) %*% solve(s)
    
    x <- x + k %*% y
    obj$P <- obj$P - (k %*% obj$H %*% obj$P)
    obj$predicted_states[[i]] <- x 
  }
  return(obj)
}




