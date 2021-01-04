

test_datac <- data.frame ( a1=c(1,1,2,2,1,2,2,2,1,2), a2=c(1,2,3,2,3,3,1,2,3,3), a3=c(1,1,2,1,1,2,2,1,2,2),c=c(0,0,0,0,0,0,1,1,1,1))

test_data <- subset(test_datac, select=c("a1","a2","a3"))

learn_datac <- head(test_datac,9)

uniqued <- unique(test_datac[ncol(test_datac)])
uniqued


aode_bayes <- function(data, to_classify){
  unique_values <- unique(data[,ncol(data)])
  n <- nrow(data)
  P = data.frame()
  P_raw = c()
  
  for (i in unique_values){
    #    p_i = nrow(subset(data,data[,ncol(data)]==i))/n
    
    
    
    filtered_data <- subset(data)
    p_i = c()
    p_podsieci = c()
    it_vector_j <- 1:(ncol(test_datac)-1) 
    
    for (j in it_vector_j){
      n_ij = nrow(subset(data,data[,ncol(data)]==i & data[,j]==to_classify[,j]))
      p_ij = n_ij/n
      
      it_vector_k <- it_vector_j[it_vector_j!=j]  
      for (k in it_vector_k){
        n_ijk = nrow(subset(data,data[,ncol(data)]==i & data[,j]==to_classify[,j] & data[,k]==to_classify[,k]))
        if (n_ijk != 0){p_ijk = n_ijk/n_ij}
        else {p_ijk=0.01}
        
        p_ij = p_ij * p_ijk
      }
      
      p_podsieci <- append(p_podsieci, p_ij)
      
      
    }
    p_i <- mean(p_podsieci)
    P_raw <- append(P_raw, p_i)
    
  }
  
  P <- rbind(P,P_raw)
  colnames(P) <- unique_values
  
  result <- P
}

x <- aode_bayes(test_datac,test_datac[10,])











data <- test_datac
to_classify <- test_datac[10,]



