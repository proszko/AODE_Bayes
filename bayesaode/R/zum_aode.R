#' bayesaode: A package for classifying dataset using non-naive AODE Bayesian algorithm
#'
#' The package provides two functions: aode_bayes and predict for usage of
#' performing classification task using non-naive Bayesian algorithm:
#'
#' @section aode_bayes:
#' aode_bayes function is used for creating model object for aode algorithm.
#'
#' @section predict:
#' function is used to perform a classifaction task using aode object
#'
#' @docType package
#' @name bayesaode
NULL
#> NULL



#' Creating aode object
#'
#' @param data training data for model creation
#' @param formula variable dependency description
#'
#' @return aode object
#' @export
#'
#' @examples data <- data.frame ( a1=c(1,1,2), a2=c(1,2,3), a3=c(1,1,2),c=c(0,0,1))
#' f <- c ~ .
#' x <- aode_bayes(data,f)
aode_bayes <- function (data, formula){

  result <- list(training_data = data, formula = formula)
  return(result)

}



#' Perform classification task for validation data using AODE Bayesian algorithm
#'
#' @param aode aode object
#' @param data data used for model validation
#'
#' @return classification vector
#' @export
#'
#' @examples data <- data.frame ( a1=c(1,1,2), a2=c(1,2,3), a3=c(1,1,2),c=c(0,0,1))
#' f <- c ~ .
#' x <- aode_bayes(data,f)
#' res <- predict(x,data)
predict <- function(aode, data){

  form <- aode[["formula"]]

  training_data <- stats::model.frame(form, data = aode[["training_data"]])

  validation_data <- stats::model.frame(form, data = data)

  unique_values <- unique(training_data[,1])
  n <- nrow(training_data)
  P = data.frame()
  result = c()
  unique_columns <- c()

  for (col_no in 1:ncol(training_data)){
    unique_columns <- append(unique_columns, length(unique(training_data[,col_no])))
  }


  for (row_no in 1:nrow(validation_data)){

    entry = validation_data[row_no,]
    P_raw = c()

    for (i in unique_values){

      p_i = c()
      p_podsieci = c()
      it_vector_j <- 2:(ncol(training_data))

      for (j in it_vector_j){
        n_ij = nrow(subset(training_data,training_data[,1]==i & training_data[,j]==entry[,j]))
        p_ij = (n_ij+1/(unique_columns[j]*unique_columns[1]))/(n+1) #wygladzone
        #p_ij = n_ij/n #niewygladzone

        # it_vector_k <- it_vector_j[it_vector_j!=j]
        for (k in it_vector_j){
          if (j==k){next}
          n_ijk = nrow(subset(training_data,training_data[,1]==i & training_data[,j]==entry[,j] & training_data[,k]==entry[,k]))
          p_ijk = (n_ijk+1/unique_columns[k])/(n_ij+1) #wygladzone
          # p_ijk = n_ijk/n_ij    #niewygladzone
          p_ij = p_ij * p_ijk
        }

        p_podsieci <- append(p_podsieci, p_ij)


      }
      p_i <- mean(p_podsieci)
      P_raw <- append(P_raw, p_i)

    }
    P <- rbind(P,P_raw)
    result <- append(result, unique_values[which.max(P_raw)])
    real_result <- validation_data$c

  }



  accuracy <- sum(result==real_result)/nrow(validation_data)
  confusion_matrix <- matrix(0, nrow = length(unique_values), ncol = length(unique_values))
  rownames(confusion_matrix) <- unique_values
  colnames(confusion_matrix) <- unique_values
  kappa <- (accuracy - 1/length(unique_values))/(1-accuracy)

  for (i in 1:length(result)){
    confusion_matrix[match(real_result[i],unique_values),match(result[i],unique_values)] = confusion_matrix[match(real_result[i],unique_values),match(result[i],unique_values)] + 1
  }

  classification_quality = data.frame(precision=c(),recall=c(),F1=c())
  for (x in 1:length(unique_values)){
    TP <- 0
    TN <- 0
    FP <- 0
    FN <- 0
    for(i in 1:length(unique_values)){
      for (j in 1:length(unique_values)){
        if (x==i && x==j){TP <- TP + confusion_matrix[i,j]}
        else if (x==i && x!=j){FN <- FN + confusion_matrix[i,j]}
        else if (x!=i && x==j){FP <- FP + confusion_matrix[i,j]}
        else if (x!=i && x!=j){TN <- TN + confusion_matrix[i,j]}
      }
    }
    precision = TP / (TP+FP)
    recall = TP / (TP+FN)
    F1 = 2*recall*precision/(recall+precision)
    classification_quality <- rbind (classification_quality, c(precision,recall,F1))

  }
  colnames(classification_quality) <- c("precision","recall","F1")
  rownames(classification_quality) <- unique_values

  colnames(P) <- unique_values

  for (i in 1:length(unique_values)){
    result[result==i]=unique_values[i]
  }

  P
  result
  classification_quality



  return(result)
}




