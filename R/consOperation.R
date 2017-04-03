convertCons <- function(consMatrix, numVar) {
  # create temporary matrix
  tempMat <- matrix(NA, numVar, numVar)

  # initial index
  ind <- 1

  #fill in the matrix by column
  for (i in 1:numVar) {
    for (j in 1:numVar) {
      if (i != j) {
        tempMat[j, i] <- ind
        ind <- ind + 1
      }
    }
  }

conString <- NULL

  for (i in 1:nrow(consMatrix)) {
    conString <- c(conString, tempMat[consMatrix[i, 1], consMatrix[i, 2]])
  }

  return(sort(conString))
}

#function to convert/extend the constraint matrix for stability selection
cons4Stab <- function(consMatrix, numVar, longitudinal) {

  if (longitudinal) {

    #create constraint for inter
    cons_inter <- NULL
    for (i in 1:numVar) {
      cons_inter <- rbind(cons_inter,
                          matrix(c(c(1:numVar), rep(numVar + i, numVar)),
                                 numVar, 2))
    }

    #swap column, for intra, and add with NumVar to get correct indices
    #in longitudinal model
    #in case no constraint
    if (nrow(consMatrix) == 1 & all(consMatrix == 0)) {

      return(cons_inter)

    } else if (nrow(consMatrix) == 1){

      cons_intra <- matrix(c(consMatrix[, 2], consMatrix[, 1]),
                           1, 2, byrow = TRUE) + numvar

    } else {

      cons_intra <- consMatrix[, c(2, 1)] + numVar
      return(rbind(cons_intra, cons_inter))

    }

  } else { #if cross-sectional

    #swap the column, e.g., 1 becomes 2 and vice versa
    #in case no constraint
    if (nrow(consMatrix) == 1 & all(consMatrix == 0)) {
      return(matrix(0, 1, 2, byrow = TRUE))

    } else if (nrow(consMatrix) == 1){

      #in case only one constraint, to ensure returning matrix of 1x2
      return(matrix(c(consMatrix[, 2], consMatrix[, 1]),
                    1, 2, byrow = TRUE))

    } else {

      #in case of constraint
      return(consMatrix[, c(2, 1)])

    }
  }
}
