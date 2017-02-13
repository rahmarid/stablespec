#' Reshape longitudinal data with \code{t} time slices into a longitudinal
#' data with two time slices.
#' @title Reshape longitudinal data
#' @param theData a data frame containing longitudinal
#' data to which the model will be fit.
#' @param numTime number of time slices.
#' @return A data frame representing longitudinal data with
#' two time slices, such that the first \code{n} data points contain the
#' relations that occur in the first two time slices
#' \code{t_0} and \code{t_1}. The next \code{n} data points contain the
#' relations that occur in time slices \code{t_1} and \code{t_2}.
#' The \code{i-th} subset of \code{n} data points contain the relations
#' in time slices \code{t_i-1} and \code{t_i}. The reshaped data can be used
#' as data input for function \code{\link{stableSpec}} when computing longitudinal data.
#'
#' @examples
#' the_data <- longiData4V3T
#' num_time <- 3
#' reshaped_the_data <- dataReshape(the_data, num_time)
#' @author Ridho Rahmadi \email{r.rahmadi@cs.ru.nl}
#' @export
dataReshape <- function(theData=NULL, numTime=NULL) {
  #get number of variables
  numVar <- ncol(theData) / numTime
  allData <- columnNames <- list()
  theIndex <- 1

  #get column names of two time slices
  for (j in 1:(numVar * 2)) {
    columnNames[[j]] <- paste('x', j, sep="")
  }

  for (i in 1:(numTime - 1)) {

    # get t1..t2, t2..t3, and so on
    allData[[i]] <- data.frame(theData[, c(theIndex:((theIndex - 1) + 2 * numVar))])

    #rename the columns
    colnames(allData[[i]]) <- c(unlist(columnNames))

    #renew index
    theIndex <- theIndex + numVar

  }

  # merge all data
  fixData <- do.call(rbind, allData)
  return(fixData)

}
