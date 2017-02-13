context("Expected input arguments and output in getModelFitness")

models <- modelPop(nPop=15, numVar=6, longitudinal=FALSE,
                   consMatrix = matrix(c(1, 2), 1, 2))

test_that("Incorrect/missing input arguments yields errors in getModelFitness", {

  # Argument theData
  expect_error(getModelFitness(theData=NULL, allModelString=models,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Data cannot be missing")

  expect_error(getModelFitness(theData=1:10, allModelString=models,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Data should be either a data frame or a matrix of numerical values.")

  expect_error(getModelFitness(theData=c("a", "b"), allModelString=models,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Data should be either a data frame or a matrix of numerical values.")

  expect_error(getModelFitness(theData=data.frame(letter=letters[1:3], number=1:3),
                               allModelString=models,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Data should be either a data frame or a matrix of numerical values.")


  # other arguments
  expect_error(getModelFitness(theData=crossdata6V, allModelString=1:3,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Argument allModelString should be formed in a matrix.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=NULL,
                               longitudinal=FALSE, co="covariance", mixture=FALSE),
               "Argument allModelString cannot be missing.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=models,
                               longitudinal=NULL, co="covariance", mixture=FALSE),
               "Argument longitudinal cannot be missing.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=models,
                               longitudinal=1:3, co="covariance", mixture=FALSE),
               "Argument longitudinal should be either logical TRUE or FALSE.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=models,
                               longitudinal=FALSE, co="covariance", mixture=1:3),
               "Argument mixture should be either logical TRUE or FALSE.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=models,
                               longitudinal=FALSE, co="wrongString", mixture=FALSE),
               "Argument co should be either covariance or correlation matrix.")

  expect_error(getModelFitness(theData=crossdata6V, allModelString=models,
                               longitudinal=FALSE, co=20, mixture=FALSE),
               "Argument co should be a string of characters, e.g., either covariance or correlation.")

})

test_that("Correct input arguments yield expected output in modelFitness.", {

  skip_on_cran()
  result <- getModelFitness(theData=crossdata6V, allModelString=models,
                            longitudinal=FALSE, co="covariance", mixture=FALSE)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(models))
  expect_equal(ncol(result), ncol(models) + 2)
})
