######################################################
# Unit tests
######################################################

######################################################
#       Test getDistinctValues() function
######################################################

# Test 1
test_that("getDistinctValues_removeDuplicatesWorks",{
  test_data <- "8,1,2,3,1,2,3,8"
  expected_data <- list("1", "2", "3", "8")
  results <- getDistinctValues(test_data, delimiter= ",")
  expect_equal(expected_data, results)
})

# Test 2
test_that("getDistinctValues_testTrimFunctionTrue",{
  test_data <- " 8 ,1 ,2, 3,1,2,3,8"
  expected_data <- list("1", "2", "3", "8")
  results <- getDistinctValues(test_data, delimiter= ",", trim= TRUE)
  expect_equal(expected_data, results)
})

# Test 3
test_that("getDistinctValues_testTrimFunctionFalse",{
  
  # Setup test data
  test_data <- " 8 ,1 ,2, 3,1,2,3,8"
  
  # This is what we expect with trim on
  expected_trimmed_data <- list("1", "2", "3", "8")
  
  # Spaces come first in order
  expected_data <- list(" 3", " 8 ", "1", "1 ", "2", "3", "8")
  
  results <- getDistinctValues(test_data, delimiter= ",", trim= FALSE)
  
  # These should no longer be equal
  expect_false(isTRUE(all.equal(expected_trimmed_data, results)))
  
  # New expected results
  expect_equal(expected_data, results)
})

# Test 4
test_that("getDistinctValues_testIgnoreCaseTrue",{
  
  # Setup test data
  test_data <- "a,A,b,C,B,c"
  
  # This is what we expect with trim on
  expected_data <- list("a", "b", "c")
  
  results <- getDistinctValues(test_data, delimiter= ",", trim= TRUE, ignore_case = TRUE)
  
  # New expected results
  expect_equal(expected_data, results)
})

# Test 5
test_that("getDistinctValues_testIgnoreCaseFalse",{
  
  # Setup test data
  test_data <- "a,A,b,C,B,c"
  
  # This is what we expect with trim on
  expected_data <- list("a", "A", "b", "B", "c", "C")

  results <- getDistinctValues(test_data, delimiter= ",", trim= TRUE, ignore_case = FALSE)
  
  # New expected results
  expect_equal(expected_data, results)
})

# Test 6
test_that("getDistinctValues_testTrimeAndIgnoreCase",{
  
  # Setup test data
  test_data <- "a,A,  b,C ,B,c "
  
  # This is what we expect with trim on
  expected_data <- list("a", "b", "c" )
  
  results <- getDistinctValues(test_data, delimiter= ",", trim= TRUE, ignore_case = TRUE)
  
  # New expected results
  expect_equal(expected_data, results)
})


# Test 7
test_that("getDistinctValues_testNADataEntry",{
  
  # Setup test data
  test_data <- NA
  
  # This is what we expect with trim on
  expected_data <- NA
  
  results <- getDistinctValues(test_data, delimiter= ",", trim= TRUE, ignore_case = TRUE)
  
  # New expected results
  expect_equal(expected_data, results)
})

######################################################
# Expect errors here
######################################################
testthat::test_that("errors", {
  # Missing delimiter test
  testthat::expect_error(
    getDistinctValues("1,2,3,4"),
    "Missing delimiter"
  )
})

######################################################
#       Test expand_column() function
######################################################

######################################################
# Expect errors here
######################################################
testthat::test_that("errors", {

  # Missing dataframe
  testthat::expect_error(
    expand_column(),
    "Function was called without a dataframe"
  )
  
  # Missing column name to expand
  testthat::expect_error(
    expand_column(data.frame(c( c("a"), c("a,b")))),
    "No column was specified"
  )
 
  # Invalid data type for data frame
  testthat::expect_error(
    expand_column("test_stuff"),
    "Function was called without a dataframe"
  )

  # Invalid data type for column name
  testthat::expect_error(
    expand_column(data.frame(c( c("a"), c("a,b"))), data.frame(c( c("a"), c("a,b")))),
    "Column name must be provided as a string"
  )
  
  # Test that column number is not less than 1
  testthat::expect_error(
    expand_column(data.frame(c( c("a"), c("a,b"))), colnumber = 0),
    "Column number cannot be less than 1"
  )

  # Test that column number is not greater than the total number of columns
  testthat::expect_error(
    expand_column(data.frame(c( c("a"), c("a,b"))), colnumber = 10),
    "Column number cannot exceed total number of columns in data frame"
  )
  
  
})