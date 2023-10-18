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

# Test 1
test_that("expand_column_BasicTest",{
  
  # Create some fake data with duplicates
  rows = c(
    c("c;a;b"), c("a;b"), c("c;b"), c("d"), c("d")
  )
  
  # Add to a dataframe
  df = data.frame(rows)
  
  colnames(df) <- c("myvar")
  #
  # The default behavior is to trim extra whitespace from the extracted values, 
  # but not to alter or change the case of the values. So 'Alpha' is distinct from 'alpha'
  # but ' beta ' is the same as 'beta'. You can override this behavior with
  # the trim and ignore case flags.
  #
  expanded_df = expand_column(df, colname = "myvar", colnumber = 1, ';', trim = TRUE, ignore_case = TRUE )
  expected_colnames = c("myvar", "myvar_a", "myvar_b", "myvar_c", "myvar_d")
  results <- colnames(expanded_df)
  
  # Test that the column names are equivalent
  expect_equal(results, expected_colnames)
})


# Test 2
test_that("expand_column_ColNumberTest",{
  
  # Sample dataframe
  df <- structure(list(ID = 1:3, Colors = c("Red, Blue", "Yellow", "Green, Black"), Shapes = c("Triangle", "Square", "Circle, Oval")), class = "data.frame", row.names = c(NA, -3L))
  # Split on colors, comma separated values
  expanded_df = expand_column(df, colnumber = 2, delimiter = ',', trim = TRUE, ignore_case = TRUE )
  
  # The newly appended column names should be in alpha-numeric order
  expected_colnames = c("ID", "Colors", "Shapes",  "Colors_black", "Colors_blue", "Colors_green", "Colors_red", "Colors_yellow" )
  results <- colnames(expanded_df)
  
  # Test that the column names are equivalent
  expect_equal(results, expected_colnames)
})

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
 
  # Non-string data in column
  testthat::expect_error(
    expand_column(data.frame(c( c(1), c(23))), colnumber = 1),
    "The data in the column provided are not strings"
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