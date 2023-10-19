# Dependencies
library("stringr")


#' @title dfexpand
#'
#' @description Methods to auto-expand a delimited column into multiple columns
#'
#' @param dataframe The dataframe that contains the information you want to
#' expand upon.
#' @param colname The name of the column in the provided data frame which contains
#' delimited data elements for each row.
#' @param delimiter This should be a single quoted character such as a comma or
#' semi-colon which denotes how the data are separated in the column you wish
#' to expand
#' 
#' @return dataframe
#'
#'
#' @export


#####################################################################
# expand_col.R
#
# @author: Jeffery Painter <jeff@jivecast.com>
# @date: 2023-Oct-14
# Copyright 2023 by Jeffery Painter, All Right Reseved
#
# Description:
#
#  Imagine you have a data frame that contains a column which has
#  a variety of delimited values in a single column. Note, this may
#  take a while on large data sets, and is not yet optimized for
#  performance. E.g. a 90k row data set where the column splits into
#  13 sub-columns may take up to 1-2 minutes to run as the function
#  generates each new sub-column in the data frame.
#
#  Example: The column lists "My Favorite Programming Languages"
#           in a column titled "lang" and users could enter a variety.
#
#           User 1: Python, Java
#           User 2: Java, C
#           User 3: Python
#
#           But you may not know the order in which the languages
#           appear. This method will extrat all the unique
#           values from this column, iterate through each row,
#           and expand the column from "lang" into 3 new columns
#
#           New columns:
#             "lang_Python", "lang_Java", and "lang_C"
#
#           Further, it will populate each new column with a 0 or 1
#           as appropriately matched to each row entry.
#
#           For the above example, you would then expect to see:
#                       lang_Python |  lang_Java  |  lang_C
#            __________________________________________________
#              user_1 |       1     |       1     |     0
#              user_2 |       0     |       1     |     1
#              user_3 |       1     |       0     |     0
#            __________________________________________________
#####################################################################

#####################################################################
# Expanding a data frame code example
#
# Sample code:
#
# My Delimiter
# myDelimiter = ";"
#
#  Create some fake data with duplicates
# rows = c(
#     c("a;b"), c("a;b;c"), c("b;c"), c("d"), c("d")
# )
#
# Add to a dataframe
# df = data.frame(rows)
#
# # Name the column 'myvar'
# colnames(df) <- c("myvar")
#
# # Call the expand column function
# new_df = expand_column(df, "myvar", myDelimiter)
#####################################################################

###############################################################################
# Helper function that gets distinct values from a string
###############################################################################

#' Extract the distinct values from a string given a delimiter
#'
#' @param entry A string to parse.
#' @param delimiter A single character to split the string on.
#' @return A list of distinct values found in the entry string
#' @examples
#' values <- getDistinctValues(entry, delimiter)
#' 
getDistinctValues <- function(entry, delimiter, trim = TRUE, ignore_case = FALSE)
{
  # It is possible that as we iterate over the data, that we have NA in a column
  # Therefore, we must return NA if no data was sent from the expand_col function.
  if (is.na(entry) == TRUE )
  {
    return(NA)
  } else {

    # A delimiter is required    
    if ( missing(delimiter) )
    {
      stop("Missing delimiter")
    } else {

      # List of distinct values to collect
      distinct_values = list()
  
      # Test non-zero length string
      x = nchar(entry)
      if ( is.na(x) )
      {
        return(NA)
      } else {
        if (x > 0 )
        {
          # Does it contain a delimiter we want to split on?
          entry_contains_delimiter <- grepl(delimiter, entry, fixed = TRUE )
          if ( entry_contains_delimiter == TRUE )
          {
            # Quiet = TRUE will suppress printing output line by line
            subelements = as.list(scan(text=entry, what='', sep=delimiter, quiet=TRUE))
            for ( sub_element in subelements ) {
              
              if ( trim == TRUE ) {
                # By default, trimws will trim both leading and trailing whitespace
                # and removes "[ \t\r\n]"
                sub_element = trimws(sub_element)
              }
              
              # If we are ignoring case, convert all to lower case
              if ( ignore_case == TRUE ) {
                sub_element = tolower(sub_element)
              }
              
              distinct_values <- append(distinct_values, sub_element)
            }
          } else {
            # This is a single value entry
            # Test for trim
            if ( trim == TRUE ) {
              entry = trimws(entry)
            }
            
            # Test for case ignore
            if ( ignore_case == TRUE ) {
              entry = tolower(entry)
            }
  
            distinct_values <- append(distinct_values, entry)
          }
        }
  
        # Remove duplicates
        distinct_values <- unique(distinct_values)
        
        # Sort order
        distinct_values <- distinct_values[str_order(distinct_values, numeric = TRUE)]
        return(distinct_values)
      }
    }
  }
}

###############################################################################
# Expand a dataframe that contains a column which has split entries
###############################################################################

#' Expand a single column containing delimited values into multiple binary columns
#'
#' @param dataframe The data frame containing the column we want to expand
#' @param colname The name of the column to split on.
#' @param delimiter A single character to split the string on.
#' 
#' @param trim Boolean field to trim white space when searching for unique values
#' @param ignore_case Boolean flag if you want the split values to ignore case
#' @param colnumber You can provide the column number in the dataframe to expand, rather than the name
#' 
#' @return A list of distinct values found in the entry string
#' @examples
#' new_df <- expand_column(dataframe, "myColumn", ';')
#' 
expand_column <-function(dataframe, colname = NULL, delimiter = ';', trim = TRUE, ignore_case = FALSE, colnumber = NULL)
{
  ##############################################################################
  # Error checking
  ##############################################################################
  if ( missing(dataframe) )
  {
    stop("Function was called without a dataframe")
    return()
  } else {
    if ( class(dataframe) != class(data.frame())) {
      stop("Function was called without a dataframe")
      return()
    }
  }
  
  ##############################################################################
  # Get the column name if we were provided the column number instead  
  ##############################################################################
  if ( missing(colname) )
  {
    if (missing(colnumber) == FALSE ) {
      if ( class(colnumber) != class(123)) {
        stop("Column number must be an integer")
        return()
      } else {
        
        # Add some error checking
        total_columns = length(colnames(dataframe))
        if ( colnumber < 1 )
        {
          stop("Column number cannot be less than 1")
          return()
        } else {
          if ( colnumber > total_columns ) {
            stop("Column number cannot exceed total number of columns in data frame")
            return()
          } else {
            # Set the column name
            colname = colnames(dataframe)[colnumber]    
          }
        }
      }
    } else {
      stop("No column was specified")
    }
  } else {
    if ( class(colname) != class("string") ) {
      stop("Column name must be provided as a string")
      return()
    }
  }
  
  # The data in this column must be character data, otherwise
  # we cannot split strings!
  if ( class(dataframe[[colname]]) != class("string") )
  {
    stop("The data in the column provided are not strings")
    return()
  }
  ##############################################################################
  # End of error checking and setup of the function data type
  ##############################################################################
  
  # Extract the unique values for this column out of the data frame
  unique_cols <- unique(dataframe[[colname]])
  
  # Build a list of distinct values
  distinct_values = list()
  
  # Loop through the unique column entries and extract individual values that are
  # separated by a semi-colon. Add those to the distinct_values list above
  #
  for ( entry in unique_cols )
  {
    for ( dv in getDistinctValues( entry, delimiter, trim = trim, ignore_case = ignore_case ) )
    {
      if (is.na(dv) == FALSE) {
        distinct_values <- append(distinct_values, dv)
      }
    }
  }
  
  # Remove duplicates
  distinct_values <- unique(distinct_values)
  
  # Sort order
  distinct_values <- distinct_values[str_order(distinct_values, numeric = TRUE)]  
  
  # Create the columns and initialize all values to zero
  for ( dv in distinct_values )
  {
    new_col = paste0(colname, "_", dv)
    dataframe[new_col] = 0
  }
  
  # Now iterate row by row, and update the rows to be a 1 if
  # they contained that distinct value
  for (row in 1:nrow(dataframe)) {
    current_val = dataframe[row, colname]
    if ( length(current_val) == 1 && !is.na(current_val) )
    {
      for ( dv in getDistinctValues(current_val, delimiter, trim = trim, ignore_case = ignore_case ) )
      {
        # Update this row to have a 1
        new_col = paste0(colname, "_", dv)
        dataframe[row, new_col] = 1
      }
    }
  }

  return(dataframe)
}


