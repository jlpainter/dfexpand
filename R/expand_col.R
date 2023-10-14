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
getDistinctValues <- function(entry, delimiter)
{
  # First, test that the entry is not 'NA'
  if (is.na(entry) == FALSE )
  {

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
            distinct_values <- append(distinct_values, sub_element)
          }
        } else {
          # This is a single value entry
          distinct_values <- append(distinct_values, entry)
        }
      }

      # Remove duplicates
      distinct_values <- unique(distinct_values)
      return(distinct_values)
    }
  } else {
    return(NA)
  }
}

###############################################################################
# Expand a dataframe that contains a column which has split entries
###############################################################################

expand_column <-function(dataframe, colname, delimiter)
{

  # Extract the unique values for this column out of the data frame
  unique_cols <- unique(dataframe[[colname]])

  # Build a list of distinct values
  distinct_values = list()

  # Loop through the unique column entries and extract individual values that are
  # separated by a semi-colon. Add those to the distinct_values list above
  #
  for ( entry in unique_cols )
  {
    for ( dv in getDistinctValues( entry, delimiter ) )
    {
      if (is.na(dv) == FALSE) {
        distinct_values <- append(distinct_values, dv)
      }
    }
  }

  # Remove duplicates
  distinct_values <- unique(distinct_values)

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
      for ( dv in getDistinctValues(current_val, delimiter) )
      {
        # Update this row to have a 1
        new_col = paste0(colname, "_", dv)
        dataframe[row, new_col] = 1
      }
    }
  }

  # First get the unique values found in the column
  unique_cols <- unique(dataframe[[colname]])
  return(dataframe)
}

