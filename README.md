# dfexpand
R module to expand a delimited column in a dataframe into multiple binary columnar output

# Description:

  Imagine you have a data frame that contains a column which has
  a variety of delimited values in a single column. Note, this may
  take a while on large data sets, and is not yet optimized for
  performance. E.g. a 90k row data set where the column splits into
  13 sub-columns may take up to 1-2 minutes to run as the function
  generates each new sub-column in the data frame.

## Example Case

  Example: The column lists "My Favorite Programming Languages"
           in a column titled "lang". The individuals who were
           surveyed could have entered one or more programming
           languages as their favorite. An example of survey results
           could look like the following.

           User 1: Python, Java
           User 2: Java, C
           User 3: Python

           And in the data set, we would see something like:
           
            |--------|-------------|
            | User   | lang        |
            |--------|-------------|
            | user_1 | Python;Java |
            | user_2 | Java;C      |
            | user_3 | Python      | 
            |--------|-------------|
           
           While efficient for data storage, it is not easy to do any analysis
           on this data set without splitting out the exact values. One
           issue found with most modern libraries is that they are expecting
           your values to be in a specific order when they are split. 
           This method does not make that assumption.
           
           dfexpand's expand_column() function will extract the unique
           values found for all users surveyed from this column, then it will
           iterate and expand the column from "lang" into 3 new distinct columns.
           
           New columns:
             "lang_Python", "lang_Java", and "lang_C"

           Further, it will populate each new column with a 0 or 1
           as appropriately matched to each row entry.

           For the above example, you would then expect to see in the expanded dataframe:

            |--------|-------------|-------------|---------|
            | User   | lang_Python |  lang_Java  |  lang_C |
            |--------|-------------|-------------|---------|
            | user_1 |       1     |       1     |     0   |
            | user_2 |       0     |       1     |     1   |
            | user_3 |       1     |       0     |     0   |
            |--------|-------------|-------------|---------|

          The method does not drop the original 'lang' column but it has not been
          shown here for simplification.

## Example R code

```r
    myDelimiter = ";"

    # Create some fake data with duplicates
    rows = c(
        c("a;b"), c("a;b;c"), c("b;c"), c("d"), c("d")
    )

    # Add to a dataframe
    df = data.frame(rows)

    colnames(df) <- c("myvar")
    expanded_df = expand_column(df, "myvar", myDelimiter)

```
