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
           in a column titled "lang" and users could enter a variety.

           User 1: Python, Java
           User 2: Java, C
           User 3: Python

           But you may not know the order in which the languages
           appear. This method will extrat all the unique
           values from this column, iterate through each row,
           and expand the column from "lang" into 3 new columns

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
