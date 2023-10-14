\name{expand_column}
\alias{expand_column}
\title{expand_column}
\usage{
 df <- expand_column(dataframe, "column_name", delimiter )
}
 \description{
   Expand a single column into several new columns with binary output using a speficied data delimiter.
   The delimiter should be a single character.
}
\examples{
    myDelimiter = ";"

    # Create some fake data with duplicates
    rows = c(
        c("a;b"), c("a;b;c"), c("b;c"), c("d"), c("d")
    )

    # Add to a dataframe
    df = data.frame(rows)

    colnames(df) <- c("myvar")
    expanded_df = expand_column(df, "myvar", myDelimiter)
}

