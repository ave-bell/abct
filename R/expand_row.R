# name: expand_row

# process: utility function for column_expansion, applies expansion to a
#          single row.

# arguments: df-dataframe, vector-vector of values to build columns for,
#            row-row number of dataframe

# dependencies: find_existing_column

#' expand row
#'
#' @param df dataframe
#' @param vector vector of values to build columns for
#' @param row row number of dataframe
#'
#' @return updated dataframe
#'
#' @examples expand_row(df, row_contents_vector, row_number)
expand_row <- function(df, vector, row)
{
  NO_COLUMN_FOUND = -999999

  # loop across contents of vector
  for (i in length(vector)){
    # determine index of existing column
    col = find_existing_column(df, vector[[i]])

    # check to see if column for current element DOES NOT exists
    if(col == NO_COLUMN_FOUND){

      # build column
      df <- df %>%
        mutate(newcol = 0)

      # name new column
      `colnames<-`(df, c(colnames(df)[-1], vector[[i]]))

    }

  }
    # place indicator value into column
    df$length(df)[row] = 1

    # return the updated df
    return(df)
}




