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
  require(tidyverse)

    # loop across contents of vector
  for (i in 1:length(vector[[1]])){

    # determine index of existing column
    col = find_existing_column(df, vector[[1]][i])


    # check to see if column for current element DOES NOT exists
    if(col == -999999){

      # build column
      df <- df %>%
        mutate(newcol = 0)

      # place indicator in column
      df$newcol[row] = 1

      # name new column
      df <- `colnames<-`(df,
                           c(colnames(df)[-length(df)], vector[[1]][i]))
    }else{

      df[[col]][row] = 1
    }
  }


    # return the updated df
    return(df)
}




