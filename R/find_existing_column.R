# name: find_existingo_column
# arguments: dataframe (df), name of column to check for (colName)
# process:   Checks dataframe for column name matching colName parameter.
#            Returns index of column or NO_COLUMN_FOUND.
# output:    index of column or NO_COLUMN_FOUND
# dependencies: colnames, any, which

#' findExistingColumn
#'
#' @param df dataframe to be checked
#' @param colName name of column to check dataframe for
#'
#' @return index of column or NO_COLUMN_FOUND (-999999)
#'
#' @examples findExistingColumn(insertCommas, "acetaminophen_mg")

find_existing_column <- function(df, colname){

  # initialize index to default value
  columnIndex = -999999

  # create vector of column names in dataframe
  colNameVector <- colnames(df)

  if(any(colNameVector == colname)){

    columnIndex = which(colNameVector == colname)

  }

  # return value as specified
  return(columnIndex)

}
