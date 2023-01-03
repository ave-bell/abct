# name: column_expansion

# process: expands a dataframe column that contains multiple pieces
#          of information in to multiple columns with one piece of
#          information.

# arguments: df-dataframe, colname-column for expansion,
#            separator-separating character

# dependencies: expand_row

#' Column Expansion
#'This function performs a column expansion. A column expansion is required when
#'multiple pieces of data are included in a single column. This function assumes that
#'all seperator characters are correctly located, and the data in the column is spelled correctly.
#' @param df dataframe
#' @param colname name of the column for expansion
#' @param separator character that separates the data
#'
#' @return expanded dataframe
#' @export
#'
#' @examples column_expansion(df, Diseases, ",")
#'
column_expansion <- function(df, colname, separator)
{
  # loop across rows
  for (row in nrow(df)){

    # split contents of current row
      # function: strsplit
    contents <- strsplit(df$colname[[row]], separator)

    # expand row contents
      # function: expand_row
    df <- expand_row(df, contents, row)

  # end of loop across row
  }

  # return updated dataframe
  return(df)
}
