#' Remove and Rank observations in a dataframe
#'
#' `rerank()` takes a dataframe, a column name (numeric)
#' and a threshold value for filtering and a string that
#' specifies whether to remove values above or below the
#' threshold value
#'
#' @param df A dataframe
#' @param var Variable name. Can be quoted or unquoted
#' @param thresh Numeric value indicating number to use to
#' filter the data
#' @param upper Logical (TRUE/FALSE) indicating wheter to
#' filter values above the threshold. Default is `TRUE`
#' @param inclusive Logical (TRUE/FALSE) indicating wheter to
#' Include the threshold value in filtering
#' @param ascending Logical (TRUE/FALSE) indicating wheter to
#' Rank the rows by the `var` in ascending sequence
#'
#' @return dataframe that is filtered and ranked
#'
#' @export
rerank <- function(df,
                   var,
                   thresh,
                   upper=TRUE,
                   inclusive=TRUE,
                   ascending=TRUE
){
  assert_that(is.numeric(thresh))
  assert_that(is.logical(upper))

  # for unquoted variable name
  if(class(substitute(var)) == "name") {
    # for filtering values above threshold
    if(upper) {
      # for including threshold
      if(inclusive){
        # for ranking by ascending
        if(ascending){
          df <- df[df[,deparse(substitute(var))] >= thresh,]
          df[order(df[,deparse(substitute(var))]),]
        } else {
          df <- df[df[,deparse(substitute(var))] >= thresh,]
          df[order(df[,deparse(substitute(var))], decreasing = TRUE),]
        }
      } else {
        if(ascending){
          df <- df[df[,deparse(substitute(var))] > thresh,]
          df[order(df[,deparse(substitute(var))]),]
        } else {
          df <- df[df[,deparse(substitute(var))] > thresh,]
          df[order(df[,deparse(substitute(var))], decreasing = TRUE),]
        }
      }
    } else {
      # for including threshold
      if(inclusive){
        # for ranking by ascending
        if(ascending){
          df <- df[df[,deparse(substitute(var))] <= thresh,]
          df[order(df[,deparse(substitute(var))]),]
        } else {
          df <- df[df[,deparse(substitute(var))] <= thresh,]
          df[order(df[,deparse(substitute(var))], decreasing = TRUE),]
        }
      } else {
        if(ascending){
          df <- df[df[,deparse(substitute(var))] < thresh,]
          df[order(df[,deparse(substitute(var))]),]
        } else {
          df <- df[df[,deparse(substitute(var))] < thresh,]
          df[order(df[,deparse(substitute(var))], decreasing = TRUE),]
        }
      }
    }
  # for quoted variable name
  } else {
    # for filtering values above threshold
    if(upper) {
      # for including threshold
      if(inclusive){
        # for ranking by ascending
        if(ascending){
          df <- df[df[,var] >= thresh,]
          df[order(df[,var]),]
        } else {
          df <- df[df[,var] >= thresh,]
          df[order(df[,var], decreasing = TRUE),]
        }
      } else {
        if(ascending){
          df <- df[df[,var] > thresh,]
          df[order(df[,var]),]
        } else {
          df <- df[df[,var] > thresh,]
          df[order(df[,var], decreasing = TRUE),]
        }
      }
    } else {
      # for including threshold
      if(inclusive){
        # for ranking by ascending
        if(ascending){
          df <- df[df[,var] <= thresh,]
          df[order(df[,var]),]
        } else {
          df <- df[df[,var] <= thresh,]
          df[order(df[,var], decreasing = TRUE),]
        }
      } else {
        if(ascending){
          df <- df[df[,var] < thresh,]
          df[order(df[,var]),]
        } else {
          df <- df[df[,var] < thresh,]
          df[order(df[,var], decreasing = TRUE),]
        }
      }
    }
  }
}
