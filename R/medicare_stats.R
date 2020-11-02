
#' Medicare Payment Statistics
#'
#'Calculates the Statistics for DRG codes by Average Medicare Payment
#' @param df A data frame
#' @param x The desired variable to be grouped by, passed as a string
#' @param y The variable that is having its statistics calculated, as a string
#' @param stat The desired statistic being calculated, passed as a string
#'
#' @return A table of the DRG codes and the desired statistic for each
#' @export
#'
#' @examples
#' load("lab_data.Rdata")
#' medicare_stats(lab2_data,"DRG.Definition","Average.Medicare.Payments","mean")
medicare_stats<-function(df,x,y,stat){
  if(stat=="Mean" || stat=="mean"){
    me<-df %>%
      group_by(.data[[x]]) %>%
      summarize(mean=mean(.data[[y]]))
    return(me)
  } else if(stat=="Median" || stat=="median"){
    med<-df %>%
      group_by(.data[[x]]) %>%
      summarize(median=median(.data[[y]]))
    return(med)
  } else if(stat=="Standard Deviation" || stat=="Standard deviation"
            || stat=="SD" || stat=="sd"){
    stdev<-df %>%
      group_by(.data[[x]]) %>%
      summarize(SD=sd(.data[[y]]))
    return(stdev)
  }
}
