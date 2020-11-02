
library(ggplot2)
library(tidyverse)
#' Payment Boxplot
#'
#'Creates a boxplot for the desired variable
#' @param df a data frame
#' @param pay_type a string name for variable in data frame df
#'
#' @return A boxplot of the desired variable
#' @export
#'
#' @examples
#' load(lab_data)
#' pay_boxplot(lab2_data,"Average.Covered.Charges")

pay_boxplot<-function(df,pay_type){
  names(pay_type)<-gsub("."," ",pay_type,fixed=TRUE)
  ggplot(df,aes(y=.data[[pay_type]]))+
    geom_boxplot()+
    labs(title=paste("Boxplot of ",names(pay_type)),y=names(pay_type))
}
