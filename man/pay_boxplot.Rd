% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pay_boxplot.R
\name{pay_boxplot}
\alias{pay_boxplot}
\title{Payment Boxplot}
\usage{
pay_boxplot(df, pay_type)
}
\arguments{
\item{df}{a data frame}

\item{pay_type}{a string name for variable in data frame df}
}
\value{
A boxplot of the desired variable
}
\description{
Creates a boxplot for the desired variable
}
\examples{
lab2_data<-load(lab_data)
pay_boxplot(lab2_data,"Average.Covered.Charges")
}
