% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/3dgenome_specific.R
\name{modelEigens.all}
\alias{modelEigens.all}
\title{Build Random Forest to predict compartments}
\usage{
modelEigens.all(all.dat, n = 500, ...)
}
\arguments{
\item{all.dat}{Numeric data.frame with one feature per col}
}
\value{
data.frame model with importance metrics
}
\description{
Uses the Random Forest library to build a model
of a properly-formatted *dat data.frame (i.e.,
contains "eigen" column (y) and all other columns
are named features).
}
\examples{
modelEigens.all(h.dat)
}

