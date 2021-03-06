% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eqbuild.R
\name{eq_linear}
\alias{eq_linear}
\title{build a formula for fixed effect model corresponding to lists of variables
   for fixed and random effects}
\usage{
eq_linear(dv, fev, rev = NULL, morevars = NULL, addcons = TRUE)
}
\arguments{
\item{dv}{string containing the dependent variable name}

\item{fev}{vector of strings containing variable names for fixed effects}

\item{rev}{list of strings containing variable names for random effects.
Each list entry sould be a string vector containing variables
combined using the standard operators +,:,* and ^}

\item{morevars}{list of variables to include in model}

\item{addcons}{logical, if TRUE add a constant term to the fixed effects.
Overrides 0 and -1 in fev}
}
\value{
string
}
\description{
\code{eq_linear} build a formula for simple model
}
