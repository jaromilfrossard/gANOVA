% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gANOVA_lFormula.R
\name{gANOVA_lFormula}
\alias{gANOVA_lFormula}
\title{lFormula for gANOVA correlation structure}
\usage{
gANOVA_lFormula(
  formula,
  data = NULL,
  REML = TRUE,
  subset,
  weights,
  na.action,
  offset,
  contrasts = NULL,
  control = lmerControl(),
  ...
)
}
\arguments{
\item{formula}{a \code{lme4} formula. A formula of type \code{(1|id:g)} where \code{id} is the grouping variable and \code{g} is a factor will have the covariance structure will estimate the same variance for all orthonormal (\code{contr.poly}) contrasts in \code{g}. WARNINGS The identifier of the grouping variable \code{id} must be written as the first terms to the right of the \code{"|"} because all other terms will be "reduced" by an orthonormal contrasts. See link{gANOVA} for the reduced notation.}

\item{data}{a data frame. See \code{\link{lmer}} for more details.}

\item{REML}{a logical that indicate which criterion to optimize. See \code{\link{lmer}} for more details.}

\item{subset}{an expression to selecte a subset of the data. See \code{\link{lmer}} for more details.}

\item{weights}{an optional vector of weights. See \code{\link{lmer}} for more details.}

\item{na.action}{a function that handle \code{NA}'s. See \code{\link{lmer}} for more details.}

\item{offset}{specify a priori component in the predictor. See \code{\link{lmer}} for more details.}

\item{contrasts}{a list of contrasts. See \code{\link{lmer}} for more details.}

\item{control}{Some parameters. See \code{\link{lmerControl}} or \code{\link{lmer}} for more details.}

\item{...}{See \code{\link{lFormula}} or \code{\link{lmer}} for more details.}
}
\description{
Construction of the matrices from a formula object, modification of the \code{lme4} function \code{link{lFormula}}.
}
\seealso{
Other helper: 
\code{\link{lmerModgANOVA-class}}
}
\concept{helper}
