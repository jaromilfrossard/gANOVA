#' The MerModgANOVA class

#' @exportClass merModgANOVA
setClass("merModgANOVA", contains = c("merMod", "lmerMod","merModLmerTest"))