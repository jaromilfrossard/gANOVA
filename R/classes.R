#' The MerModgANOVA class

#' @importClassesFrom lme4 merMod lmerMod
#' @importClassesFrom lmerTest lmerModLmerTest
#' @exportClass merModgANOVA
setClass("merModgANOVA", contains = c("merMod", "lmerMod","lmerModLmerTest"))