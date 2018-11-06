#' The MerModgANOVA class

#' @importClassesFrom lme4 merMod lmerMod
#' @importClassesFrom lmerTest lmerModLmerTest
#' @exportClass lmerModgANOVA
setClass("lmerModgANOVA", contains = c("merMod", "lmerMod","lmerModLmerTest"),
         slots = c(reTrms="list"))->lmerModgANOVA


