#' @export
merModgANOVA <- setClass("merModgANOVA", contains = c("merMod", "lmerMod","merModLmerTest"))