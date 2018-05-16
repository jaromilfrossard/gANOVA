#' summary method for merModgANVOA
#'
#' @description modified summary method from lmerTest package
#'
#' @param object an object of class lmerModgANOVA.
#' @param ddf a character string indicating the method. Default is \code{"Satterthwaite"} and \code{"Kenward-Roger"} is available. See details.
#' @param ... futher argument. See details.
#' @details See the lmerTest package for more informations.
#' @export
summary.lmerModgANOVA <- function(object, ..., ddf = c("Satterthwaite", "Kenward-Roger", "lme4")){
  lmerTest:::summary.lmerModLmerTest(object = object, ... = ... ,ddf = ddf)
}




# summary.lmerModgANOVA <- function(object, ddf="Satterthwaite", ...){
#             if(!is.null(ddf) && ddf=="lme4"){
#               if(class(object) == "lmerModgANOVA")
#                 return(summary(as(object, "lmerMod")))
#               #return(cl)
#             }else{
#               ## commented callNextMethod
#               ## since it produces warning, summary cannot have multiple arguments
#               ##cl <- callNextMethod()
#               if(class(object) == "lmerModgANOVA")
#                 cl <- summary(as(object, "lmerMod"))
#               #errors in specifying the parameters
#               ddf <- lmerTest:::checkNameDDF(ddf)
#
#               tsum <- tryCatch( {lmerTest:::calcSummary(object, ddf)},
#                                 error = function(e) { NULL })
#               if(is.null(tsum)){
#                 message("summary from lme4 is returned\nsome computational error has occurred in lmerTest")
#                 return(cl)
#               }
#               coefs.satt <- cbind(cl$coefficients[,1:2, drop = FALSE], tsum$df,
#                                   tsum$tvalue, tsum$tpvalue)
#               cl$coefficients <- coefs.satt
#               colnames(cl$coefficients)[3:5] <- c("df","t value","Pr(>|t|)")
#             }
#
#             cl$methTitle <- paste(cl$methTitle,  "\nt-tests use ", ddf,
#                                   "approximations to degrees of freedom")
#             return(cl)
#           }

