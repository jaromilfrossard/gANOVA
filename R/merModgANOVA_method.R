#' summary method for merModgANVOA
#'
#' @description modified summary method from lmerTest package
#'
#' @param object an object of class merModgANOVA.
#' @param ddf a character string indicating the method. Default is \code{"Satterthwaite"} and \code{"Kenward-Roger"} is available. See details.
#' @param ... futher argument. See details.
#' @details See the lmerTest package for more informations.
#' @importMethodsFrom lmerTest summary
#' @export summary
setMethod("summary", signature(object = "merModgANOVA"),
          function(object, ddf="Satterthwaite", ...){
            if(!is.null(ddf) && ddf=="lme4"){
              if(class(object) == "merModgANOVA")
                return(summary(as(object, "lmerMod")))
              #return(cl)
            }else{
              ## commented callNextMethod
              ## since it produces warning, summary cannot have multiple arguments
              ##cl <- callNextMethod()
              if(class(object) == "merModgANOVA")
                cl <- summary(as(object, "lmerMod"))
              #errors in specifying the parameters
              ddf <- lmerTest:::checkNameDDF(ddf)

              tsum <- tryCatch( {lmerTest:::calcSummary(object, ddf)},
                                error = function(e) { NULL })
              if(is.null(tsum)){
                message("summary from lme4 is returned\nsome computational error has occurred in lmerTest")
                return(cl)
              }
              coefs.satt <- cbind(cl$coefficients[,1:2, drop = FALSE], tsum$df,
                                  tsum$tvalue, tsum$tpvalue)
              cl$coefficients <- coefs.satt
              colnames(cl$coefficients)[3:5] <- c("df","t value","Pr(>|t|)")
            }

            cl$methTitle <- paste(cl$methTitle,  "\nt-tests use ", ddf,
                                  "approximations to degrees of freedom")
            return(cl)
          })

#' anova method for merModgANVOA
#'
#' @description modified anova method from lmerTest package
#'
#' @param object an object of class merModgANOVA.
#' @param ddf a character string indicating the method. Default is \code{"Satterthwaite"} and \code{"Kenward-Roger"} is available. See details.
#' @param type a integer indicating the equivalent type of SS. Default is 3.
#' @param ... futher argument. See details.
#' @details See the lmerTest package for more informations.
#' @importFrom methods callNextMethod
#' @importFrom utils as.roman
#' @export anova
setMethod("anova", signature(object="merModgANOVA"),
          function(object, ..., ddf="Satterthwaite", type=3)
          {
            mCall <- match.call(expand.dots = TRUE)
            dots <- list(...)
            modp <- if (length(dots))
              sapply(dots, is, "merModgANOVA") | sapply(dots, is, "merMod") |
              sapply(dots, is, "lm") else logical(0)
            if (any(modp)) {
              return(callNextMethod())
            }
            else
            {
              cnm <- callNextMethod()
              if(!is.null(ddf) &&  ddf=="lme4")
                return(cnm)
              {
                table <- cnm

                ## errors in specifying the parameters
                ddf <- lmerTest:::checkNameDDF(ddf)
                an.table <- tryCatch({lmerTest:::calcANOVA(model=object, ddf=ddf, type=type)}
                                     , error = function(e) { NULL })
                if(!is.null(an.table))
                {
                  table <- an.table

                  attr(table, "heading") <-
                    paste("Analysis of Variance Table of type", as.roman(type) ,
                          " with ", ddf,
                          "\napproximation for degrees of freedom")
                }
                else
                  message("anova from lme4 is returned\nsome computational error has occurred in lmerTest")



                class(table) <- c("anova", "data.frame")
                return(table)
                }

            }

          })

