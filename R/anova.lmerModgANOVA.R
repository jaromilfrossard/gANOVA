#' #' anova method for merModgANVOA
#' #'
# #' @description modified anova method from lmerTest package
#' #'
# #' @param object an object of class lmerModgANOVA.
# #' @param ddf a character string indicating the method. Default is \code{"Satterthwaite"} and \code{"Kenward-Roger"} is available. See details.
# #' @param type a integer indicating the equivalent type of SS. Default is 3.
# #' @param ... futher argument. See details.
# #' @details See the lmerTest package for more informations.
# #' @importFrom methods callNextMethod
# #' @importFrom utils as.roman
# #' @export
#' #'
# anova.lmerModgANOVA <- function (object, ..., type = c("III", "II", "I", "3", "2", "1"),
#                                  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))
# {
#   if (!inherits(object, "lmerModgANOVA")){
#     stop("'object' of class: ", paste(class(object), collapse = ", "),
#          ". Expecting object of class 'lmerModgANOVA'")
#   }
#   dots <- list(...)
#   ddf <- match.arg(ddf)
#   if (ddf == "lme4")
#     return(anova(as(object, "lmerMod"), ...))
#   single_ganova(object = object, type = type, ddf = ddf)
# }
#
# single_ganova = function (object, type = c("III", "II", "I", "3", "2", "1", "yates",
#                                           "marginal", "2b"), ddf = c("Satterthwaite", "Kenward-Roger"))
#   {type <- type[1L]
#   if (!is.character(type))
#     type <- as.character(type)
#   type <- match.arg(type)
#   if (type %in% c("I", "II", "III"))
#     type <- as.character(as.integer(as.roman(type)))
#   ddf <- match.arg(ddf)
#   L_list <- if (type == "1") {
#     lmerTest:::get_contrasts_type1(object)
#   }
#   else if (type == "2") {
#     lmerTest:::get_contrasts_type2_unfolded(object)
#   }
#   else if (type == "2b") {
#     lmerTest:::get_contrasts_type2(object)
#   }
#   else if (type == "3") {
#     lmerTest:::get_contrasts_type3(object)
#   }
#   else if (type == "yates") {
#     lmerTest:::get_contrasts_yates(object)
#   }
#   else if (type == "marginal") {
#     lmerTest:::get_contrasts_marginal(object)
#   }
#   else {
#     stop("'type' not recognized")
#   }
#   table <- lmerTest:::rbindall(lapply(L_list, function(L) contestMD(object, L, ddf = ddf)))
#
#   if (length(nm <- setdiff(names(L_list), rownames(table)))) {
#     tab <- array(NA_real_, dim = c(length(nm), 6L), dimnames = list(nm,
#                                                                     colnames(table)))
#     table <- rbind(table, tab)
#   }
#   method <- switch(ddf, Satterthwaite = "Satterthwaite's",
#                    `Kenward-Roger` = "Kenward-Roger's")
#   type <- if (type == "marginal") {
#     "Marginal"
#   }
#   else if (type == "yates" || type == "3b") {
#     "Yates"
#   }
#   else if (grepl("b|c", type)) {
#     alph <- gsub("[0-9]", "", type)
#     paste0("Type ", as.roman(as.integer(gsub("b|c", "", type))),
#            alph)
#   }
#   else paste("Type", as.roman(as.integer(type)))
#   attr(table, "heading") <- paste(type, "Analysis of Variance Table",
#                                   "with", method, "method")
#   attr(table, "hypotheses") <- L_list
#   class(table) <- c("anova", "data.frame")
#   table
# }
#'
#'
#'
#'
#'
#'
#' # anova.lmerModgANOVA <- function(object, ..., ddf="Satterthwaite", type=3)
#' #           {
#' #             mCall <- match.call(expand.dots = TRUE)
#' #             dots <- list(...)
#' #             modp <- if (length(dots))
#' #               sapply(dots, is, "lmerModgANOVA") | sapply(dots, is, "merMod") |
#' #               sapply(dots, is, "lm") else logical(0)
#' #             if (any(modp)) {
#' #               return(callNextMethod())
#' #             }
#' #             else
#' #             {
#' #               cnm <- callNextMethod()
#' #               if(!is.null(ddf) &&  ddf=="lme4")
#' #                 return(cnm)
#' #               {
#' #                 table <- cnm
#' #
#' #                 ## errors in specifying the parameters
#' #                 ddf <- lmerTest:::checkNameDDF(ddf)
#' #                 an.table <- tryCatch({lmerTest:::calcANOVA(model=object, ddf=ddf, type=type)}
#' #                                      , error = function(e) { NULL })
#' #                 if(!is.null(an.table))
#' #                 {
#' #                   table <- an.table
#' #
#' #                   attr(table, "heading") <-
#' #                     paste("Analysis of Variance Table of type", as.roman(type) ,
#' #                           " with ", ddf,
#' #                           "\napproximation for degrees of freedom")
#' #                 }
#' #                 else
#' #                   message("anova from lme4 is returned\nsome computational error has occurred in lmerTest")
#' #
#' #
#' #
#' #                 class(table) <- c("anova", "data.frame")
#' #                 return(table)
#' #                 }
#' #
#' #             }}
#'
#'
