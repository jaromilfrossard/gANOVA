#' ranef method for lmerModgANOVA object
#'
#' @description Extract the modes of random effects (BLUP) of the model. Just small changes from the ranef.merMod method of lme4 to handle lmerModgANOVA.
#' @param object a lmerModgANOVA object.
#' @param condVar see lmer.
#' @param drop see lmer.
#' @param whichel see lmer.
#' @param postVar see lmer.
#' @param ... see lmer.
#' @importFrom Matrix rBind
#' @importFrom lme4 getME rePos
#' @export
ranef.lmerModgANOVA <- function (object, condVar = FALSE, drop = FALSE, whichel = names(ans),
                     postVar = FALSE, ...)
{
  if (length(L <- list(...)) > 0) {
    warning(paste("additional arguments to ranef.merMod ignored:",
                  paste(names(L), collapse = ", ")))
  }
  if (!missing(postVar) && missing(condVar)) {
    warning(sQuote("postVar"), " is deprecated: please use ",
            sQuote("condVar"), " instead")
    condVar <- postVar
  }
  ans <- object@pp$b(1)
  if (!is.null(object@flist)) {
    fl <- object@flist
    #levs <- lapply(fl <- object@flist, levels)
    levs = lapply(getME(object,"Ztlist"),rownames)
    asgn <- attr(fl, "assign")
    cnms <- object@cnms
    nc <- lengths(cnms)
    nb <- nc * lengths(levs)[asgn]
    nbseq <- rep.int(seq_along(nb), nb)
    ml <- split(ans, nbseq)
    for (i in seq_along(ml)) ml[[i]] <- matrix(ml[[i]], ncol = nc[i],
                                               byrow = TRUE, dimnames = list(NULL, cnms[[i]]))
    ans <- lapply(seq_along(fl), function(i) data.frame(do.call(cbind,
                                                                ml[asgn == i]), row.names = levs[[i]], check.names = FALSE))
    names(ans) <- names(fl)
    stopifnot(is(whichel, "character"))
    whchL <- names(ans) %in% whichel
    ans <- ans[whchL]
    if (condVar) {
      sigsqr <- sigma(object)^2
      rp <- rePos$new(object)
      if (any(lengths(rp$terms) > 1L)) {
        warning("conditional variances not currently available via ",
                "ranef when there are multiple terms per factor")
      }
      else {
        vv <- .Call(lme4:::merPredDcondVar, object@pp$ptr(),
                    as.environment(rp))
        for (i in names(ans)) attr(ans[[i]], "postVar") <- vv[[i]] *
            sigsqr
      }
    }
    if (drop)
      ans <- lapply(ans, function(el) {
        if (ncol(el) > 1)
          return(el)
        pv <- drop(attr(el, "postVar"))
        el <- drop(as.matrix(el))
        if (!is.null(pv))
          attr(el, "postVar") <- pv
        el
      })
    class(ans) <- "ranef.mer"
  }
  ans
}
