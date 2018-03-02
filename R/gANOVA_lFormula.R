#' lFormula for gANOVA correlation structure
#'
#' @description This function provide a small modification of the \code{lme4} function \code{link{lFormula}} in order to estimated mixed models with the same variance for orthonormal contrasts. Check formula parameter for details.
#' @param formula a \code{lme4} formula. If you provide f formula of type \code{(1|id:g)} where \code{id} is the grouping variable and \code{g} is a factor then the covariance structure will estimate the same variance for all orthonormal (\code{contr.poly}) contrasts in \code{g}. WARNINGS The indifier of the grouping variable \code{id} must be written as the first terms to the right of the \code{"|"} because all other terms will be "reduced" by an orthonormal contrasts.
#' @param data a data frame. See \code{\link{lmer}} for more details.
#' @param REML a logical that indicate which criterion to optimize. See \code{\link{lmer}} for more details.
#' @param subset an expression to selecte a subset of the data. See \code{\link{lmer}} for more details.
#' @param weights an optional vector of weights. See \code{\link{lmer}} for more details.
#' @param na.action a function that handle \code{NA}'s. See \code{\link{lmer}} for more details.
#' @param offset specify a priori component in the predictor. See \code{\link{lmer}} for more details.
#' @param contrasts a list of contrasts. See \code{\link{lmer}} for more details.
#' @param control Some parameters. See \code{\link{lmerControl}} or \code{\link{lmer}} for more details.
#' @param ... See \code{\link{lFormula}} or \code{\link{lmer}} for more details.
#' @importFrom Matrix rankMatrix
#' @importFrom lme4 expandDoubleVerts subbars factorize findbars nobars
#' @export
gANOVA_lFormula <- function(formula, data = NULL, REML = TRUE, subset, weights,
            na.action, offset, contrasts = NULL, control = lmerControl(),
            ...){
    control <- control$checkControl
    mf <- mc <- match.call()
    ignoreArgs <- c("start", "verbose", "devFunOnly", "control")
    l... <- list(...)
    l... <- l...[!names(l...) %in% ignoreArgs]
    do.call(lme4:::checkArgs, c(list("lmer"), l...))
    if (!is.null(list(...)[["family"]])) {
      mc[[1]] <- quote(lme4::glFormula)
      if (missing(control))
        mc[["control"]] <- glmerControl()
      return(eval(mc, parent.frame()))
    }
    cstr <- "check.formula.LHS"
    lme4:::checkCtrlLevels(cstr, control[[cstr]])
    denv <- lme4:::checkFormulaData(formula, data, checkLHS = control$check.formula.LHS ==
                               "stop")
    formula <- as.formula(formula, env = denv)
    lme4:::RHSForm(formula) <- expandDoubleVerts(lme4:::RHSForm(formula))
    mc$formula <- formula
    m <- match(c("data", "subset", "weights", "na.action", "offset"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    fr.form <- subbars(formula)
    environment(fr.form) <- environment(formula)
    for (i in c("weights", "offset")) {
      if (!eval(bquote(missing(x = .(i)))))
        assign(i, get(i, parent.frame()), environment(fr.form))
    }
    mf$formula <- fr.form
    fr <- eval(mf, parent.frame())
    fr <- factorize(fr.form, fr, char.only = TRUE)
    attr(fr, "formula") <- formula
    attr(fr, "offset") <- mf$offset
    n <- nrow(fr)
    reTrms <- gANOVA_mkReTrms(findbars(lme4:::RHSForm(formula)), fr)
    wmsgNlev <- lme4:::checkNlevels(reTrms$flist, n = n, control)
    wmsgZdims <- lme4:::checkZdims(reTrms$Ztlist, n = n, control, allow.n = FALSE)
    if (anyNA(reTrms$Zt)) {
      stop("NA in Z (random-effects model matrix): ", "please use ",
           shQuote("na.action='na.omit'"), " or ", shQuote("na.action='na.exclude'"))
    }
    wmsgZrank <- lme4:::checkZrank(reTrms$Zt, n = n, control, nonSmall = 1e+06)
    fixedform <- formula
    lme4:::RHSForm(fixedform) <- nobars(lme4:::RHSForm(fixedform))
    mf$formula <- fixedform
    fixedfr <- eval(mf, parent.frame())
    attr(attr(fr, "terms"), "predvars.fixed") <- attr(attr(fixedfr,
                                                           "terms"), "predvars")
    ranform <- formula
    lme4:::RHSForm(ranform) <- subbars(lme4:::RHSForm(lme4:::reOnly(formula)))
    mf$formula <- ranform
    ranfr <- eval(mf, parent.frame())
    attr(attr(fr, "terms"), "predvars.random") <- attr(terms(ranfr),
                                                       "predvars")
    X <- model.matrix(fixedform, fr, contrasts)
    if (is.null(rankX.chk <- control[["check.rankX"]]))
      rankX.chk <- eval(formals(lmerControl)[["check.rankX"]])[[1]]
    X <- lme4:::chkRank.drop.cols(X, kind = rankX.chk, tol = 1e-07)
    if (is.null(scaleX.chk <- control[["check.scaleX"]]))
      scaleX.chk <- eval(formals(lmerControl)[["check.scaleX"]])[[1]]
    X <- lme4:::checkScaleX(X, kind = scaleX.chk)
    list(fr = fr, X = X, reTrms = reTrms, REML = REML, formula = formula,
         wmsgs = c(Nlev = wmsgNlev, Zdims = wmsgZdims, Zrank = wmsgZrank))
}

