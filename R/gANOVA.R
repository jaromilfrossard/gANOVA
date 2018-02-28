#' Linear Mixed Models with same variance for contrasts.
#'
#' @description This function provide a small modification of the lmer function in order to estimated mixed models with the same variance for orthonormal contrasts. Check formula parameter for details.
#' @param formula a lme4 formula. If you provide f formula of type (1|id:g) where, g is a factor then the same variance will be estimated for all orthonormal contrasts in g.
#' @param data a data frame. See \code{\link{lmer}} for more details.
#' @param REML a logical that indicate which criterion to optimize. See \code{\link{lmer}} for more details.
#' @param control Some parameters. See \link{lmerControl} or \code{\link{lmer}} for more details.
#' @param start starting values for the paramters. See \code{\link{lmer}} for more details.
#' @param verbose See \code{\link{lmer}} for more details.
#' @param subset an expression to selecte a subset of the data. See \code{\link{lmer}} for more details.
#' @param weights an optional vector of weights. See \code{\link{lmer}} for more details.
#' @param na.action a function that handle \code{NA}'s. See \code{\link{lmer}} for more details.
#' @param offset specify a priori component in the predictor. See \code{\link{lmer}} for more details.
#' @param contrasts a list of contrasts. See \code{\link{lmer}} for more details.
#' @param devFunOnly a logical set by default to \code{FALSE}. See \code{\link{lmer}} for more details.
#' @param ... addition arguments. See \code{\link{lmer}} for more details.
#' @seealso \code{\link{lmer}}.
#' @export
gANOVA <- function (formula, data = NULL, REML = TRUE, control = lmerControl(),
                    start = NULL, verbose = 0L, subset, weights, na.action, offset,
                    contrasts = NULL, devFunOnly = FALSE, ...)
{
  mc <- mcout <- match.call()
  missCtrl <- missing(control)
  if (!missCtrl && !inherits(control, "lmerControl")) {
    if (!is.list(control))
      stop("'control' is not a list; use lmerControl()")
    warning("passing control as list is deprecated: please use lmerControl() instead",
            immediate. = TRUE)
    control <- do.call(lmerControl, control)
  }
  if (!is.null(list(...)[["family"]])) {
    warning("calling lmer with 'family' is deprecated; please use glmer() instead")
    mc[[1]] <- quote(lme4::glmer)
    if (missCtrl)
      mc$control <- glmerControl()
    return(eval(mc, parent.frame(1L)))
  }
  mc$control <- control
  mc[[1]] <- quote(gANOVA_lFormula)
  lmod <- eval(mc, parent.frame(1L))
  mcout$formula <- lmod$formula
  lmod$formula <- NULL

  devfun <- do.call(mkLmerDevfun, c(lmod, list(start = start,
                                               verbose = verbose, control = control)))
  if (devFunOnly)
    return(devfun)
  if (identical(control$optimizer, "none"))
    stop("deprecated use of optimizer=='none'; use NULL instead")
  opt <- if (length(control$optimizer) == 0) {
    s <- getStart(start, environment(devfun)$lower, environment(devfun)$pp)
    list(par = s, fval = devfun(s), conv = 1000, message = "no optimization")
  }
  else {
    optimizeLmer(devfun, optimizer = control$optimizer, restart_edge = control$restart_edge,
                 boundary.tol = control$boundary.tol, control = control$optCtrl,
                 verbose = verbose, start = start, calc.derivs = control$calc.derivs,
                 use.last.params = control$use.last.params)
  }
  cc <- checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,
                  lbound = environment(devfun)$lower)

  # mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr,
  #          mc = mcout, lme4conv = cc)
  model = mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr,
           mc = mcout, lme4conv = cc)
  if (inherits(model, "merMod"))
    model <- as(model, "merModLmerTest")
  model
}
