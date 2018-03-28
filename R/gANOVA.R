#' Linear Mixed Models with same variance for contrasts.
#'
#' @description This function provide a small modification of the \code{lme4} function \code{link{lmer}} in order to estimated mixed models with the same variance for orthonormal contrasts. Check formula parameter for details.
#' @param formula a \code{lme4} formula. If you provide a formula of type \code{(1|id:g)} where \code{id} is the grouping variable and \code{g} is a factor then the covariance structure will estimate the same variance for all orthonormal (\code{contr.poly}) contrasts in \code{g}. WARNINGS The identifier of the grouping variable \code{id} must be written as the first terms to the right of the \code{"|"} because all other terms will be "reduced" by an orthonormal contrasts. See Details for the reduced notation.
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
#' @details \code{summary} and \code{anova} method are copied from the \code{lmerTest} package in order to have p-values.
#' Given 2 factors, \code{f} and \code{g}, and a grouping variable \code{id}, the formula \code{(1|id) + (1|id:f)+ (1|id:g)+ (1|id:f:g)} can be reduced to  \code{(1|id|f*g)}. However the grouping variable (herer \code{id}) should be written as one variable.
#' @seealso \code{\link{lmer}}.
#' @importClassesFrom lmerTest merModLmerTest
#' @importClassesFrom lme4 merMod
#' @importFrom methods as is new
#' @importFrom stats as.formula formula getCall model.matrix sigma terms update.formula
#' @importFrom lme4 lmerControl glmerControl mkLmerDevfun optimizeLmer checkConv
#' @importFrom lmerTest summary
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
  mc[[1]] <- quote(gANOVA::gANOVA_lFormula)
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
    s <- lme4:::getStart(start, environment(devfun)$lower, environment(devfun)$pp)
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
  model = lme4::mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr,
           mc = mcout, lme4conv = cc)
  if (inherits(model, "merMod")){
    model <- as(model, c("merModLmerTest"))
    model <- as(model, c("merModgANOVA"))
    }
  model
}
