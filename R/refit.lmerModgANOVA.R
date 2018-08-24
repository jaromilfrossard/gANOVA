#' Refit a lmerModgANOVA model with a new response
#'
#' @description Refit a lmerModgANOVA model with a new response
#'
#' @param object a lmerModgANOVA model
#' @param newresp a vector of new responses.
#' @param rename.response Replacing the name of in the forumla. Default is FALSE
#' @param maxit a integer, not use. Default is 100.
#' @param ... orther arguments
#' @seealso \code{\link{refit}}
#' @export
#' @importFrom lme4 isGLMM isLMM isREML GHrule fixef mkMerMod mkRespMod
#' @importFrom stats model.frame family
refit.lmerModgANOVA <-function (object, newresp = NULL, rename.response = FALSE, maxit = 100L,
            ...)
  {
    l... <- list(...)
    ctrl.arg <- NULL
    if ("control" %in% names(l...))
      ctrl.arg <- l...$control
    if (!all(names(l...) %in% c("control", "verbose"))) {
      warning("additional arguments to refit.merMod ignored")
    }
    newrespSub <- substitute(newresp)
    if (is.list(newresp)) {
      if (length(newresp) == 1) {
        na.action <- attr(newresp, "na.action")
        newresp <- newresp[[1]]
        attr(newresp, "na.action") <- na.action
      }
      else {
        stop("refit not implemented for 'newresp' lists of length > 1: ",
             "consider ", sQuote("lapply(object,refit)"))
      }
    }
    control <- if (!is.null(ctrl.arg)) {
      if (length(ctrl.arg$optCtrl) == 0) {
        obj.control <- object@optinfo$control
        ignore.pars <- c("xst", "xt")
        if (any(ign <- names(obj.control) %in% ignore.pars))
          obj.control <- obj.control[!ign]
        ctrl.arg$optCtrl <- obj.control
      }
      ctrl.arg
    }
    else if (isGLMM(object)){
      #glmerControl()
    } else lmerControl()
    if (object@optinfo$optimizer == "optimx") {
      control$optCtrl <- object@optinfo$control
    }
    pp <- object@pp$copy()
    dc <- object@devcomp
    nAGQ <- dc$dims["nAGQ"]
    nth <- dc$dims[["nth"]]
    verbose <- l...$verbose
    if (is.null(verbose))
      verbose <- 0L
    if (!is.null(newresp)) {
      rcol <- attr(attr(model.frame(object), "terms"), "response")
      if (rename.response) {
        attr(object@frame, "formula")[[2]] <- object@call$formula[[2]] <- newrespSub
        names(object@frame)[rcol] <- deparse(newrespSub)
      }
      if (!is.null(na.act <- attr(object@frame, "na.action")) &&
          is.null(attr(newresp, "na.action"))) {
        newresp <- if (is.matrix(newresp))
          newresp[-na.act, ]
        else newresp[-na.act]
      }
      object@frame[, rcol] <- newresp
    }
    rr <- if (isLMM(object))
    {mkRespMod(model.frame(object), REML = isREML(object))
      }
    else if (isGLMM(object)) {
      #mkRespMod(model.frame(object), family = family(object))
    }
    else stop("refit.merMod not working for nonlinear mixed models.\n",
              "try update.merMod instead.")
    if (!is.null(newresp)) {
      if (family(object)$family == "binomial") {
        if (is.matrix(newresp) && ncol(newresp) == 2) {
          ntot <- rowSums(newresp)
          newresp <- newresp[, 1]/ntot
          rr$setWeights(ntot)
        }
        if (is.factor(newresp)) {
          newresp <- as.numeric(newresp) - 1
        }
      }
      stopifnot(length(newresp <- as.numeric(as.vector(newresp))) ==
                  length(rr$y))
    }
    if (isGLMM(object)) {
      GQmat <- GHrule(nAGQ)
      if (nAGQ <= 1) {
        # glmerPwrssUpdate(pp, rr, control$tolPwrss, GQmat,
        #                  maxit = maxit)
      }
      else {
        # glmerPwrssUpdate(pp, rr, control$tolPwrss, GQmat,
        #                  maxit = maxit, grpFac = object@flist[[1]])
      }
    }
    devlist <- if (isGLMM(object)) {
      # baseOffset <- forceCopy(object@resp$offset)
      # list(tolPwrss = dc$cmp[["tolPwrss"]], compDev = dc$dims[["compDev"]],
      #      nAGQ = unname(nAGQ), lp0 = pp$linPred(1), baseOffset = baseOffset,
      #      pwrssUpdate = glmerPwrssUpdate, GQmat = GHrule(nAGQ),
      #      fac = object@flist[[1]], pp = pp, resp = rr, u0 = pp$u0,
      #      verbose = verbose, dpars = seq_len(nth))
    }
    else list(pp = pp, resp = rr, u0 = pp$u0, verbose = verbose,
              dpars = seq_len(nth))
    ff <- lme4:::mkdevfun(list2env(devlist), nAGQ = nAGQ, maxit = maxit,
                   verbose = verbose)
    xst <- rep.int(0.1, nth)
    if(is.null(l...$start)){x0 <- pp$theta}else{x0<-l...$start}

    lower <- object@lower
    if (!is.na(nAGQ) && nAGQ > 0L) {
      xst <- c(xst, sqrt(diag(pp$unsc())))
      x0 <- c(x0, unname(fixef(object)))
      lower <- c(lower, rep(-Inf, length(x0) - length(lower)))
    }
    calc.derivs <- !is.null(object@optinfo$derivs)
    opt <- lme4:::optwrap(object@optinfo$optimizer, ff, x0, lower = lower,
                   control = control$optCtrl, calc.derivs = calc.derivs)
    cc <- checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,
                    lbound = lower)
    if (isGLMM(object)){
      #rr$setOffset(baseOffset)
    }

    res <- mkMerMod(environment(ff), opt, list(flist = object@flist,
                                        cnms = object@cnms, Gp = object@Gp, lower = object@lower),
             object@frame, getCall(object), cc)
    res = lmerTest:::as_lmerModLT(res, ff )
    res@call <- object@call
    res <- as(res, c("lmerModgANOVA"))
    res
  }
