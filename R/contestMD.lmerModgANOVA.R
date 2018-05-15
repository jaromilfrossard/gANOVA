# contestMD.lmerModgANOVA <- function (model, L, rhs = 0, ddf = c("Satterthwaite", "Kenward-Roger"),
#                               eps = sqrt(.Machine$double.eps), ...)
# {
#   mk_Ftable <- function(Fvalue, ndf, ddf, sigma, Fscale = 1) {
#     MS <- Fvalue * sigma^2
#     Fvalue <- Fvalue * Fscale
#     pvalue <- pf(q = Fvalue, df1 = ndf, df2 = ddf, lower.tail = FALSE)
#     data.frame(`Sum Sq` = MS * ndf, `Mean Sq` = MS, NumDF = ndf,
#                DenDF = ddf, `F value` = Fvalue, `Pr(>F)` = pvalue,
#                check.names = FALSE)
#   }
#   if (!is.matrix(L))
#     L <- matrix(L, ncol = length(L))
#   stopifnot(is.matrix(L), is.numeric(L), ncol(L) == length(model@beta))
#   if (length(rhs) == 1L)
#     rhs <- rep(rhs, nrow(L))
#   stopifnot(is.numeric(rhs), length(rhs) == nrow(L))
#   method <- match.arg(ddf)
#   if (nrow(L) == 0L) {
#     x <- numeric(0L)
#     return(mk_Ftable(x, x, x, x))
#   }
#   if (any(is.na(L)))
#     return(mk_Ftable(NA_real_, NA_real_, NA_real_, NA_real_))
#   if (method == "Kenward-Roger") {
#     if (!getME(model, "is_REML"))
#       stop("Kenward-Roger's method is only available for REML model fits",
#            call. = FALSE)
#     if (!requireNamespace("pbkrtest", quietly = TRUE))
#       stop("pbkrtest package required for Kenward-Roger's method",
#            call. = FALSE)
#     if (getRversion() < "3.3.2")
#       warning("Kenward-Roger may give faulty results with R <= 3.3.2")
#     if (qr(L)$rank < nrow(L) && !all(rhs == 0))
#       warning("Contrast is rank deficient and test may be affected")
#     betaH <- if (all(rhs == 0))
#       0
#     else drop(MASS::ginv(L) %*% rhs)
#     x <- try(pbkrtest::KRmodcomp(model, L, betaH = betaH)$test,
#              silent = TRUE)
#     if (inherits(x, "try-error")) {
#       warning("Unable to compute Kenward-Roger F-test: using Satterthwaite instead",
#               call. = FALSE)
#       if (!inherits(model, "lmerModLmerTest"))
#         model <- as_lmerModLmerTest(model)
#     }
#     else {
#       return(mk_Ftable(Fvalue = x["FtestU", "stat"], ndf = x[1L,
#                                                              "ndf"], ddf = x[1L, "ddf"], sigma = sigma(model),
#                        Fscale = x["Ftest", "F.scaling"]))
#     }
#   }
#   if (nrow(L) == 1L) {
#     res <- contest1D(model, drop(L), rhs = rhs, confint = FALSE)
#     return(mk_Ftable(Fvalue = res[["t value"]]^2, ndf = 1L,
#                      ddf = res$df, sigma = model@sigma))
#   }
#   beta <- model@beta
#   if (!all(rhs == 0))
#     beta <- beta - drop(MASS::ginv(L) %*% rhs)
#   VLbeta <- L %*% model@vcov_beta %*% t(L)
#   eig_VLbeta <- eigen(VLbeta)
#   P <- eig_VLbeta$vectors
#   d <- eig_VLbeta$values
#   tol <- max(eps * d[1], 0)
#   pos <- d > tol
#   q <- sum(pos)
#   if (q < nrow(L) && !all(rhs == 0))
#     warning("Contrast is rank deficient and test may be affected")
#   if (q <= 0) {
#     x <- numeric(0L)
#     return(mk_Ftable(x, x, x, x))
#   }
#   PtL <- crossprod(P, L)[1:q, ]
#   if (q == 1) {
#     res <- contest1D(model, PtL, rhs = rhs[1L], confint = FALSE)
#     return(mk_Ftable(Fvalue = res[["t value"]]^2, ndf = q,
#                      ddf = res$df, sigma = model@sigma))
#   }
#   t2 <- drop(PtL %*% beta)^2/d[1:q]
#   Fvalue <- sum(t2)/q
#   grad_PLcov <- lapply(1:q, function(m) {
#     vapply(model@Jac_list, function(J) qform(PtL[m, ], J),
#            numeric(1L))
#   })
#   nu_m <- vapply(1:q, function(m) {
#     2 * (d[m])^2/qform(grad_PLcov[[m]], model@vcov_varpar)
#   }, numeric(1L))
#   ddf <- get_Fstat_ddf(nu_m, tol = 1e-08)
#   mk_Ftable(Fvalue, ndf = q, ddf = ddf, sigma = model@sigma)
# }