gANOVA_mkReTrms = function (bars, fr, drop.unused.levels = TRUE) {
  if (!length(bars))
    stop("No random effects terms specified in formula",
         call. = FALSE)
  stopifnot(is.list(bars), vapply(bars, is.language, NA), inherits(fr,
                                                                   "data.frame"))
  names(bars) <- lme4:::barnames(bars)
  term.names <- vapply(bars, lme4:::safeDeparse, "")
  blist <- lapply(bars, gANOVA_mkBlist, fr, drop.unused.levels)
  nl <- vapply(blist, `[[`, 0L, "nl")
  if (any(diff(nl) > 0)) {
    ord <- rev(order(nl))
    blist <- blist[ord]
    nl <- nl[ord]
    term.names <- term.names[ord]
  }
  Ztlist <- lapply(blist, `[[`, "sm")
  Zt <- do.call(rBind, Ztlist)
  names(Ztlist) <- term.names
  q <- nrow(Zt)
  cnms <- lapply(blist, `[[`, "cnms")
  nc <- lengths(cnms)
  nth <- as.integer((nc * (nc + 1))/2)
  nb <- nc * nl
  if (sum(nb) != q) {
    stop(sprintf("total number of RE (%d) not equal to nrow(Zt) (%d)",
                 sum(nb), q))
  }
  boff <- cumsum(c(0L, nb))
  thoff <- cumsum(c(0L, nth))
  Lambdat <- t(do.call(sparseMatrix, do.call(rBind, lapply(seq_along(blist),
                                                           function(i) {
                                                             mm <- matrix(seq_len(nb[i]), ncol = nc[i], byrow = TRUE)
                                                             dd <- diag(nc[i])
                                                             ltri <- lower.tri(dd, diag = TRUE)
                                                             ii <- row(dd)[ltri]
                                                             jj <- col(dd)[ltri]
                                                             data.frame(i = as.vector(mm[, ii]) + boff[i], j = as.vector(mm[,
                                                                                                                            jj]) + boff[i], x = as.double(rep.int(seq_along(ii),
                                                                                                                                                                  rep.int(nl[i], length(ii))) + thoff[i]))
                                                           }))))
  thet <- numeric(sum(nth))
  ll <- list(Zt = drop0(Zt), theta = thet, Lind = as.integer(Lambdat@x),
             Gp = unname(c(0L, cumsum(nb))))
  ll$lower <- -Inf * (thet + 1)
  ll$lower[unique(diag(Lambdat))] <- 0
  ll$theta[] <- is.finite(ll$lower)
  Lambdat@x[] <- ll$theta[ll$Lind]
  ll$Lambdat <- Lambdat
  fl <- lapply(blist, `[[`, "ff")
  fnms <- names(fl)
  if (length(fnms) > length(ufn <- unique(fnms))) {
    fl <- fl[match(ufn, fnms)]
    asgn <- match(fnms, ufn)
  }
  else asgn <- seq_along(fl)
  names(fl) <- ufn
  attr(fl, "assign") <- asgn
  ll$flist <- fl
  ll$cnms <- cnms
  ll$Ztlist <- Ztlist
  ll
}
