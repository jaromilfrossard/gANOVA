update.merModgANOVA <- function (object, formula., ..., evaluate = TRUE,input = NULL) {
  if (is.null(call <- getCall(object)))
    stop("object should contain a 'call' component")
  extras <- match.call(expand.dots = FALSE)$...
  if (!missing(formula.))
    call$formula <- update.formula(formula(object), formula.)
  if (length(extras) > 0) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) {
    with(input,{
      ff <- environment(formula(object))
      pf <- parent.frame()
      sf <- sys.frames()[[1]]
      ff = list2env(input,envir = ff)

      tryCatch(eval(call, envir = ff), error = function(e) {
        tryCatch(eval(call, envir = sf), error = function(e) {
          eval(call, pf)
        })
      })})
  }
  else call
}
