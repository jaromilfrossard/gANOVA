#' @importFrom stats contr.poly
#' @importFrom Matrix fac2sparse KhatriRao Matrix
#' @importFrom lme4 factorize
gANOVA_mkBlist = function (x, frloc, drop.unused.levels = TRUE) {
  frloc <- factorize(x, frloc)
  interlogical = length(x[[3]])>1

  if (is.null(ff <- tryCatch(eval(substitute(lme4:::makeFac(fac),
                                             list(fac = x[[3]])), frloc), error = function(e) NULL)))
    stop("couldn't evaluate grouping factor ", deparse(x[[3]]),
         " within model frame:", " try adding grouping factor to data ",
         "frame explicitly if possible", call. = FALSE)
  if (all(is.na(ff)))
    stop("Invalid grouping factor specification, ", deparse(x[[3]]),
         call. = FALSE)
  if (drop.unused.levels)
    ff <- factor(ff, exclude = NA)
  #nl <- length(levels(ff))
  mm <- model.matrix(eval(substitute(~foo, list(foo = x[[2]]))),
                     frloc)


  sm = fac2sparse(ff, to = "d", drop.unused.levels = drop.unused.levels)
  #print(t(sm)[1:nrow(sm),1:nrow(sm)])

  if(interlogical){
    form = paste("~",deparse(x[[3]]))
    form = gsub(":", "*", form )
    terms = terms(formula(form))
    #print(form)

    effect = attr(terms,"term.labels")[attr(terms,"order")==1]
    sm = lapply(effect,function(e){
      Matrix(model.matrix(formula(paste("~0+",e,sep="")),data = frloc),sparse = TRUE)
    })



    sm[2:length(sm)] <- lapply(sm[2:length(sm)],function(mat){
      mat%*%Matrix(contr.poly(ncol(mat)),sparse=TRUE)})
    sm = lapply(sm, function(mat){t(mat)})
    smtemp = sm[[1]]


    for(i in 2:length(sm)){
      smtemp = KhatriRao(smtemp, sm[[i]],make.dimnames = TRUE)
    }
    sm = smtemp}

    # sm = model.matrix(formula(form),data = frloc)
    # sm = sm[,attr(sm,"assign")==max(attr(sm,"assign")),drop=F]
    # sm = Matrix(t(sm),sparse = TRUE) }else{
    #   sm <- fac2sparse(ff, to = "d", drop.unused.levels = drop.unused.levels)}
  #print(t(sm)[1:nrow(sm),1:nrow(sm)])


  nl <- nrow(sm)
  sm <- KhatriRao(sm, t(mm),make.dimnames = TRUE)
  #sm@x <- sm@x / rep.int(colSums(sm), diff(sm@p))
  #dimnames(sm) <- list(rep(levels(ff), each = ncol(mm)), rownames(mm))
  list(ff = ff, sm = sm, nl = nl, cnms = colnames(mm))
}




