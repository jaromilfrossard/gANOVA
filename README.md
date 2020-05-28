# gANOVA

This package provides a modification of the `lmer()` function from the `lme4` package. It estimates a spherical random effects when interacting fixed factors with random samples. 

Currently `lme4` has the possibiliy to estimate the spherical random effects using `+ (1|id:f)`. However the parametrization used in lme4 puts constraints on lower interaction parameters, such that the covariance estimated using `+ (1|id) + (1|id:f)` may have the variance of `(1|id)` estimated at the boundary (or simply 0). `gANOVA` reparametrizes the covariance matrix in order to extend the range of parameters such that variance of lower interactions estimated at 0 happen less.

This new parametrization is simply transforming factors into orthonormal contrasts and imposing a unique variance parameter for all contrasts of the factors.

Currently, `lmer()` estimated spherical random effect using `+ (1|id) + (1|id:f) + (1|id:g) + (1|id:f:g)`. This notation can be used in `gANOVA()` using the new parametrization. However, `gANOVA()` also provides a simplification, such that `+ (1|id) + (1|id:f) + (1|id:g) + (1|id:f:g)` may be rewrite simply `+ (1|id|f*g)`.

Note that the `gANOVA()` is a "hack" of `lmer()`and should be used carefully. For instance, the sampling units, here represented by `id` should always be written after the first vertical. If `+ (1|id:g)` equivalent to `+ (1|g:id)` in `lmer()`, it is NOT the case in `gANOVA()`.

## Installation

Make sure to install `gANOVA` with its full documentation:

`devtools::install_github("jaromilfrossard/gANOVA", build_vignettes = TRUE)`


