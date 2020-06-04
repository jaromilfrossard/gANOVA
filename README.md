# gANOVA

This package provides a modification of the `lmer()` function from the `lme4` package. It estimates a spherical correlation of random effects when interacting fixed factors with random samples. The method is explained in detail in [The correlation structure of mixed effects models with crossed random effects in controlled experiments](https://arxiv.org/abs/1903.10766).

Currently `lme4` has the possibiliy to estimate the spherical random effects using `+ (1|id:f)`. However the parametrization used in `lme4` puts constraints on lower interaction parameters, such that the covariance estimated using `+ (1|id) + (1|id:f)` may have the variance of `(1|id)` estimated at the boundary (or simply 0). `gANOVA` reparametrizes the covariance matrix in order to extend the range of parameters such that variance of lower interactions estimated at 0 happen less.

Technically, this new parametrization is simply transforming factors into orthonormal contrasts and imposing a unique variance parameter for all contrasts of the factors.

Currently, the notation in `lmer()` to estimate spherical covariance of random effect is using `+ (1|id) + (1|id:f) + (1|id:g) + (1|id:f:g)`. This notation can be used in `gANOVA()` for this new parametrization. However, `gANOVA()` also provides a simplification of the notatation, such that `+ (1|id) + (1|id:f) + (1|id:g) + (1|id:f:g)` may be rewritten simply as `+ (1|id|f*g)`.

Note that the `gANOVA()` is a "hack" of `lmer()`and should be used carefully. For instance, the sampling units, here represented by `id` should always be written after the first vertical. If `+ (1|id:g)` equivalent to `+ (1|g:id)` in `lmer()`, it is NOT the case in `gANOVA()`.

## Installation

Make sure to install `gANOVA` with its full documentation:

`devtools::install_github("jaromilfrossard/gANOVA", build_vignettes = TRUE)`


To see the difference between the `lme4` and the `gANOVA` parametrization of the spherical correlation structure check the vignette:

`vignette("spherical-distribution-example", package = "gANOVA")`



