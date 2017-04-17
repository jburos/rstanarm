# Part of the rstanarm package for estimating model parameters
# Copyright (C) 2015, 2016 Trustees of Columbia University
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

# tests can be run using devtools::test() or manually by loading testthat 
# package and then running the code below possibly with options(mc.cores = 4).

library(rstanarm)
stopifnot(require(lme4))

ITER <- 1000
CHAINS <- 2
SEED <- 123
REFRESH <- ITER
set.seed(SEED)
if (interactive()) options(mc.cores = parallel::detectCores())
FIXEF_tol <- 0.05
RANEF_tol <- 0.20 

expect_stanjm <- function(x) expect_s3_class(x, "stanjm")
expect_gg <- function(x, info = NULL, label = NULL) {
  expect_is(x, "ggplot", info = info, label = label)
}
SW <- function(expr) capture.output(suppressWarnings(expr))


context("ps_check")
test_that("ps_check runs on standard stan_jm object", {
  SW(fit <- stan_jm(formulaLong = logBili ~ year + (1 | id), 
                    dataLong = pbcLong,
                    formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
                    dataEvent = pbcSurv,
                    time_var = "year",
                    iter = ITER,
                    chains = CHAINS,
                    seed = SEED,
                    refresh = REFRESH))
  psfit <- ps_check(fit)
  expect_gg(psfit)
})

test_that("ps_check runs when id-vars are characters", {
  pbcLong$id_chr <- paste0(as.character(pbcLong$id), '-new')
  pbcEvent$id_chr <- paste0(as.character(pbcLong$id), '-new')
  SW(fit <- stan_jm(formulaLong = logBili ~ year + (1 | id_chr), 
                    dataLong = pbcLong,
                    formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
                    dataEvent = pbcSurv,
                    time_var = "year",
                    iter = ITER,
                    chains = CHAINS,
                    seed = SEED,
                    refresh = REFRESH))
  psfit <- ps_check(fit)
  expect_gg(psfit)
})

test_that("ps_check runs when id-vars are factors", {
  pbcLong$id_fac <- as.factor(pbcLong$id)
  pbcEvent$id_fac <- as.factor(pbcLong$id)
  SW(fit <- stan_jm(formulaLong = logBili ~ year + (1 | id_fac), 
                    dataLong = pbcLong,
                    formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
                    dataEvent = pbcSurv,
                    time_var = "year",
                    iter = ITER,
                    chains = CHAINS,
                    seed = SEED,
                    refresh = REFRESH))
  psfit <- ps_check(fit)
  expect_gg(psfit)
})
