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

ITER <- 400
CHAINS <- 2
SEED <- 123
REFRESH <- ITER
set.seed(SEED)
if (interactive()) options(mc.cores = parallel::detectCores())
FIXEF_tol <- 0.05
RANEF_tol <- 0.20 

expect_stanjm <- function(x) expect_s3_class(x, "stanjm")
SW <- function(expr) capture.output(suppressWarnings(expr))


context("stan_jm")
test_that("draws from stan_jm return stanjm object", {
  SW(fit <- stan_jm(formulaLong = logBili ~ year + (1 | id), 
                    dataLong = pbcLong,
                    formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
                    dataEvent = pbcSurv,
                    time_var = "year",
                    iter = ITER,
                    chains = CHAINS,
                    seed = SEED))
  expect_stanjm(fit)
})
