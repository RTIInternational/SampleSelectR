test_that("chromy sample size is correct", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N * .2)
  mos <- rlnorm(N, meanlog = 0, sdlog = 2)
  exphits <- n * mos / sum(mos)
  hits <- chromy_inner(exphits)
  expect_equal(sum(hits), sum(exphits), expected.label = "n")
})

test_that("chromy sample size is nonnegative", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N * .2)
  mos <- rlnorm(N, meanlog = 0, sdlog = 2)
  exphits <- n * mos / sum(mos)
  hits <- chromy_inner(exphits)
  expect_gte(min(hits), 0)
})


test_that("chromy hits within correct range", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N * .2)
  mos <- rlnorm(N, meanlog = 0, sdlog = 2)
  exphits <- n * mos / sum(mos)
  hits <- chromy_inner(exphits)
  expect(
    all(hits >= floor(exphits) & hits <= (floor(exphits) + 1)),
    "Number of hits is outside of floor(exphits) and floor(exphits)+1"
  )
})

test_that("chromy always gets sample size of 1 when hits .5/.5", {
  expect(
    all(replicate(1000, sum(chromy_inner(c(.5, .5)))) == 1),
    "Did not get a sample size of 1 when expected hits was .5/.5"
  )
})

test_that("chromy hits within correct range for PNR case", {
  expect(
    all(replicate(1000, chromy_inner(mtcars$wt/sum(mtcars$wt))) %in% c(0, 1)),
    "Number of hits for PNR is not exactly 0 or 1"
  )
})
