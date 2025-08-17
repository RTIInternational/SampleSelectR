test_that("chromy sample size is correct", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N*.2)
  mos <- rlnorm(N, meanlog=0, sdlog=2)
  exphits <- n*mos/sum(mos)
  hits <- chromy_inner(exphits)
  expect_equal(sum(hits), n, expected.label="n")
})

test_that("chromy sample size is nonnegative", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N*.2)
  mos <- rlnorm(N, meanlog=0, sdlog=2)
  exphits <- n*mos/sum(mos)
  hits <- chromy_inner(exphits)
  expect_gte(min(hits), 0)
})


test_that("chromy hits within correct range", {
  set.seed(8675309)
  N <- 10000L
  n <- floor(N*.2)
  mos <- rlnorm(N, meanlog=0, sdlog=2)
  exphits <- n*mos/sum(mos)
  hits <- chromy_inner(exphits)
  expect(
    all(hits >= floor(exphits) & hits <= (floor(exphits) + 1)),
    "Number of hits is outside of floor(exphits) and floor(exphits)+1"
  )
})
