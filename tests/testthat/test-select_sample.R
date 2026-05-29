test_that("n on frame does not cause issue - chromy_pps", {
  county_2023_slim_n <- county_2023 |>
    tidytable::select(GEOID, Region, Pop_Tot) |>
    tidytable::mutate(
      n = 50,
      ExpHits_man = 10 * Pop_Tot / sum(Pop_Tot),
      .by = "Region"
    )

  sampsizes <- county_2023_slim_n |>
    tidytable::distinct(Region) |>
    tidytable::mutate(sample_size = 10)

  set.seed(12345)
  samp1 <- county_2023_slim_n |>
    select_sample(
      "chromy_pps",
      n = sampsizes,
      strata = "Region",
      mos = "Pop_Tot",
      outall = TRUE
    )
  set.seed(12345)
  samp2 <- county_2023_slim_n |>
    tidytable::select(-n) |>
    select_sample(
      "chromy_pps",
      n = sampsizes,
      strata = "Region",
      mos = "Pop_Tot",
      outall = TRUE
    )
  expect_equal(
    samp1 |> tidytable::select(-n),
    samp2
  )
})

test_that("n on frame does not cause issue - srs", {
  county_2023_slim_n <- county_2023 |>
    tidytable::select(GEOID, Region, Pop_Tot) |>
    tidytable::mutate(
      n = 50,
      ExpHits_man = 10 * Pop_Tot / sum(Pop_Tot),
      .by = "Region"
    )

  sampsizes <- county_2023_slim_n |>
    tidytable::distinct(Region) |>
    tidytable::mutate(sample_size = 10)

  set.seed(12345)
  samp1 <- county_2023_slim_n |>
    select_sample(
      "srs",
      n = sampsizes,
      strata = "Region",
      outall = TRUE
    )
  set.seed(12345)
  samp2 <- county_2023_slim_n |>
    tidytable::select(-n) |>
    select_sample(
      "srs",
      n = sampsizes,
      strata = "Region",
      outall = TRUE
    )
  expect_equal(
    samp1 |> tidytable::select(-n),
    samp2
  )
})

test_that("n on frame does not cause issue - sys_pps", {
  county_2023_slim_n <- county_2023 |>
    tidytable::select(GEOID, Region, Pop_Tot) |>
    tidytable::mutate(
      n = 50,
      ExpHits_man = 10 * Pop_Tot / sum(Pop_Tot),
      .by = "Region"
    )

  sampsizes <- county_2023_slim_n |>
    tidytable::distinct(Region) |>
    tidytable::mutate(sample_size = 10)

  set.seed(12345)
  samp1 <- county_2023_slim_n |>
    select_sample(
      "sys_pps",
      n = sampsizes,
      strata = "Region",
      mos = "Pop_Tot",
      outall = TRUE
    )
  set.seed(12345)
  samp2 <- county_2023_slim_n |>
    tidytable::select(-n) |>
    select_sample(
      "sys_pps",
      n = sampsizes,
      strata = "Region",
      mos = "Pop_Tot",
      outall = TRUE
    )
  expect_equal(
    samp1 |> tidytable::select(-n),
    samp2
  )
})

test_that("n on frame does not cause issue - sys", {
  county_2023_slim_n <- county_2023 |>
    tidytable::select(GEOID, Region, Pop_Tot) |>
    tidytable::mutate(
      n = 50,
      ExpHits_man = 10 * Pop_Tot / sum(Pop_Tot),
      .by = "Region"
    )

  sampsizes <- county_2023_slim_n |>
    tidytable::distinct(Region) |>
    tidytable::mutate(sample_size = 10)

  set.seed(12345)
  samp1 <- county_2023_slim_n |>
    select_sample(
      "sys",
      n = sampsizes,
      strata = "Region",
      outall = TRUE
    )
  set.seed(12345)
  samp2 <- county_2023_slim_n |>
    tidytable::select(-n) |>
    select_sample(
      "sys",
      n = sampsizes,
      strata = "Region",
      outall = TRUE
    )
  expect_equal(
    samp1 |> tidytable::select(-n),
    samp2
  )
})
