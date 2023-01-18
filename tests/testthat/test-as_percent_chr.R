test_that("as_percent_chr works", {
  expect_identical(as_percent_chr(0.3, 2L),
                   "30.00%")
  expect_identical(as_percent_chr(0.3, 1L),
                   "30.0%")
  expect_identical(as_percent_chr(0.3, 0L),
                   "30%")
  expect_identical(as_percent_chr(0.123456, 2L),
                   "12.35%")
  expect_identical(as_percent_chr(0.123456, 1L),
                   "12.3%")
  expect_identical(as_percent_chr(0.123456, 0L),
                   "12%")
  expect_identical(as_percent_chr(0.12002, 2L),
                   "12.00%")
  expect_identical(as_percent_chr(0.12009, 2L),
                   "12.01%")
  expect_identical(as_percent_chr(0.1299, 1L),
                   "13.0%")
  expect_identical(as_percent_chr(0.129, 0L),
                   "13%")
  expect_identical(as_percent_chr(0.3, 2L,","),
                   "30,00%")
  expect_identical(as_percent_chr(0.3, 1L, ","),
                   "30,0%")
  expect_identical(as_percent_chr(0.3, 0L, ","),
                   "30%")
  expect_identical(as_percent_chr(0.123456, 2L, ","),
                   "12,35%")
  expect_identical(as_percent_chr(c(0.3, 0.123456), 2L),
                   c("30.00%", "12.35%"))
})
