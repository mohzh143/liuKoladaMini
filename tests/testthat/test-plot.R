test_that("kld_plot works on kld_values() output (recorded fixture)", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("fx_plot", simplify = TRUE, {
    df <- kld_values("0580", "N02282", 2019:2024)
  })

  p <- kld_plot(df)

  expect_s3_class(p, "ggplot")

  expect_true(grepl("N02282", p$labels$title))
  expect_true(grepl("0580", p$labels$title))
})
