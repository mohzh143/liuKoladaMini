test_that("kld_indicators returns id/title", {
  testthat::skip_if_not_installed("httptest2")
  httptest2::with_mock_dir("fixtures/kpi_all", simplify = TRUE, {
    df <- kld_indicators()
    expect_s3_class(df, "tbl_df")
    expect_true(all(c("id","title") %in% names(df)))
    expect_gt(nrow(df), 10)
  })
})

test_that("kld_indicators can search by title", {
  testthat::skip_if_not_installed("httptest2")
  httptest2::with_mock_dir("fixtures/kpi_search", {
    df <- kld_indicators("arbetslöshet")
    expect_s3_class(df, "tbl_df")
    expect_true(all(c("id","title") %in% names(df)))
    expect_gt(nrow(df), 1)

    expect_true("N02282" %in% df$id)
  })
})
