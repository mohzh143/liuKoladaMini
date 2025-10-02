test_that("kld_municipalities returns id/title/type", {
  testthat::skip_if_not_installed("httptest2")
  httptest2::with_mock_dir("fixtures/municipality" , simplify = TRUE, {
    df <- kld_municipalities()
    expect_s3_class(df, "tbl_df")
    expect_true(all(c("id","title","type") %in% names(df)))
    expect_gt(nrow(df), 100)
  })
})
