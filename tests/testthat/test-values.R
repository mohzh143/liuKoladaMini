test_that("kld_values returns a tidy series (Linköping N02282)", {
  testthat::skip_if_not_installed("httptest2")
  httptest2::with_mock_dir("fixtures", simplify = TRUE, {
    df <- kld_values("0580", "N02282", 2019:2024)

    # Structure and required columns
    expect_s3_class(df, "tbl_df")
    expect_true(all(c("year","value","municipality_id","kpi_id") %in% names(df)))

    # ID and year range
    expect_true(all(df$municipality_id == "0580"))
    expect_true(all(df$kpi_id          == "N02282"))
    expect_true(all(df$year %in% 2019:2024))

    # Type and values (allow NA)
    expect_true(is.double(df$value))               # value 为 double
    expect_true(all(is.finite(df$value) | is.na(df$value)))

    # At most 6 rows (2019–2024)
    expect_lte(nrow(df), 6L)
    expect_true(all(sort(unique(df$year)) %in% 2019:2024))

    # Years should be in ascending order
    expect_equal(df$year, sort(df$year))
  })
})
