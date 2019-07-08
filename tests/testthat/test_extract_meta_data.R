context("Extract Meta Data")

# invalid inputs ---------------------------------------------------------------
testthat::test_that(
  "test that function returns error if no html document provided", {
    testthat::expect_error(mediacloudr::extract_meta_data(),
                           regexp = "Please define a html document.")
  })

# test html document -----------------------------------------------------------
