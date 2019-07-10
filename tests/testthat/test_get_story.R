context("Get Story")

# invalid inputs ---------------------------------------------------------------
testthat::test_that(
  "test that function returns error if no story_id provided", {
  testthat::expect_error(mediacloudr::get_story(),
                         regexp = "Please define a story id.")
})

testthat::test_that(
  "test that function returns error if story_id is float", {
    testthat::expect_error(mediacloudr::get_story(story_id = 6.496),
                           regexp = "Please provide a positive integer for story id.")
  })

testthat::test_that(
  "test that function returns error if story_id is character", {
    testthat::expect_error(mediacloudr::get_story(story_id = "abs"),
                           regexp = "Please provide a positive integer for story id.")
  })

testthat::test_that(
  "test that function returns error if story_id negative", {
    testthat::expect_error(mediacloudr::get_story(story_id = -2L),
                           regexp = "Please provide a positive integer for story id.")
  })

testthat::test_that(
  "test that function returns error if no api_key provided", {
    testthat::expect_error(mediacloudr::get_story(story_id = 604L, api_key = ""),
                           regexp = "Please define an API key.")
  })

# test result set --------------------------------------------------------------
testthat::test_that(
  "test that function returns only one row", {
    testthat::skip_if(nchar(Sys.getenv("MEDIACLOUD_API_KEY")) == 0,
                      message = "API key not available in environment. Skipping test.")
    example_result <- mediacloudr::get_story(27456565L)
    testthat::expect_equal(nrow(example_result), 1,
                           info = paste0("1 row expected, but got ", nrow(example_result), " row(s)."))
  })
testthat::test_that(
  "test that function returns 15 columns", {
    testthat::skip_if(nchar(Sys.getenv("MEDIACLOUD_API_KEY")) == 0,
                      message = "API key not available in environment. Skipping test.")
    example_result <- mediacloudr::get_story(27456565L)
    testthat::expect_equal(ncol(example_result), 15,
                           info = paste0("15 cols expected, but got ", nrow(example_result), " col(s)."))
  })
