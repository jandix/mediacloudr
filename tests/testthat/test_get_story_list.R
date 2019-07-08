context("Get Story List")

# invalid inputs ---------------------------------------------------------------
testthat::test_that(
  "test that function returns error if story_id is float", {
    testthat::expect_error(mediacloudr::get_story_list(last_process_stories_id = 6.496),
                           regexp = "Please provide a positive integer for last process stories id")
  })

testthat::test_that(
  "test that function returns error if story_id is character", {
    testthat::expect_error(mediacloudr::get_story_list(last_process_stories_id = "abs"),
                           regexp = "Please provide a positive integer for last process stories id")
  })

testthat::test_that(
  "test that function returns error if story_id negative", {
    testthat::expect_error(mediacloudr::get_story_list(last_process_stories_id = -2L),
                           regexp = "Please provide a positive integer for last process stories id")
  })

testthat::test_that(
  "test that function returns error if no api_key provided", {
    testthat::expect_error(mediacloudr::get_story_list(api_key = ""),
                           regexp = "Please define an API key.")
  })

testthat::test_that(
  "test that function returns error if row smaller 1", {
    testthat::expect_error(mediacloudr::get_story_list(rows = 0),
                           regexp = "Rows should be larger than 0 and smaller or equal to 1000.")
  })

testthat::test_that(
  "test that function returns error if row larger 1000", {
    testthat::expect_error(mediacloudr::get_story_list(rows = 1001),
                           regexp = "Rows should be larger than 0 and smaller or equal to 1000.")
  })
