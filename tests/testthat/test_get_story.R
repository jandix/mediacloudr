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
