context("Extract Meta Data")

# invalid inputs ---------------------------------------------------------------
testthat::test_that(
  "test that function returns error if no html document provided", {
    testthat::expect_error(mediacloudr::extract_meta_data(),
                           regexp = "Please define a html document.")
  })

# test html document -----------------------------------------------------------
html_doc <- mediacloudr::meta_data_html
meta_data <- list(
  open_graph = list(
    url = "https://github.com/jandix/mediacloudr",
    type = "article",
    title = "Hello world!",
    image = "https://images.pexels.com/photos/113338/pexels-photo-113338.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940",
    description = "mediacloudr hello world test file"
  ),
  twitter = list(
    url = "https://github.com/jandix/mediacloudr",
    title = "Hello world!",
    description = "mediacloudr hello world test file",
    image = "https://images.pexels.com/photos/113338/pexels-photo-113338.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940",
    image_alt = "Trees",
    card = "summary_large_image"
  ),
  native = list (
    title = "Hello world!",
    description = "mediacloudr hello world test file",
    image = "https://images.pexels.com/photos/113338/pexels-photo-113338.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940",
    thumbnail = "https://images.pexels.com/photos/113338/pexels-photo-113338.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=50"
  )
)
testthat::test_that(
  "test that function parses the document correct", {
    testthat::expect_equal(mediacloudr::extract_meta_data(html_doc = html_doc), meta_data)
  })
