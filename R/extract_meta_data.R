#' Extract meta data
#'
#' \code{extract_meta_data} extracts native, open graph and twitter meta data
#' from html documents. The meta data include url, title, description and image.
#' The html document is parsed within the function
#'
#' @param html_doc Charcter string including the html document.
#'
#' @return List with three sublists for native, open graph and twitter.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_attr html_text html_node
#'
#' @export

extract_meta_data <- function (html_doc) {

  # errors and warnings --------------------------------------------------------
  # check if html document is passed
  if (missing(html_doc)) stop("Please define a html document.")

  # parse document and prepare empty result set --------------------------------
  # parse html
  parsed_html <- xml2::read_html(html_doc)
  # define empty return object
  meta_data <- list(
    open_graph = list(
      url = NA,
      type = NA,
      title = NA,
      image = NA,
      description = NA
    ),
    twitter = list(
      url = NA,
      title = NA,
      description = NA,
      image = NA,
      image_alt = NA,
      card = NA
    ),
    native = list (
      title = NA,
      description = NA,
      image = NA,
      thumbnail = NA
    )
  )

  # extract meta data ----------------------------------------------------------
  # og url
  meta_data$open_graph$url <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='og:url']"),
    "content"
  )
  # og type
  meta_data$open_graph$type <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='og:type']"),
    "content"
  )
  # og title
  meta_data$open_graph$title <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='og:title']"),
    "content"
  )
  # og image
  meta_data$open_graph$image <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='og:image']"),
    "content"
  )
  # og description
  meta_data$open_graph$description <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='og:description']"),
    "content"
  )
  # twitter url
  meta_data$twitter$url <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:url']"),
    "content"
  )
  # twitter title
  meta_data$twitter$title <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:title']"),
    "content"
  )
  # twitter description
  meta_data$twitter$description <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:description']"),
    "content"
  )
  # twitter image
  meta_data$twitter$image <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:image']"),
    "content"
  )
  # twitter image_alt
  meta_data$twitter$image_alt <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:image:alt']"),
    "content"
  )
  # twitter card
  meta_data$twitter$card <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[property='twitter:card']"),
    "content"
  )
  # native title
  meta_data$native$title <- rvest::html_text(
    rvest::html_node(parsed_html, "title")
  )
  # native description
  meta_data$native$description <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[name='description']"),
    "content"
  )
  # native image
  meta_data$native$image <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[name='image']"),
    "content"
  )
  # native thumbnail
  meta_data$native$thumbnail <- rvest::html_attr(
    rvest::html_node(parsed_html, "meta[name='thumbnail']"),
    "content"
  )

  # process and return result set ----------------------------------------------
  # replace empty characters
  meta_data <- lapply(meta_data, lapply,
                      function (x) ifelse(length(x) <= 0, NA, x))
  # return results
  return(meta_data)
}
