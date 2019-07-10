#' Get media by id
#'
#' \code{get_media} returns media source by their id. A media source
#' is one publisher. Every story that can be collected via \code{get_story}
#' or \code{get_story_list} belongs to one media source.
#'
#' @param media_id Positive integer that contains a valid media`` id.
#' @param api_key Character string with the API key you get from mediacloud.org.
#'                Passing it is compulsory. Alternatively, function can be
#'                provided from the global environment.
#'
#' @return Data frame with results. See \url{https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#media} for field descriptions.
#'
#' @examples
#' \dontrun{
#'  media_source <- get_media_source(media_id = 604L)
#' }
#'
#' @importFrom httr parse_url build_url GET http_error http_status content
#' @importFrom jsonlite fromJSON
#'
#' @export

get_media_source <- function(media_id,
                             api_key = Sys.getenv("MEDIACLOUD_API_KEY")) {

  # errors and warnings --------------------------------------------------------
  # check if media_id is passed
  if (missing(media_id)) stop("Please define a media id.")

  # check if story_id integer and positive
  if (!is.integer(media_id) | media_id < 0L)
    stop("Please provide a positive integer for media id.")

  # check if api key is passed
  if (nchar(api_key) == 0) {
    stop("Please define an API key.")
  }

  # define and build url  ------------------------------------------------------
  # define base url
  url <-  "https://api.mediacloud.org/api/v2/media/single"
  # parse url
  url <- httr::parse_url(url = url)
  # add api key query parameter
  url$query <- list(
    key = api_key
  )
  # add story id to path
  url$path <- paste(url$path, media_id, sep = "/")
  # build url
  url <- httr::build_url(url)

  # query and parse api --------------------------------------------------------
  # query api
  response <- httr::GET(url)
  # parse response
  parsed_response <- httr::content(response, type = "text", encoding = "UTF-8")
  # parse json
  parsed_json <- jsonlite::fromJSON(parsed_response)

  # check possible errors ------------------------------------------------------
  # check if any error
  if (httr::http_error(response)) {
    stop(parsed_json$error)
  }

  # define and return result object --------------------------------------------
  # return result set
  return(parsed_json)
}
