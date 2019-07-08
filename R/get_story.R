#' Get story by id
#'
#' \code{get_story} returns news stories by their id. One story represents
#' one online publication. Each story refers to a single URL from any feed
#' within a single media source.
#'
#' @param story_id Character string that contains a valid sotry id.
#' @param api_key Character string with the API key you get from mediacloud.org.
#'                Passing it is compulsory. Alternatively, function can be
#'                provided from the global environment
#'                (see \code{set_api_key()}).
#'
#' @return Data frame with results. See \url{https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#stories} for field descriptions.
#' @export

get_story <- function(story_id,
                      api_key = Sys.getenv("MEDIACLOUD_API_KEY")) {

  # errors and warnings --------------------------------------------------------
  # check if story_id is passed
  if (missing(story_id)) stop("Please define a story id.")

  # check if api key is passed
  if (nchar(api_key) == 0) {
    stop("Please define an API key.")
  }

  # define and build url  ------------------------------------------------------
  # define base url
  url <-  "https://api.mediacloud.org/api/v2/stories_public/single"
  # parse url
  url <- httr::parse_url(url = url)
  # add api key query parameter
  url$query <- list(
    key = api_key
  )
  # add story id to path
  url$path <- paste(url$path, story_id, sep = "/")
  # build url
  url <- httr::build_url(url)

  # query and parse api --------------------------------------------------------
  # query api
  response <- httr::GET(url)
  # check if any error
  if (httr::http_error(response)) {
    stop(httr::http_status(response)$message)
  }
  # parse response
  parsed_response <- httr::content(response, type = "text", encoding = "UTF-8")
  # parse json
  parsed_json <- jsonlite::fromJSON(parsed_response)

  # define and return result object --------------------------------------------
  # return result set
  return(parsed_json)
}
