#' Get story list
#'
#' \code{get_story} returns a list of stories based on a multifacted query. One
#' story represents one online publication. Each story refers to a single URL
#' from any feed within a single media source.
#'
#' @param last_process_stories_id Return stories in which the
#'                                processed_stories_id is greater than this
#'                                value.
#' @param rows Number of stories to return, max 1000.
#' @param feeds_id Return only stories that match the given feeds_id, sorted
#'                 my descending publish date
#' @param q If specified, return only results that match the given Solr query.
#'          Only one q parameter may be included.
#' @param fq If specified, file results by the given Solr query. More than one
#'           fq parameter may be included.
#' @param sort Returned results sort order. Supported values:
#'             processed_stories_id, random
#' @param wc If set to TRUE, include a 'word_count' field with each story that
#'           includes a count of the most common words in the story
#' @param show_feeds If set to TRUE, include a 'feeds' field with a list of the
#'                   feeds associated with this story
#' @param api_key Character string with the API key you get from mediacloud.org.
#'                Passing it is compulsory. Alternatively, function can be
#'                provided from the global environment.
#' @examples
#' \dontrun{
#'  stories <- get_story_list()
#'  stories <- get_story_list(q = "Trump")
#' }
#'
#' @return Data frame with results. See \url{https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#stories} for field descriptions.
#' @export

get_story_list <- function(last_process_stories_id = 0L,
                           rows                    = 100,
                           feeds_id                = NULL,
                           q                       = NULL,
                           fq                      = NULL,
                           sort                    = "processed_stories_id",
                           wc                      = FALSE,
                           show_feeds              = FALSE,
                           api_key                 = Sys.getenv("MEDIACLOUD_API_KEY")) {

  # errors and warnings --------------------------------------------------------
  # check if last_process_stories_id valid is passed
  # check if story_id integer and positive
  if (!is.integer(last_process_stories_id) | last_process_stories_id < 0)
    stop("Please provide a positive integer for last process stories id.")

  # check if rows in range
  if (rows <= 0 | rows > 1000)
    stop("Rows should be larger than 0 and smaller or equal to 1000.")

  # check if api key is passed
  if (nchar(api_key) == 0) {
    stop("Please define an API key.")
  }

  # define and build url  ------------------------------------------------------
  # define base url
  url <-  "https://api.mediacloud.org/api/v2/stories_public/list"
  # parse url
  url <- httr::parse_url(url = url)
  # add api key query parameter
  url$query <- list(
    last_process_stories_id = last_process_stories_id,
    rows = rows,
    feeds_id = feeds_id,
    q = q,
    fq = fq,
    sort = sort,
    wc = wc,
    show_feeds = show_feeds,
    key = api_key
  )
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
