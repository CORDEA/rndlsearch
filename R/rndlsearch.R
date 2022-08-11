#' Query the Search API of National Diet Library.
#'
#' @param limit Number of results. It will be sent as `cnt`.
#' @param title Title.
#' @param description Description.
#' @param creator Creator.
#' @param publisher Publisher.
#' @param from Results after `from` specified in `yyyy`.
#' @param until Results before `until` specified in `yyyy`.
#' @param ... Extra parameters. First parameters are preferred if overridden.
#'
#' @return Results. Returns as named `list`.
#'
#' @export
rndlsearch <- function(limit = NULL,
                       title = NULL, description = NULL,
                       creator = NULL, publisher = NULL,
                       from = NULL, until = NULL,
                       ...) {
  query <- c(list(
    "cnt" = limit,
    "title" = title,
    "description" = description,
    "creator" = creator,
    "publisher" = publisher,
    "from" = from,
    "until" = until
  ), list(...))
  query <- Filter(function (e) { !is.null(e) }, query)
  request(query[!duplicated(names(query))])
}

request <- function(query) {
  response <- httr::GET(
    url = "https://iss.ndl.go.jp/api/opensearch",
    query = query
  )
  xml <- xml2::read_xml(response)
  parse_response(xml)
}

parse_response <- function(response) {
  items <- xml2::xml_find_all(response, "//item")
  lapply(xml2::as_list(items), flatten_item)
}

flatten_item <- function(item) {
  ids = item[names(item)=="identifier"]
  l <- list(
    item$title[[1]],
    item$link[[1]],
    unname(unlist(item[names(item)=="description"], recursive=FALSE)),
    unname(unlist(item[names(item)=="subject"], recursive=FALSE)),
    item$author[[1]],
    item$category[[1]],
    item$guid[[1]],
    extract_id(ids, "ISBN"),
    item$pubDate[[1]],
    item$issued[[1]],
    item$date[[1]],
    unname(unlist(item[names(item)=="creator"], recursive=FALSE)),
    item$volume[[1]],
    item$edition[[1]],
    item$seriesTitle[[1]],
    unname(unlist(item[names(item)=="extent"], recursive=FALSE)),
    unname(unlist(item[names(item)=="publisher"], recursive=FALSE)),
    unname(lapply(item[names(item)=="seeAlso"], function (e) {
      attributes(e)$resource
    }))
  )
  names(l) <- c(
    "Title",
    "Link",
    "Descriptions",
    "Subjects",
    "Author",
    "Category",
    "GUID",
    "ISBN",
    "Publish Date",
    "Issued Date",
    "Date",
    "Creators",
    "Volume",
    "Edition",
    "Series Title",
    "Extents",
    "Publishers",
    "Resources"
  )
  l
}

extract_id <- function(ids, type) {
  r <- Filter(
    function (e) { attr(e, "type") == paste0("dcndl:", type) },
    ids
  )
  if (length(r) == 0) {
    NULL
  } else {
    r[[1]][[1]]
  }
}
