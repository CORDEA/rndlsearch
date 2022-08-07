#' @export
rndlsearch <- function(...) {
  response <- httr::GET(
    url = "https://iss.ndl.go.jp/api/opensearch",
    query = list(...)
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
    item$description[[1]],
    item$author[[1]],
    item$category[[1]],
    item$guid[[1]],
    extract_id(ids, "ISBN"),
    item$pubDate[[1]],
    unname(unlist(item[names(item)=="creator"], recursive=FALSE)),
    item$volume[[1]],
    item$edition[[1]],
    item$publisher[[1]],
    item$date[[1]],
    unname(lapply(item[names(item)=="seeAlso"], function (e) {
      attributes(e)$resource
    }))
  )
  names(l) <- c(
    "Title",
    "Link",
    "Description",
    "Author",
    "Category",
    "GUID",
    "ISBN",
    "Publish Date",
    "Creators",
    "Volume",
    "Edition",
    "Publisher",
    "Date",
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
