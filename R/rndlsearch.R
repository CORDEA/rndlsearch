#' @export
rndlsearch <- function(...) {
  response <- httr::GET(
    url = "https://iss.ndl.go.jp/api/opensearch",
    query = list(...)
  )
  xml <- xml2::read_xml(response)
  xml2::xml_find_all(xml, "//item")
}
