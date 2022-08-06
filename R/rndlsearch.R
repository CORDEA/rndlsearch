#' @export
rndlsearch <- function(...) {
  httr::GET(
    url = "https://iss.ndl.go.jp/api/opensearch",
    query = list(...)
  )
}
