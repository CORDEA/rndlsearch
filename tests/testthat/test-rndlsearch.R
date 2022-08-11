test_that("build query", {
  mockery::stub(rndlsearch, "request", function(query) {
    expect_equal(length(query), 7)
    expect_equal(query$cnt, 2)
    expect_equal(query$title, "title")
    expect_equal(query$description, "description")
    expect_equal(query$creator, "creator")
    expect_equal(query$publisher, "publisher")
    expect_equal(query$from, "from")
    expect_equal(query$until, "until")
    list()
  })

  rndlsearch(
    limit = 2,
    title = "title",
    description = "description",
    creator = "creator",
    publisher = "publisher",
    from = "from",
    until = "until"
  )
})

test_that("build query", {
  mockery::stub(rndlsearch, "request", function(query) {
    expect_equal(length(query), 3)
    expect_equal(query$cnt, 2)
    expect_equal(query$foo, "foo")
    expect_equal(query$bar, "bar")
    list()
  })

  rndlsearch(
    limit = 2,
    foo = "foo",
    bar = "bar"
  )
})

test_that("build query", {
  mockery::stub(rndlsearch, "request", function(query) {
    expect_equal(length(query), 1)
    expect_equal(query$cnt, 2)
    list()
  })

  rndlsearch(
    limit = 2,
    cnt = 3
  )
})

test_that("parse response", {
  xml <- xml2::read_xml("test.xml")
  parsed <- parse_response(xml)

  expect_equal(length(parsed), 3)

  expect_equal(sum(lengths(parsed[[1]])), 0)

  second = parsed[[2]]
  expect_equal(sum(lengths(second)), 15)
  expect_equal(second$title, "Title")
  expect_equal(second$link, "Link")
  expect_equal(second$descriptions, list("Description"))
  expect_equal(second$author, "Author")
  expect_equal(second$publishDate, "PubDate")
  expect_equal(second$category, "Category")
  expect_equal(second$guid, "GUID")
  expect_equal(second$date, "Date")
  expect_equal(second$creators, list("Creator"))
  expect_equal(second$subjects, list("Subject"))
  expect_equal(second$volume, "Volume")
  expect_equal(second$edition, "Edition")
  expect_equal(second$seriesTitle, "SeriesTitle")
  expect_equal(second$publishers, list("Publisher"))
  expect_equal(second$issuedDate, "Issued")

  third = parsed[[3]]
  expect_equal(sum(lengths(third)), 19)
  expect_equal(third$title, "Title")
  expect_equal(third$link, "Link")
  expect_equal(third$author, "Author")
  expect_equal(third$category, "Category")
  expect_equal(third$guid, "GUID")
  expect_equal(third$descriptions, list("Description1", "Description2"))
  expect_equal(third$creators, list("Creator1", "Creator2"))
  expect_equal(third$subjects, list("Subject1", "Subject2"))
  expect_equal(third$publishers, list("Publisher1", "Publisher2"))
  expect_equal(third$extents, list("Extent1", "Extent2"))
  expect_equal(third$isbn, "ISBN")
  expect_equal(third$resources, list("Reference1", "Reference2", "Reference3"))
})
