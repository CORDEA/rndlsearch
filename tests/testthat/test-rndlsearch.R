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
    count = 2,
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
    count = 2,
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
    count = 2,
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
  expect_equal(second$Title, "Title")
  expect_equal(second$Link, "Link")
  expect_equal(second$Descriptions, list("Description"))
  expect_equal(second$Author, "Author")
  expect_equal(second$`Publish Date`, "PubDate")
  expect_equal(second$Category, "Category")
  expect_equal(second$GUID, "GUID")
  expect_equal(second$Date, "Date")
  expect_equal(second$Creators, list("Creator"))
  expect_equal(second$Subjects, list("Subject"))
  expect_equal(second$Volume, "Volume")
  expect_equal(second$Edition, "Edition")
  expect_equal(second$`Series Title`, "SeriesTitle")
  expect_equal(second$Publishers, list("Publisher"))
  expect_equal(second$`Issued Date`, "Issued")

  third = parsed[[3]]
  expect_equal(sum(lengths(third)), 19)
  expect_equal(third$Title, "Title")
  expect_equal(third$Link, "Link")
  expect_equal(third$Author, "Author")
  expect_equal(third$Category, "Category")
  expect_equal(third$GUID, "GUID")
  expect_equal(third$Descriptions, list("Description1", "Description2"))
  expect_equal(third$Creators, list("Creator1", "Creator2"))
  expect_equal(third$Subjects, list("Subject1", "Subject2"))
  expect_equal(third$Publishers, list("Publisher1", "Publisher2"))
  expect_equal(third$Extents, list("Extent1", "Extent2"))
  expect_equal(third$ISBN, "ISBN")
  expect_equal(third$Resources, list("Reference1", "Reference2", "Reference3"))
})
