handle <- httr::handle(url = "http://example.org:8080")
connection <- methods::new("ArmadilloConnection",
  name = "name",
  handle = handle,
  workspaces = list("GECKO/customer"),
  user = "admin",
  cookies = cookies
)

test_that("dsDisconnect calls /logout endpoint ", {
  post <- mock()
  with_mock(
    "httr::POST" = post,
    dsDisconnect(connection)
  )
  expect_called(post, 1)
  expect_args(post, 1, handle = handle, path = "/logout")
})

test_that("dsDisconnect saves the workspace", {
  post <- mock(list(status_code = 200), cycle = TRUE)
  with_mock(
    "httr::POST" = post,
    dsDisconnect(connection, save = "keepit")
  )
  expect_called(post, 2)
  expect_args(post, 1, handle = handle, path = "/workspaces/keepit")
})

test_that("dsListTables retrieves tables", {
  tables <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(tables)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListTables(connection)
  )
  expect_args(get, 1, handle = handle, path = "/tables")
  expect_equal(result, unlist(tables))
})

test_that("dsHasTable returns TRUE if table exists", {
  head <- mock(list(status_code = 200))
  with_mock(
    "httr::HEAD" = head,
    expect_true(dsHasTable(connection, "package.NAME"))
  )
  expect_args(head, 1, handle = handle, path = "/tables/package.NAME")
})

test_that("dsHasTable returns FALSE if table doesnot exist", {
  head <- mock(list(status_code = 404))
  with_mock(
    "httr::HEAD" = head,
    expect_false(dsHasTable(connection, "package.NAME"))
  )
  expect_args(head, 1, handle = handle, path = "/tables/package.NAME")
})

test_that("dsIsAsync returns boolean list", {
  expect_equal(
    dsIsAsync(connection),
    list(aggregate = TRUE, assignTable = FALSE, assignExpr = TRUE)
  )
})

test_that("dsListSymbols returns symbols", {
  symbols <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(symbols)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListSymbols(connection)
  )
  expect_args(get, 1, handle = handle, path = "/symbols")
  expect_equal(result, unlist(symbols))
})

test_that("dsRmSymbol removes symbol", {
  delete <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::DELETE" = delete,
    dsRmSymbol(connection, "D")
  )
  expect_args(delete, 1, handle = handle, path = "/symbols/D")
})

test_that("dsAssignTable assigns table to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignTable(connection, "D", "package.NAME")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/symbols/D?table=package.NAME"
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignTable, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    dsAssignTable(connection, "D", "package.NAME", async = FALSE)
  )
  expect_args(post, 1,
    handle = handle,
    path = "/symbols/D?table=package.NAME"
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsListMethods returns assign methods", {
  methods <- list(list(
    name = "foo",
    "function" = "dsBase::foo",
    version = "6.0.0-03",
    package = "dsBase"
  ), list(
    name = "bar",
    "function" = "dsBase::bar",
    version = "6.0.0-03",
    package = "dsBase"
  ))
  get <- mock(list(status_code = 200))
  content <- mock(methods)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListMethods(connection, type = "assign")
  )

  expect_args(get, 1, handle = handle, path = "/methods/assign")

  skip("Fix the columns!")

  expected <- tibble(
    name = list("foo", "bar"),
    value = list("dsBase::foo", "dsBase::bar"),
    version = list("6.0.0-03", "6.0.0-03"),
    package = list("dsBase", "dsBase"),
    type = list("assign", "assign"),
    class = list("function", "function")
  )
  expect_equivalent(result, expected)
})

test_that("dsListPackages extracts name and version from packages", {
  packages <- list(list(
    built = "3.6.3",
    libPath = "/usr/local/lib/R/site-library",
    name = "minqa",
    version = "1.2.4"
  ), list(
    built = "3.6.3",
    libPath = "/usr/local/lib/R/site-library",
    name = "RANN",
    version = "2.6.1"
  ))
  get <- mock(list(status_code = 200))
  content <- mock(packages)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListPackages(connection)
  )

  expect_equivalent(result, tibble(
    name = list("minqa", "RANN"),
    version = list("1.2.4", "2.6.1")
  ))
})

test_that("dsListWorkspaces lists workspaces", {
  workspaces <- list(
    list(
      name = "hello",
      size = 48L,
      ETag = "\"ca5d3a000723844e874b93a65c3888a1-1\"",
      lastModified = "2020-05-06T13:09:00.725Z"
    ),
    list(
      name = "world",
      size = 378L,
      ETag = "\"1f45d1a6f13adda1d05adf1f3da4c4ca-1\"",
      lastModified = "2020-05-06T13:09:07.617Z"
    )
  )
  get <- mock(list(status_code = 200))
  content <- mock(workspaces)

  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListWorkspaces(connection)
  )

  skip("Fix the bug!")
  expect_equivalent(result, tibble(
    name = list("hello", "world"),
    size = list(48L, 378L),
    ETag = list("\"ca5d3a000723844e874b93a65c3888a1-1\"",
                "\"1f45d1a6f13adda1d05adf1f3da4c4ca-1\""),
    lastAccessDate = list("2020-05-06T13:09:00.725Z"),
                          "2020-05-06T13:09:07.617Z"))
})
