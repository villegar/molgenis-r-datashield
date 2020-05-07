driver <- armadillo()

test_that("armadillo method creates a driver instance", {
  expect_s4_class(driver, "ArmadilloDriver")
})

test_that("dsGetInfo returns the driver version", {
  expect_equal(
    dsGetInfo(driver),
    list(driver.version = packageVersion("DSMolgenisArmadillo"))
  )
})

test_that("dsConnect returns an ArmadilloConnection", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_cookies <- mock(cookies)
  httr_handle <- mock(handle)
  with_mock(
    result <- dsConnect(driver,
      url = "https://example.org?workspace=GECKO/customer",
      username = "admin",
      password = "admin",
      name = "test"
    ),
    "httr::POST" = httr_post,
    "httr::cookies" = httr_cookies,
    "httr::handle" = httr_handle
  )
  expect_s4_class(result, "ArmadilloConnection")
  expect_equal(result@handle, handle)
  expect_equal(result@cookies$name, "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")

  expect_args(httr_handle, 1, url = "https://example.org")
  expect_args(httr_post, 1,
    handle = handle,
    path = "/login",
    encode = "form",
    body = list(username = "admin", password = "admin")
  )
})

test_that("dsConnect explains if you don't have access to the workspaces", {
  ok <- list(status_code = 200)
  forbidden <- list(status_code = 403)
  httr_post <- mock(ok, forbidden)
  httr_cookies <- mock(cookies)
  expect_error(
    with_mock(
      dsConnect(driver,
        url = "https://example.org?workspace=GECKO/customer",
        username = "admin",
        password = "admin",
        name = "test"
      ),
      "httr::POST" = httr_post,
      "httr::cookies" = httr_cookies
    ),
    "You don't have access to one or more of the workspaces"
  )
})

test_that("dsConnect restores user workspace", {
  ok <- list(status_code = 200)
  httr_post <- mock(ok, cycle = TRUE)
  httr_cookies <- mock(cookies)
  httr_handle <- mock(handle)
  with_mock(
    dsConnect(driver,
      url = "https://example.org?workspace=GECKO/customer",
      username = "admin",
      password = "admin",
      name = "test",
      restore = "keepit"
    ),
    "httr::POST" = httr_post,
    "httr::cookies" = httr_cookies,
    "httr::handle" = httr_handle
  )
  expect_called(httr_post, 3)
  expect_args(httr_post, 3,
    handle = handle,
    query = list(id = "keepit"),
    path = "/load-workspace"
  )
})

test_that("dsConnect explains when you cannot load all workspaces", {
  expect_error(
    dsConnect(driver),
    "argument \"url\" is missing, with no default"
  )
})
