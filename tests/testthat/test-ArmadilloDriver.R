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

test_that("dsConnect complains about missing required parameters", {
  expect_error(
    dsConnect(driver),
    "argument \"url\" is missing, with no default"
  )
})

test_that("dsConnect returns an ArmadilloConnection", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_cookies <- mock(cookies)
  with_mock(
    result <- dsConnect(driver,
      url = "https://example.org?workspace=GECKO/customer",
      username = "admin",
      password = "admin",
      name = "test"
    ),
    "httr::POST" = httr_post,
    "httr::cookies" = httr_cookies
  )
  expect_s4_class(result, "ArmadilloConnection")
  expect_equal(result@cookies$name, "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")
})
