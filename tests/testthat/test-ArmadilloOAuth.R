test_that("get_token test if the token information is retrieved correctly", {
  content <- mock()
  discover <- mock()
  get <- mock(structure(list(status_code = 200), class = "response"))
  credentials <- mock(list(id_token = "abcd-abcd"))

  with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    "MolgenisAuth::discover" = discover,
    "MolgenisAuth::device_flow_auth" = credentials,
    token <- datashield.get.token("https://armadillo.dev.molgenis.org")
  )
  expect_equal(token, "abcd-abcd")
})
