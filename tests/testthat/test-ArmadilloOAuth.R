test_that("get_token test if the token information is retrieved correctly", {
  httr_get <- mock(structure(list(status_code = 200), class = "response"))
  httr_content <- mock(list(auth = list(clientId = "hufjksdhfdjks-sdsa", issuerUri = "https://auth.example.org")))
  endpoint <- httr::oauth_endpoint(
    authorize = "https://example.org/oauth2/authorize",
    access = "https://example.org/oauth2/token",
    user = "https://example.org/oauth2/userinfo",
    device = "https://example.org/oauth2/device-authorize",
    logout = "https://example.org/oauth2/logout"
  )
  molgenisauth_discover <- mock(endpoint)
  molgenisauth_device_flow_auth <- mock(list(id_token = "abcd-abcd"))

  with_mock(
    "httr::GET" = httr_get,
    "httr::content" = httr_content,
    "MolgenisAuth::discover" = molgenisauth_discover,
    "MolgenisAuth::device_flow_auth" = molgenisauth_device_flow_auth,
    token <- armadillo.get_token("https://example.org")
  )
  expect_equal(token, "abcd-abcd")
  expect_args(
    httr_get,
    1,
    "https://example.org/actuator/info"
  )
  expect_args(
    molgenisauth_discover,
    1,
    "https://auth.example.org"
  )
  expect_args(
    molgenisauth_device_flow_auth,
    1,
    endpoint,
    "hufjksdhfdjks-sdsa"
  )
})
