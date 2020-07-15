library(httr)

test_that("get_token test if the token information is retrieved correctly", {
  authInfo <- list(issuerUri = "https://auth.server.org", clientId = "xxxxx-xxxxx")
  get <- mock(list(status_code = status_code, auth = authInfo))
  discover <- mock(list(endpoint = "https://auth.server.org/discover"))
  deviceFlowAuth <- mock("abcd-abcd")
  statusCode = mock(200)
  
  with_mock(
    "httr::GET" = get,
    "httr::content" = get,
    "MolgenisAuth::discover" = discover,
    #"MolgenisAuth::device_flow_auth" = deviceFlowAuth,
    token <- datashield.get_token("https://armadillo.dev.molgenis.org")
  )
  expect_equal(token, "abcd-abcd")
})