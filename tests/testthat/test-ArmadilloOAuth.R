test_that("get_token returns the id_token from credentials", {
  mock_credentials <- new("ArmadilloCredentials",
                          access_token = "access123",
                          expires_in = 3600,
                          expires_at = Sys.time() + 3600,
                          id_token = "abcd-abcd",
                          refresh_token = "refresh123",
                          token_type = "Bearer",
                          userId = "user123"
  )

  with_mock(
    "DSMolgenisArmadillo::armadillo.get_credentials" = function(server) mock_credentials,
    token <- armadillo.get_token("https://example.org")
  )

  expect_equal(token, "abcd-abcd")
})

test_that("armadillo.get_credentials returns a valid ArmadilloCredentials object", {
  server <- "https://example.org"

  dummy_auth_info <- list(auth = list(
    clientId = "dummy-client-id",
    issuerUri = "https://auth.example.org"
  ))

  dummy_endpoint <- httr::oauth_endpoint(
    authorize = "auth_url",
    access = "token_url"
  )

  dummy_credentials <- list(
    access_token = "access123",
    expires_in = 3600,
    id_token = "id123",
    refresh_token = "refresh123",
    token_type = "Bearer",
    userId = "user123"
  )

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "MolgenisAuth::discover" = function(issuer) dummy_endpoint,
    "MolgenisAuth::device_flow_auth" = function(endpoint, client_id) dummy_credentials,
    {
      result <- armadillo.get_credentials(server)
    }
  )

  expect_s4_class(result, "ArmadilloCredentials")
  expect_equal(result@access_token, "access123")
  expect_equal(result@expires_in, 3600)
  expect_equal(result@id_token, "id123")
  expect_equal(result@refresh_token, "refresh123")
  expect_equal(result@token_type, "Bearer")
  expect_equal(result@userId, "user123")
  expect_true(abs(as.numeric(result@expires_at - (Sys.time() + 3600))) < 2)
})

test_that(".refresh_token returns new credentials when refresh is successful", {
  server <- "https://example.org"
  credentials <- new("ArmadilloCredentials",
                     access_token = "access123",
                     refresh_token = "refresh123",
                     expires_in = 3600,
                     expires_at = Sys.time() + 3600,
                     id_token = "id123",
                     token_type = "Bearer",
                     userId = "user123")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_new_creds <- list(
    access_token = "new-access",
    refreshToken = "new-refresh",
    id_token = "new-id",
    token_type = "Bearer",
    userId = "user123"
  )

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_new_creds,
    {
      result <- DSMolgenisArmadillo:::.refresh_token(server, credentials)
    }
  )

  expect_equal(result$access_token, "new-access")
  expect_equal(result$refreshToken, "new-refresh")
})

test_that(".refresh_token stops with message if fieldErrors returned", {
  server <- "https://example.org"
  credentials <- new("ArmadilloCredentials",
                     access_token = "access123",
                     refresh_token = "refresh123",
                     expires_in = 3600,
                     expires_at = Sys.time() + 3600,
                     id_token = "id123",
                     token_type = "Bearer",
                     userId = "user123")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_error_response <- list(fieldErrors = list("Invalid refresh token"))

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_error_response,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        "Invalid refresh token"
      )
    }
  )
})

test_that(".refresh_token stops with generic message if refresh fails silently", {
  server <- "https://example.org"
  credentials <- new("ArmadilloCredentials",
                     access_token = "access123",
                     refresh_token = "refresh123",
                     expires_in = 3600,
                     expires_at = Sys.time() + 3600,
                     id_token = "id123",
                     token_type = "Bearer",
                     userId = "user123")

  dummy_auth_info <- list(auth = list(issuerUri = "https://auth.example.org"))
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_empty_response <- list()  # no refreshToken, no fieldErrors

  with_mock(
    "DSMolgenisArmadillo:::.get_oauth_info" = function(server) dummy_auth_info,
    "httr::POST" = function(...) dummy_response,
    "httr::content" = function(response) dummy_empty_response,
    {
      expect_error(
        DSMolgenisArmadillo:::.refresh_token(server, credentials),
        "Refresh failed"
      )
    }
  )
})
