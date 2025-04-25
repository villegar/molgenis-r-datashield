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

test_that(".get_oauth_info returns content when request is successful", {
  dummy_response <- structure(list(status_code = 200), class = "response")
  dummy_content <- list(auth = list(clientId = "abc123", issuerUri = "https://auth.example.org"))

  with_mock(
    "httr::GET" = function(url) dummy_response,
    "httr::stop_for_status" = function(response, task) invisible(response),
    "httr::content" = function(response) dummy_content,
    {
      result <- DSMolgenisArmadillo:::.get_oauth_info("https://example.org")
    }
  )

  expect_equal(result$auth$clientId, "abc123")
  expect_equal(result$auth$issuerUri, "https://auth.example.org")
})

test_that(".get_oauth_info stops if server info fetch fails", {
  dummy_response <- structure(list(status_code = 404), class = "response")

  with_mock(
    "httr::GET" = function(url) dummy_response,
    "httr::stop_for_status" = function(response, task) {
      stop("404 Not Found")
    },
    {
      expect_error(
        DSMolgenisArmadillo:::.get_oauth_info("https://example.org"),
        "404 Not Found"
      )
    }
  )
})


test_that(".get_all_armadillo_credentials finds all credentials in test environment", {
  test_1 <- new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  test_2 <- new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer",
    userId = "bbb-c"
  )

  expect_equal(
    .get_all_armadillo_credentials(env = environment()),
    list(
      test_1 = test_1,
      test_2 = test_2
    )
  )
})

test_that(".get_all_armadillo_credentials returns nothing if no connections present", {

  expect_null(
    .get_all_armadillo_credentials(env = environment())
  )

})


test_that("get_matching_credential returns correct match when there is one credentials object and it has a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  cohort_1_cred <- list(cohort_1 = credentials_1)

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-a"
  )

  expect_equal(
    .get_matching_credential(cohort_1_cred, conn_1),
    list(
      name = "cohort_1",
      object = cohort_1_cred$cohort_1
    )
  )
})

test_that("get_matching_credential returns correct match when at least two credentials object but only has a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  credentials_2 <-new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer",
    userId = "bbb-c"
  )

  cohort_mult_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_2
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_2",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "bbb-a"
  )

  expect_equal(
    .get_matching_credential(cohort_mult_cred, conn_1),
    list(
      name = "cohort_2",
      object = cohort_mult_cred$cohort_2
    )
  )
})


test_that("get_matching_credential returns first match when there is are two identical credentials objects and both have a matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  cohort_identical_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_1
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_2",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-a"
  )

  expect_equal(
    .get_matching_credential(cohort_identical_cred, conn_1),
    list(
      name = "cohort_1",
      object = cohort_identical_cred$cohort_1
    )
  )
})

test_that("get_matching_credential returns NULL there is one credentials object and no matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  cohort_1_cred <- list(cohort_1 = credentials_1)

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "bbb-b"
  )

  expect_null(
    .get_matching_credential(cohort_1_cred, conn_1)
  )
})

test_that("get_matching_credential returns NULL when there are two credentials object and no matching token", {
  credentials_1 <-new(
    "ArmadilloCredentials",
    access_token = "aaa-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "aaa-b",
    token_type = "Bearer",
    userId = "aaa-c"
  )

  credentials_2 <-new(
    "ArmadilloCredentials",
    access_token = "bbb-a",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:15:36", tz = "CET"),
    id_token = "original_id_token_3",
    refresh_token = "bbb-b",
    token_type = "Bearer",
    userId = "bbb-c"
  )

  cohort_mult_cred <- list(
    cohort_1 = credentials_1,
    cohort_2 = credentials_2
  )

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "ccc-c"
  )

  expect_null(
    .get_matching_credential(cohort_mult_cred, conn_1)
  )
})

test_that(".get_armadillo_credentials returns the correct match", {
  env <- new.env()

  cred <- new(
    "ArmadilloCredentials",
    access_token = "abc123",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-abc",
    refresh_token = "ref-abc",
    token_type = "Bearer",
    userId = "user-abc"
  )

  assign("test_cred", cred, envir = env)

  conn <- new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(),
    token = "abc123"
  )

  result <- .get_armadillo_credentials(conn, env = env)

  expect_type(result, "list")
  expect_equal(result$name, "test_cred")
  expect_identical(result$object, cred)
})

test_that(".get_armadillo_credentials returns NULL when no matching credential found", {
  env <- new.env()

  cred <- new(
    "ArmadilloCredentials",
    access_token = "abc123",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-abc",
    refresh_token = "ref-abc",
    token_type = "Bearer",
    userId = "user-abc"
  )

  assign("test_cred", cred, envir = env)

  conn <- new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(),
    token = "zzz999"
  )

  expect_null(.get_armadillo_credentials(conn, env = env))
})

test_that(".get_armadillo_credentials returns NULL when there are no credentials", {
  env <- new.env()
  expect_null(.get_armadillo_credentials(conn = new("ArmadilloConnection", token = "abc"), env = env))
})

test_that(".get_armadillo_credentials works with custom env and multiple credentials", {
  env <- new.env()

  cred1 <- new(
    "ArmadilloCredentials",
    access_token = "token-1",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-1",
    refresh_token = "ref-1",
    token_type = "Bearer",
    userId = "user-1"
  )

  cred2 <- new(
    "ArmadilloCredentials",
    access_token = "token-2",
    expires_in = 29,
    expires_at = Sys.time(),
    id_token = "id-2",
    refresh_token = "ref-2",
    token_type = "Bearer",
    userId = "user-2"
  )

  assign("cred_one", cred1, envir = env)
  assign("cred_two", cred2, envir = env)

  conn <- new(
    "ArmadilloConnection",
    name = "cohort_X",
    handle = handle,
    user = "",
    cookies = list(),
    token = "token-2"
  )

  result <- .get_armadillo_credentials(conn, env = env)

  expect_type(result, "list")
  expect_equal(result$name, "cred_two")
  expect_identical(result$object, cred2)
})

test_that(".reset_armadillo_credentials correctly updates tokens", {
  test_env <- new.env()

  # Original credentials
  old_cred <- new(
    "ArmadilloCredentials",
    access_token = "old-token",
    expires_in = 29,
    expires_at = as.POSIXct("2025-03-26 11:00:00", tz = "CET"),
    id_token = "id_token_x",
    refresh_token = "old-refresh",
    token_type = "Bearer",
    userId = "user-123"
  )

  assign("cohort_1", old_cred, envir = test_env)

  old_credentials <- list(
    name = "cohort_1",
    object = old_cred
  )

  new_credentials <- list(
    token = "new-token",
    refreshToken = "new-refresh"
  )

  .reset_armadillo_credentials(old_credentials, new_credentials, env = test_env)

  updated <- get("cohort_1", envir = test_env)

  expect_s4_class(updated, "ArmadilloCredentials")
  expect_equal(updated@access_token, "new-token")
  expect_equal(updated@refresh_token, "new-refresh")

  # Check that other slots remain unchanged
  expect_equal(updated@id_token, "id_token_x")
  expect_equal(updated@token_type, "Bearer")
  expect_equal(updated@userId, "user-123")
})

test_that(".reset_armadillo_credentials modifies globalenv by default", {
  # Set up in globalenv (clean up after test)
  on.exit(rm("cohort_2", envir = globalenv()), add = TRUE)

  old_cred <- new(
    "ArmadilloCredentials",
    access_token = "global-token",
    expires_in = 10,
    expires_at = Sys.time(),
    id_token = "id_token_y",
    refresh_token = "global-refresh",
    token_type = "Bearer",
    userId = "user-456"
  )

  assign("cohort_2", old_cred, envir = globalenv())

  old_credentials <- list(
    name = "cohort_2",
    object = old_cred
  )

  new_credentials <- list(
    token = "new-global-token",
    refreshToken = "new-global-refresh"
  )

  .reset_armadillo_credentials(old_credentials, new_credentials)

  updated <- get("cohort_2", envir = globalenv())

  expect_equal(updated@access_token, "new-global-token")
  expect_equal(updated@refresh_token, "new-global-refresh")
})

test_that(".reset_connections_object updates token in correct ArmadilloConnection", {
  test_env <- new.env()

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-old"
  )

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")
  setClass("OpalConnection",
           slots = c(name = "character", opal = "opal"))

  conn_2 <- new("OpalConnection",
                name = "cohort_2",
                opal = new("opal"))

  conns <- list(conn_1, conn_2)
  names(conns) <- c("cohort_1", "cohort_2")

  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer",
                  userId = "user_dummy")

  old_credentials <- list(name = "cohort_1", object = old_cred)
  new_credentials <- list(token = "aaa-new")

  # Run test
  .reset_connections_object(old_credentials, new_credentials, conn = cohort_1, env = test_env)

  updated_conns <- get("conns", envir = test_env)
  expect_equal(updated_conns$cohort_1@token, "aaa-new")
  expect_equal(updated_conns$cohort_2@name, "cohort_2")
  expect_s4_class(updated_conns$cohort_2, "OpalConnection")
})

test_that(".reset_connections_object only updates first matching ArmadilloConnection", {
  test_env <- new.env()

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")

  conn_1 <- new("ArmadilloConnection",
                name = "cohort_1",
                handle = handle,
                user = "",
                cookies = list(
                  domain = "#HttpOnly_localhost",
                  flag = FALSE,
                  path = "/",
                  secure = FALSE,
                  expiration = "Inf",
                  name = "JSESSIONID",
                  value = "12345"),
                token = "aaa-old")

  conn_2 <- new("ArmadilloConnection",
                name = "cohort_2",
                handle = handle,
                user = "",
                cookies = list(
                  domain = "#HttpOnly_localhost",
                  flag = FALSE,
                  path = "/",
                  secure = FALSE,
                  expiration = "Inf",
                  name = "JSESSIONID",
                  value = "67890"),
                token = "aaa-old")

  conn_3 <- new("OpalConnection",
                name = "server2",
                opal = new("opal"))

  conns <- list(cohort_1 = conn_1, cohort_2 = conn_2, server2 = conn_3)
  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer",
                  userId = "user_dummy")

  old_credentials <- list(name = "cohort_1", object = old_cred)
  new_credentials <- list(token = "new-token")

  .reset_connections_object(old_credentials, new_credentials, conn = conn_1, env = test_env)

  updated <- get("conns", envir = test_env)
  expect_equal(updated$cohort_1@token, "new-token")
  expect_equal(updated$cohort_2@token, "aaa-old")
  expect_s4_class(updated$server2, "OpalConnection")
})

test_that(".reset_token_global_env updates credentials and connection", {
  test_env <- new.env()

  conn_1 <-  new(
    "ArmadilloConnection",
    name = "cohort_1",
    handle = handle,
    user = "",
    cookies = list(
      domain = "#HttpOnly_localhost",
      flag = FALSE,
      path = "/",
      secure = FALSE,
      expiration = "Inf",
      name = "JSESSIONID",
      value = "12345"),
    token = "aaa-old"
  )

  if (!"methods" %in% loadedNamespaces()) library(methods)
  setClass("opal", contains = "environment")
  conn_2 <- new("OpalConnection",
                name = "cohort_2",
                opal = new("opal"))

  conns <- list(conn_1, conn_2)
  names(conns) <- c("cohort_1", "cohort_2")

  assign("conns", conns, envir = test_env)

  old_cred <- new("ArmadilloCredentials",
                  access_token = "aaa-old",
                  expires_in = 3600,
                  expires_at = Sys.time(),
                  id_token = "id_token_dummy",
                  refresh_token = "refresh_old",
                  token_type = "Bearer",
                  userId = "user_dummy")

  assign("cohort_1", old_cred, envir = test_env)

  old_credentials <- list(name = "cohort_1", object = old_cred)
  new_credentials <- list(
    token = "aaa-new",
    refreshToken = "refresh_new"
  )

  .reset_token_global_env(old_credentials, new_credentials, conn, env = test_env)

  updated_conns <- get("conns", envir = test_env)
  updated_creds <- get("cohort_1", envir = test_env)

  expect_equal(updated_conns$cohort_1@token, "aaa-new")
  expect_equal(updated_creds@access_token, "aaa-new")
  expect_equal(updated_creds@refresh_token, "refresh_new")
})

library(mockery)

test_that(".reset_token_if_expired refreshes and updates token if expired", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  if (!isClass("opal")) setClass("opal", contains = "environment")

  handle <- structure(list(handle = new("externalptr"), url = "https://localhost"), class = "handle")

  expired_cred <- new("ArmadilloCredentials",
                      access_token = "expired-token",
                      expires_in = 3600,
                      expires_at = Sys.time() - 10,
                      id_token = "id_token_dummy",
                      refresh_token = "old-refresh",
                      token_type = "Bearer",
                      userId = "user_dummy")

  conn <- new("ArmadilloConnection",
              name = "cohort_1",
              handle = handle,
              user = "",
              cookies = list(
                domain = "#HttpOnly_localhost",
                flag = FALSE,
                path = "/",
                secure = FALSE,
                expiration = "Inf",
                name = "JSESSIONID",
                value = "12345"),
              token = "expired-token")

  updated_conn_env <- new.env()

  stub(.reset_token_if_expired, ".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  })

  stub(.reset_token_if_expired, ".refresh_token", function(url, credentials) {
    list(token = "new-token", refreshToken = "new-refresh")
  })

  stub(.reset_token_if_expired, ".reset_token_global_env", function(old_credentials, new_credentials, conn, env = updated_conn_env) {
    conn@token <- new_credentials$token
    assign("updated_conn_result", conn, envir = env)
  })

  .reset_token_if_expired(conn)

  updated_conn <- get("updated_conn_result", envir = updated_conn_env)
  expect_s4_class(updated_conn, "ArmadilloConnection")
  expect_equal(updated_conn@token, "new-token")
})

library(mockery)

test_that(".reset_token_if_expired returns warning when refresh fails", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  if (!isClass("opal")) setClass("opal", contains = "environment")

  handle <- structure(list(handle = new("externalptr"), url = "https://localhost"), class = "handle")

  expired_cred <- new("ArmadilloCredentials",
                      access_token = "expired-token",
                      expires_in = 3600,
                      expires_at = Sys.time() - 10,
                      id_token = "id_token_dummy",
                      refresh_token = "old-refresh",
                      token_type = "Bearer",
                      userId = "user_dummy")

  conn <- new("ArmadilloConnection",
              name = "cohort_1",
              handle = handle,
              user = "",
              cookies = list(
                domain = "#HttpOnly_localhost",
                flag = FALSE,
                path = "/",
                secure = FALSE,
                expiration = "Inf",
                name = "JSESSIONID",
                value = "12345"),
              token = "expired-token")

  stub(.reset_token_if_expired, ".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  })

  stub(.reset_token_if_expired, ".refresh_token", function(url, credentials) {
    stop("mocked refresh failure")
  })

  expect_warning(
    tryCatch(
      .reset_token_if_expired(conn),
      error = function(e) warning(paste0("Failed to reset token: ", e$message))
    ),
    "Failed to reset token: mocked refresh failure"
  )
})
