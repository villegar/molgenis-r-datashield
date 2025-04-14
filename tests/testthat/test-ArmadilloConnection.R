test_that("dsDisconnect calls /logout endpoint ", {
  post <- mock()
  with_mock(
    "httr::POST" = post,
    dsDisconnect(connection)
  )
  expect_called(post, 1)
  expect_args(post, 1,
              handle = handle,
              path = "/logout",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsDisconnect saves the workspace", {
  post <- mock(list(status_code = 200), cycle = TRUE)
  with_mock(
    "httr::POST" = post,
    dsDisconnect(connection, save = "keepit")
  )
  expect_called(post, 2)
  expect_args(post, 1,
              handle = handle,
              path = "/workspaces/keepit",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsListProfiles retrieves profiles", {
  profiles <- list(
                   available = list("default", "exposome"),
                   current = "exposome")
  get <- mock(list(status_code = 200))
  content <- mock(profiles)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListProfiles(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/profiles",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, profiles)
})


test_that("dsListProfiles returns default result if none found", {
  get <- mock(list(status_code = 404))
  content <- mock(profiles)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListProfiles(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/profiles",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, list(
    available = "default",
    current = "default"
  ))
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
  expect_args(get, 1,
              handle = handle,
              path = "/tables",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(tables))
})

test_that("dsListResources retrieves resources", {
  resources <- list("a", "b")
  get <- mock(list(status_code = 200))
  content <- mock(resources)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = content,
    dsListResources(connection)
  )
  expect_args(get, 1,
              handle = handle,
              path = "/resources",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(resources))
})

test_that("dsHasTable returns TRUE if table exists", {
  head <- mock(list(status_code = 200))
  with_mock(
    "httr::HEAD" = head,
    expect_true(dsHasTable(connection, "project/folder/name.parquet"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/tables/project/folder/name.parquet",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasTable returns FALSE if table doesnot exist", {
  head <- mock(list(status_code = 404))
  with_mock(
    "httr::HEAD" = head,
    expect_false(dsHasTable(connection, "project/folder/name.parquet"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/tables/project/folder/name.parquet",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasResource returns TRUE if resource exists", {
  head <- mock(list(status_code = 200))
  with_mock(
    "httr::HEAD" = head,
    expect_true(dsHasResource(connection, "project/folder/name"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/resources/project/folder/name",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsHasResource returns FALSE if table doesnot exist", {
  head <- mock(list(status_code = 404))
  with_mock(
    "httr::HEAD" = head,
    expect_false(dsHasResource(connection, "project/folder/name"))
  )
  expect_args(head, 1,
    handle = handle,
    path = "/resources/project/folder/name",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsIsAsync returns boolean list", {
  expect_equal(
    dsIsAsync(connection),
    list(
      aggregate = TRUE,
      assignTable = TRUE,
      assignResource = TRUE,
      assignExpr = TRUE
    )
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
  expect_args(get, 1,
              handle = handle,
              path = "/symbols",
              config = httr::add_headers("Authorization" = "Bearer token"))
  expect_equal(result, unlist(symbols))
})

test_that("dsRmSymbol removes symbol", {
  delete <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::DELETE" = delete,
    dsRmSymbol(connection, "D")
  )
  expect_args(delete, 1,
              handle = handle,
              path = "/symbols/D",
              config = httr::add_headers("Authorization" = "Bearer token"))
})

test_that("dsAssignTable assigns table to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignTable(connection, "D", "project/folder/name.parquet")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet",
      symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignTable allows variable selection", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignTable(connection, "D",
      "project/folder/name.parquet",
      variables = c("foo", "bar")
    )
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet",
      symbol = "D",
      async = TRUE,
      variables = "foo,bar"
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
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
    dsAssignTable(connection, "D", "project/folder/name.parquet")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-table",
    query = list(
      table = "project/folder/name.parquet", symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignResource assigns resource to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignResource(connection, "D", "project/folder/name")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-resource",
    query = list(
      resource = "project/folder/name",
      symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignResource, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    dsAssignResource(connection, "D", "project/folder/name")
  )
  expect_args(post, 1,
    handle = handle,
    path = "/load-resource",
    query = list(
      resource = "project/folder/name", symbol = "D",
      async = TRUE
    ),
    config = httr::add_headers("Authorization" = "Bearer token")
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

  expect_args(get, 1,
              handle = handle,
              path = "/methods/assign",
              config = httr::add_headers("Authorization" = "Bearer token"))

  expected <- tibble(
    name = c("foo", "bar"),
    value = c("dsBase::foo", "dsBase::bar"),
    version = c("6.0.0-03", "6.0.0-03"),
    package = c("dsBase", "dsBase"),
    type = "assign",
    class = "function"
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
    name = c("minqa", "RANN"),
    version = c("1.2.4", "2.6.1")
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

  expect_equivalent(result, tibble(
    name = c("hello", "world"),
    size = c(48L, 378L),
    ETag = c(
      "\"ca5d3a000723844e874b93a65c3888a1-1\"",
      "\"1f45d1a6f13adda1d05adf1f3da4c4ca-1\""
    ),
    lastAccessDate = c(
      "2020-05-06T13:09:00.725Z",
      "2020-05-06T13:09:07.617Z"
    ),
    user = NA_character_
  ))
})

test_that("dsSaveWorkspace saves workspace", {
  post <- mock(list(status_code = 201))

  with_mock(
    "httr::POST" = post,
    dsSaveWorkspace(connection, "keepit")
  )

  expect_args(post, 1,
    handle = handle,
    path = "/workspaces/keepit",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsRmWorkspace removes workspace", {
  delete <- mock(list(status_code = 204))

  with_mock(
    "httr::DELETE" = delete,
    dsRmWorkspace(connection, "keepit")
  )

  expect_args(delete, 1,
    handle = handle,
    path = "/workspaces/keepit",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsAssignExpr assigns expression to symbol", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignExpr(connection, "D", "ls()")
  )

  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignExpr deparses function calls in expression", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAssignExpr(connection, "D", call("ls"))
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
})

test_that("dsAssignExpr, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(NULL)
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    dsAssignExpr(connection, "D", "ls()", async = FALSE)
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = FALSE),
    path = "/symbols/D",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_args(retry, 1,
    verb = "GET",
    handle = handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    config = httr::add_headers(c("Accept" = "application/octet-stream",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAggregate executes deparsed query", {
  post <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::POST" = post,
    dsAggregate(connection, call("ls"))
  )

  expect_args(post, 1,
    handle = handle,
    query = list(async = TRUE),
    path = "/execute",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
})

test_that("dsAssignExpr, when called synchronously, waits for result", {
  post <- mock(list(status_code = 200))
  retry <- mock(list(status_code = 200))
  httr_content <- mock(base::serialize("Hello World!", NULL))
  result <- with_mock(
    "httr::POST" = post,
    "httr::RETRY" = retry,
    "httr::content" = httr_content,
    dsAggregate(connection, "ls()", async = FALSE)
  )
  expect_args(post, 1,
    handle = handle,
    query = list(async = FALSE),
    path = "/execute",
    body = "ls()",
    config = httr::add_headers(c("Content-Type" = "text/plain",
                                 "Authorization" = "Bearer token"))
  )
  expect_args(retry, 1,
    verb = "GET",
    handle = handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    config = httr::add_headers(c("Accept" = "application/octet-stream",
                                 "Authorization" = "Bearer token"))
  )
  expect_s4_class(result, "ArmadilloResult")
  expect_equal(dsFetch(result), "Hello World!")
})

test_that("dsAssignExpr handles error when called synchronously", {
  post <- mock(list(status_code = 500))
  get <- mock(list(status_code = 200))
  content <- mock(list(
    status = "FAILED",
    message = "Error"
  ))
  error <- expect_error(with_mock(
    "httr::POST" = post,
    "httr::GET" = get,
    "httr::content" = content,
    dsAggregate(connection, "ls()", async = FALSE)
  ), "Internal server error: Error")
})

test_that("dsGetInfo returns server info", {
  get <- mock(list(status_code = 200))
  server_info <- list(
    git = list(
      branch = "master",
      commit = list(
        id = "1a8ec4e",
        time = "2020-04-29T10:32:51Z"
      )
    ),
    build = list(
      java = list(version = "11"),
      version = "0.0.1-SNAPSHOT",
      artifact = "datashield",
      name = "datashield",
      time = "2020-04-29T13:18:59.833Z",
      group = "org.molgenis"
    )
  )
  httr_content <- mock(server_info)
  result <- with_mock(
    "httr::GET" = get,
    "httr::content" = httr_content,
    dsGetInfo(connection)
  )

  expected <- list(
    git = server_info$git,
    build = server_info$build,
    url = connection@handle$url,
    name = connection@name,
    cookies = connection@cookies
  )
  expect_equivalent(result, expected)
  expect_args(get, 1,
    handle = handle,
    path = "/actuator/info",
    config = httr::add_headers("Authorization" = "Bearer token")
  )
})

test_that("dsKeepAlive pings server info endpoint", {
  get <- mock(list(status_code = 200))
  result <- with_mock(
    "httr::GET" = get,
    dsKeepAlive(connection)
  )

  expect_args(get, 1,
    handle = handle,
    path = "/actuator/info",
    config = httr::add_headers("Authorization" = "Bearer token")
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

test_that(".reset_token_if_expired refreshes and updates token if expired", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  setClass("opal", contains = "environment")

  handle <- structure(list(handle = new("externalptr"), url = "https://localhost"), class = "handle")

  expired_cred <- new("ArmadilloCredentials",
                      access_token = "expired-token",
                      expires_in = 3600,
                      expires_at = Sys.time() - 10,  # expired
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

  # Backup original functions (if defined)
  old_get <- if (exists(".get_armadillo_credentials", inherits = TRUE)) get(".get_armadillo_credentials", inherits = TRUE) else NULL
  old_refresh <- if (exists(".refresh_token", inherits = TRUE)) get(".refresh_token", inherits = TRUE) else NULL
  old_reset <- if (exists(".reset_token_global_env", inherits = TRUE)) get(".reset_token_global_env", inherits = TRUE) else NULL

  # Assign mocks
  assign(".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  }, envir = globalenv())

  assign(".refresh_token", function(url, credentials) {
    list(token = "new-token", refreshToken = "new-refresh")
  }, envir = globalenv())

  assign(".reset_token_global_env", function(old_credentials, new_credentials, conn, env = globalenv()) {
    conn@token <- new_credentials$token
    assign("updated_conn_result", conn, envir = env)
  }, envir = globalenv())

  # Clean up mocks after test
  on.exit({
    if (!is.null(old_get)) assign(".get_armadillo_credentials", old_get, envir = globalenv()) else rm(".get_armadillo_credentials", envir = globalenv())
    if (!is.null(old_refresh)) assign(".refresh_token", old_refresh, envir = globalenv()) else rm(".refresh_token", envir = globalenv())
    if (!is.null(old_reset)) assign(".reset_token_global_env", old_reset, envir = globalenv()) else rm(".reset_token_global_env", envir = globalenv())
    if (exists("updated_conn_result", envir = globalenv())) rm("updated_conn_result", envir = globalenv())
  })

  # Run
  .reset_token_if_expired(conn)
  updated_conn <- get("updated_conn_result", envir = globalenv())

  expect_s4_class(updated_conn, "ArmadilloConnection")
  expect_equal(updated_conn@token, "new-token")
})

test_that(".reset_token_if_expired errors with cli_abort when refresh fails", {
  if (!"methods" %in% loadedNamespaces()) library(methods)

  setClass("opal", contains = "environment")

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

  # Backup original functions
  old_get <- if (exists(".get_armadillo_credentials", inherits = TRUE)) get(".get_armadillo_credentials", inherits = TRUE) else NULL
  old_refresh <- if (exists(".refresh_token", inherits = TRUE)) get(".refresh_token", inherits = TRUE) else NULL

  # Assign mocks
  assign(".get_armadillo_credentials", function(conn) {
    list(name = "cohort_1", object = expired_cred)
  }, envir = globalenv())

  assign(".refresh_token", function(url, credentials) {
    stop("mocked refresh failure")
  }, envir = globalenv())

  # Clean up mocks after test
  on.exit({
    if (!is.null(old_get)) assign(".get_armadillo_credentials", old_get, envir = globalenv()) else rm(".get_armadillo_credentials", envir = globalenv())
    if (!is.null(old_refresh)) assign(".refresh_token", old_refresh, envir = globalenv()) else rm(".refresh_token", envir = globalenv())
  })

  expect_warning(
    tryCatch(
      .reset_token_if_expired(conn),
      error = function(e) warning(paste0("Failed to reset token: ", e$message))
    ),
    "Failed to reset token: mocked refresh failure"
  )
})
