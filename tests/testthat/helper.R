library(mockery)

cookies <- structure(list(
  domain = "#HttpOnly_localhost",
  flag = FALSE,
  path = "/",
  secure = FALSE,
  expiration = structure(Inf,
    class = c("POSIXct", "POSIXt")
  ),
  name = "JSESSIONID",
  value = "abcde"
), row.names = c(
  NA,
  -1L
), class = "data.frame")
handle <- httr::handle(url = "http://example.org:8080")
connection <- methods::new("ArmadilloConnection",
  name = "name",
  handle = handle,
  workspaces = list("GECKO/customer"),
  user = "admin",
  cookies = cookies
)
