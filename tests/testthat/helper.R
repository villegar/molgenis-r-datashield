library(mockery)
library(tibble)

cookies <- tibble(
  domain = "#HttpOnly_example.org",
  name = "JSESSIONID",
  value = "abcde"
)
handle <- httr::handle(url = "http://example.org:8080")
connection <- methods::new("ArmadilloConnection",
  name = "name",
  handle = handle,
  workspaces = list("GECKO/customer"),
  user = "admin",
  cookies = cookies
)
