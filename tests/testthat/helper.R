library(mockery)
library(tibble)

cookies <- tibble(
  domain = "#HttpOnly_example.org",
  name = "JSESSIONID",
  value = "abcde"
)
