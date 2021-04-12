test_that("credentials test", {
  set_credentials("username", "password")

  set_credentials("username","password","scihub")

  print_credentials()
})
