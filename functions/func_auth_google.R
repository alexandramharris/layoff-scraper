auth_google <- function(email, service, token_path) {
  googlesheets4::gs4_auth(email = "alexandra.harris@timesunion.com", path = tokencodr::decrypt_token(service = service,
                                                          path = token_path,
                                                          complete = TRUE))
  }
