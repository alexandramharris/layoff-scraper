auth_google <- function(email, service, token_path) {
  googlesheets4::gs4_auth(email = "alexandra.harris@timesunion.com", path = tokencodr::decrypt_token(service = gsheet_layoffs,
                                                          path = .secret/gsheet_layoffs,
                                                          complete = TRUE))
}
