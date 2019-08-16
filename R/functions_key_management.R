#' Set or get a Scopus user key.
#'
#' Reads (or writes) the user key to an environment variable.
#'
#' Request a user key from Scopus \url{https://dev.elsevier.com/}
#' in order to use the API. This key gets appended to GET queries via the
#' \code{user_key} parameter. The key will be saved so you don't have to enter
#' it repeatedly.
#'
#' @param force Overwrite existing key? If TRUE, the user will be prompted to
#'  enter a user key even if one has already been entered before.
#' @export
scopus_key <- function(force = FALSE) {
  key_name <- "SCOPUSE_KEY"
  env <- Sys.getenv(key_name)
  if (!identical(env, "") && !force)
    return(env)

  # this is where the file does/will live
  file_name <- paste(system.file("extdata", package="TidyScientometrix"),
                     "/", key_name, sep="")

  # if the file exists, just read the key from there and quietly set the
  # environment variable
  if (file.exists(file_name) && !force) {
    key <- readRDS(file_name)
    args = list(key)
    names(args) = key_name
    do.call(Sys.setenv, args)
    return(key)
  }

  if (!interactive()) {
    stop("Please set env var SCOPUS_KEY to your crunchbase user key",
         call. = FALSE)
  }


  key <- readline(paste("Please enter your Scopus API key: "))

  if (identical(key, "")) {
    stop("Scopus user_key entry failed", call. = FALSE)
  }

  message("Updating SCOPUS_KEY environment variable")
  Sys.setenv(SCOPUS_KEY = key)

  args = list(key)
  names(args) = key_name
  do.call(Sys.setenv, args)
  saveRDS(key, file_name)
  key
}
