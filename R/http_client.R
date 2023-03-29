#' Create a new EVA ICS session
#'
#' @param url HMI or front-end server URL
#'
#' @param user user login (for login/password authentication)
#' @param password user password
#' @param key API key (for API key authentication)
#' @param timeout timeout for HTTP requests
#' @param mlkit if ML Kit server is installed: TRUE (if front-end server is used) or ML Kit server URL
#'
#' @export
eva.session <- function(url="http://localhost:7727",
                        user=NULL,
                        password=NULL,
                        key=NULL,
                        timeout=120,
                        mlkit=FALSE) {
  session <- new.env()
  session$url <- url
  session$call_id <- 1
  session$user <- user
  session$password <- password
  session$key <- key
  session$token <- NULL
  session$mlkit <- mlkit
  session$timeout <- timeout
  return(session)
}
eva.get_call_id <- function(session) {
  session$call_id <- session$call_id + 1
  if (session$call_id > 0xFFFFFFFF) session$call_id <- 1;
  return(session$call_id)
}

#' Set session credentials for login/password authentication
#'
#' @param session
#'
#' @param user user login
#' @param password user password
#'
#' @export
eva.credentials <- function(session, user, password) {
  session$user <- user;
  session$password <- password;
}

#' Authenticate the session (usually not called manually)
#'
#' @param session
#'
#' @export
eva.authenticate <- function(session) {
  if (is.null(session$user) || is.null(session$password)) stop("credentials not set")
  r <- eva.call(session, "login", list(u = session$user, p = session$password))
  session$token <- r$token
}

#' Perform a RPC call on HMI service
#'
#' @param session
#'
#' @param method RPC method
#' @param params method params
#'
#' @import httr
#' @import jsonlite
#' @export
eva.call <- function(session, method, params=list()) {
  call_id <- eva.get_call_id(session)
  token_auth <- FALSE
  if (method != "login") {
    if (!is.null(session$key)) params$k <- session$key else if (!is.null(session$token)) {
      params$k <- session$token
      token_auth <- TRUE
    } else {
      eva.authenticate(session)
      params$k <- session$token
    }
  }
  req <- list(jsonrpc = "2.0", id = call_id, method = method, params = params)
  r <- POST(session$url, content_type_json(), body = jsonlite::toJSON(req, auto_unbox=TRUE),
            timeout(session$timeout))
  if (status_code(r) != 200) stop(paste("http error", status_code(r)))
  ct <- jsonlite::fromJSON(content(r, as="text", encoding="UTF-8"))
  if (ct$id != call_id) stop("invalid API response ID")
  if (!is.null(ct$error)) {
    if (ct$error$code == -32002 && token_auth) {
      session$token <- NULL
      return(eva.call(method, params))
    } else stop(paste(ct$error$code, ct$error$message))
  }
  return(ct$result)
}
#' Execute "test" RPC method on HMI service
#'
#' @param session
#'
#' @export
eva.test <- function(session) {
  return(eva.call(session, "test"))
}
