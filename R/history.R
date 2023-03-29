#' Create a new item state history request
#'
#' @param params_csv Load oid map params from a CSV file
#'
#' @export
eva.history.request <- function(params_csv=NULL) {
  request <- list(params = list(fill="1T"), oid_map = list())
  if (!is.null(params_csv)) {
    params <- read.csv(params_csv)
    for (i in 1:nrow(params)) {
      row <- params[i,]
      request <- eva.history.append_oid(request, row$oid, row$status, row$value, row$database)
    }
  }
  return(request)
}

#' Append item OID mapping to a request
#'
#' @param request
#'
#' @param oid item OID
#' @param status status mapping: TRUE or col name
#' @param value value mapping: TRUE or col name
#' @param database database service to load state from
#' @param xopts extra database request options
#'
#' @export
eva.history.append_oid <- function(request, oid, status=FALSE, value=FALSE, database=NULL, xopts=NULL) {
  p <- list(status=status, value=value)
  if (!is.null(database)) p["database"] <- database
  if (!is.null(xopts)) p["xopts"] <- xopts
  request$oid_map[oid] <- list(p)
  return(request)
}

#' Fetch state history data
#'
#' @param session
#'
#' @param request Request object
#' @param t_start start time: date/time or unix timestamp
#' @param t_end end time: date/time or unix timestamp
#' @param fill fill: NX, where X: S for seconds, T for minutes, H for hours, D for days, W for weeks, e.g. 15T
#' @param limit limit result records to
#' @param database database service to perform request on
#' @param xopts extra options
#' @param t_col time column processing: keep (default) or drop
#' @param tz time zone
#'
#' @import httr
#' @import jsonlite
#' @import tibble
#' @import readr
#' @export
eva.history.fetch <- function(session, request,
                              t_start=NULL, t_end=NULL, fill=NULL, limit=NULL, database=NULL, xopts=NULL,
                              t_col="keep", tz=NULL) {
  ml_url <- session$mlkit
  params <- request$params
  if (!is.null(t_start)) params$t_start <- t_start
  if (!is.null(t_end)) params$t_end <- t_end
  if (!is.null(fill)) params$fill <- fill
  if (!is.null(limit)) params$limit <- limit
  if (ml_url == FALSE) {
    # fetch data from HMI
    params$i <- names(request$oid_map)
    if (!is.null(xopts)) params$xopts <- xopts
    if (!is.null(database)) params$database <- database
    stat <- eva.call(session, "item.state_history", params)
    if (t_col == "keep") {
      data <- tibble(time=stat$t)
      data$time <- as.POSIXct(data$time, origin="1970-01-01", tz=tz)
    } else data <- tibble(time=matrix(NA, ncol=1, nrow=length(stat$t)))
    for (oid in names(request$oid_map)) {
      p <- request$oid_map[[oid]]
      if (isTRUE(p$status) || identical(p$status,"true") || identical(p$status, 1) || identical(p$status, "1")) {
        col_name <- paste(oid, "status", sep="/")
        data[col_name] <- stat[col_name]
      } else if (is.character(p$status)) {
        col_name <- paste(oid, "status", sep="/")
        data[p$status] <- stat[col_name]
      }
      if (isTRUE(p$value) || identical(p$value,"true") || identical(p$value, 1) || identical(p$value, "1")) {
        col_name <- paste(oid, "value", sep="/")
        data[col_name] <- stat[col_name]
      } else if (is.character(p$value)) {
        col_name <- paste(oid, "value", sep="/")
        data[p$value] <- stat[col_name]
      }
    }
    if (t_col != "keep") data <- data[,-1]
    return(data)
  } else {
    if (isTRUE(ml_url)) ml_url <- session$url
    url <- paste(ml_url, "ml/api/query.item.state_history", sep="/")
    if (t_col == "drop") params$time_format <- "no" else params$time_format <- "raw"
    if (is.null(session$token)) {
      eva.authenticate(session)
      refreshed <- TRUE
    } else refreshed <- FALSE
    repeat {
      params$oid_map <- request$oid_map
      response <- POST(url, content_type_json(),
                       body = jsonlite::toJSON(params, auto_unbox=TRUE),
                       stream = TRUE,
                       add_headers("x-auth-key" = session$token),
                       add_headers("accept-encoding" = "gzip"), timeout(session$timeout))
      if (status_code(response) == 403 && endsWith(content(response), "(AUTH)")) {
        if (refreshed) break else {
          eva.authenticate(session)
          refreshed <- TRUE
        }
      } else break
    }
    if (status_code(response) != 200) stop(paste(status_code(response), content(response)))
    data = read_csv(response$content, show_col_types=FALSE)
    if (t_col == "keep") data$time <- as.POSIXct(data$time, origin="1970-01-01", tz=tz)
    return(data)
  }
}

#' Push data to a database service
#'
#' @param session
#'
#' @param request request object
#' @param data file path or a data frame
#' @param database database to push data to (default: default)
#'
#' @import httr
#' @import jsonlite
#' @import curl
#' @export
eva.history.push <- function(session, request, data, database="default") {
  c <- NULL
  out <- tryCatch(
                  {
                    ml_url <- session$mlkit
                    if (isFALSE(ml_url)) stop("the operation requires ml kit server")
                    if (isTRUE(ml_url)) ml_url <- session$url
                    params = list(params = jsonlite::toJSON(list(database = database, oid_map = request$oid_map), auto_unbox=TRUE))
                    if (typeof(data) == "character") {
                      params$file = upload_file(data)
                    } else if (typeof(data) == "list") {
                      c <- textConnection("csv", "w")
                      write.csv(data, c, row.names=FALSE)
                      params$file = curl::form_data(textConnectionValue(c), type="application/csv")
                    } else stop("unsupported data kind")
                    if (is.null(session$token)) {
                      eva.authenticate(session)
                      refreshed <- TRUE
                    } else refreshed <- FALSE
                    url <- paste(ml_url, "ml/api/push.item.state_history_csv", sep="/")
                    repeat {
                      response <- POST(url, body = params, encode = "multipart",
                                       add_headers("x-auth-key" = session$token), timeout(session$timeout))
                      if (status_code(response) == 403 && endsWith(content(response), "(AUTH)")) {
                        if (refreshed) break else {
                          eva.authenticate(session)
                          refreshed <- TRUE
                        }
                      } else break
                    }
                    if (status_code(response) != 200) stop(paste(status_code(response), content(response)))
                    return(content(response))
                  },
                  finally={
                    if (!is.null(c)) close(c)
                  }
  )
  return(out)
}
