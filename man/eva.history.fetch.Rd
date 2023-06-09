% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history.R
\name{eva.history.fetch}
\alias{eva.history.fetch}
\title{Fetch state history data}
\usage{
eva.history.fetch(
  session,
  request,
  t_start = NULL,
  t_end = NULL,
  fill = NULL,
  limit = NULL,
  database = NULL,
  xopts = NULL,
  t_col = "keep",
  tz = NULL
)
}
\arguments{
\item{session}{}

\item{request}{Request object}

\item{t_start}{start time: date/time or unix timestamp}

\item{t_end}{end time: date/time or unix timestamp}

\item{fill}{fill: NX, where X: S for seconds, T for minutes, H for hours, D for days, W for weeks, e.g. 15T}

\item{limit}{limit result records to}

\item{database}{database service to perform request on}

\item{xopts}{extra options}

\item{t_col}{time column processing: keep (default) or drop}

\item{tz}{time zone}
}
\description{
Fetch state history data
}
