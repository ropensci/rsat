% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/records.R
\name{new_record}
\alias{new_record}
\alias{new_record,character,character,Date,character,character,character,numeric,numeric,character,character,character,logical,extent_crs-method}
\alias{new_record,}
\alias{character,}
\alias{Date,}
\alias{numeric,}
\alias{logical,}
\alias{extent_crs}
\alias{new_record,character,character,Date,character,character,character,numeric,numeric,character,character,character,logical,missing-method}
\alias{missing}
\title{Create a new \code{records} object}
\usage{
new_record(
  sat,
  name,
  date,
  product,
  download,
  file_path,
  path,
  row,
  tileid,
  preview,
  api_name,
  order,
  extent_crs
)

\S4method{new_record}{character,character,Date,character,character,character,numeric,numeric,character,character,character,logical,extent_crs}(
  sat,
  name,
  date,
  product,
  download,
  file_path,
  path,
  row,
  tileid,
  preview,
  api_name,
  order,
  extent_crs
)

\S4method{new_record}{character,character,Date,character,character,character,numeric,numeric,character,character,character,logical,missing}(
  sat,
  name,
  date,
  product,
  download,
  file_path,
  path,
  row,
  tileid,
  preview,
  api_name,
  order
)
}
\arguments{
\item{sat}{the name of the satellite to which the record belongs.}

\item{name}{the name of the record.}

\item{date}{the date of the record.}

\item{product}{the product.}

\item{download}{the url to download the satellite record.}

\item{file_path}{the saving directory for the satellite record.}

\item{path}{the path of the tiling system.}

\item{row}{the row of the tiling system.}

\item{tileid}{the tile id.}

\item{preview}{the url of the preview of the satellite record.}

\item{api_name}{the api name.}

\item{order}{boolean, defines if the image must be requested or not.}

\item{extent_crs}{extent (used to project the preview).}
}
\value{
records object
}
\description{
Create a new \code{records} object from scratch
}
\examples{
\dontrun{
# create a new record from scrach
rcds <- new_record(
  sat = "modis",
  name = "mod09a",
  date = as.Date("2011087", "\%Y\%j"),
  product = "product",
  download = "url/aaa/download",
  file_path = "file_path",
  path = 1,
  row = 1,
  tileid = "exampleid",
  preview = "url",
  api_name = "nasa_inventory",
  order = FALSE
)
rcds
}
}
