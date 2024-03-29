% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtoi.R
\name{read_rtoi}
\alias{read_rtoi}
\alias{read_rtoi,character-method}
\alias{read_rtoi,character}
\title{Reads an rtoi from the hard drive}
\usage{
read_rtoi(path, ...)

\S4method{read_rtoi}{character}(path, ...)
}
\arguments{
\item{path}{an rtoi object.}

\item{...}{additional arguments.}
}
\value{
rtoi object readed from disk.
}
\description{
Reads an rtoi from the hard drive
}
\examples{
\dontrun{
library(rsat)

# load example rtoi
file.copy(from=system.file("ex/Navarre",package="rsat"),
         to=tempdir(),
         recursive = TRUE)

navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
print(navarre)
}
}
