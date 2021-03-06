% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empty_data.R
\name{insert_holes_in}
\alias{insert_holes_in}
\title{Obfuscate data ny inserting random NA and/or NULL noise}
\usage{
insert_holes_in(data, ..., props = c(0.1, 0.2), values = list(NA, NULL))
}
\arguments{
\item{data}{The data frame to obfuscate}

\item{...}{Names of columns to obfuscate (NONFUNCT)}

\item{props}{Range of randomly selected proportion of data to obfuscate}

\item{values}{Empty values to insert (NONFUNCT)}
}
\description{
Obfuscate data ny inserting random NA and/or NULL noise
}

