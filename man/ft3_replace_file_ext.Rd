% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_file_ext.R
\name{ft3_replace_file_ext}
\alias{ft3_replace_file_ext}
\title{Replace the extension on a file path with a new extension}
\usage{
ft3_replace_file_ext(file_name, ext, ...)
}
\arguments{
\item{file_name}{File name}

\item{ext}{New extension}

\item{...}{Additional arguments to pass to tools::file_path_sans_ext()}
}
\value{
Returns a character vector of new file names
}
\description{
Replace the extension on a file path with a new extension
}
\examples{

ft3_replace_file_ext('test.csv', 'txt')
}
