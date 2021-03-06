% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bioinfo_fns.R
\name{write_bed}
\alias{write_bed}
\title{Write data.frame in BED format}
\usage{
write_bed(df, fname)
}
\arguments{
\item{df}{data.frame (typically 4+ column)}

\item{fname}{filename to save BED file}
}
\description{
Sets options to `write.table` useful for
writing BED files.
}
\examples{
write_bed(data.frame(chr=c("chr1", "chr2"),
  start=c(100, 450), end=c(200, 300), id=c("p1", "p2")),
  "outfile.bed")
}

