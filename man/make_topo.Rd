% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_topo.R
\name{make_topo}
\alias{make_topo}
\title{Create topo input file for 3DCMCCFEM model}
\usage{
make_topo(site = NULL, elev, outdir)
}
\arguments{
\item{site}{Character: Name of the site. Used to construct output name}

\item{elev}{Numeric vector: elevation of the site}

\item{outdir}{Character: path to the output directory.}
}
\value{
Used for its side effects.It save in \code{outdir} as many topo
input files as the length of \code{elev} provided.
}
\description{
\code{make_topo} create the topo.txt file following the 3D-CMCC-FEM model
specification
}
\details{
The function can accept a vector of elevation and return as many
files as the length of the \code{elev} vector.The output topo.txt file has
the columns: \itemize{ \item \emph{X}: x cell position \item \emph{Y}: y
cell position \item \emph{ELEV}: Elevation of the stand

} For more info on the topo input data needed see the official \href{
http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
Guide}
}
\seealso{
\code{\link[=run_3DFEM]{run_3DFEM}}
}
