% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_CCS.R
\name{make_CCS}
\alias{make_CCS}
\title{Create detrended meteo input file for the current-climate scenario (CCS).}
\usage{
make_CCS(
  meteo = NULL,
  co2 = NULL,
  obs_year = NULL,
  end_year = 2099,
  outdir = NULL
)
}
\arguments{
\item{meteo}{Character or data.frame: Historical meteo data or path to a
meteo.txt file. Must have columns "Year", "Month", "n_days", "Rg_f",
"Ta_f", "Tmax", "Tmin", "RH_f", "Ts_f", "Precip", "SWC", "LAI", "ET",
"WS_f" (see details). Can be created for European sites with the function
\code{\link{make_meteo_EOBS}}, using E-OBS data}

\item{co2}{Character or data.frame: Historical CO2 data or path to co2.txt
file. Must have columns "year" and "CO2_ppm"}

\item{obs_year}{Numeric vector of length 2: first and last year of historical
meteo and CO2 data to be used to generate the future detrended weather.
Must be present in both meteo and co2 data.frame}

\item{end_year}{Numeric: Last year of simulated meteo and co2 data. Default
to 2099}

\item{outdir}{Character: path to the output directory.}
}
\value{
Used for its side effects.It save in \code{outdir} a meteo.txt and
co2.txt file with the same columns of the input data
}
\description{
\code{make_CCS} use an historical meteo input file to generate a new,
detrended meteo input file for the future (called current-climate scenario).
}
\details{
The input meteo data.frame must have the colums:
\itemize{
\item \emph{Year}: Reference year for the meteo data
\item \emph{Month}: number of month in the year
\item \emph{n_day}: number of day in the month
\item \emph{Rg_f}: Solar radiation on the ground (in  Wh/m-2)
\item \emph{Ta_f}: Mean temperature (in °C)
\item \emph{Tmax}: Maximum temperature (in °C)
\item \emph{Tmin}: Minimum temperature (in °C)
\item \emph{RH_f}: relative humidity (in \%)
\item \emph{Ts_f}: Daily soil temperature (in °C)
\item \emph{Precip}: Daily precipitation (in mm day-1)
\item \emph{SWC}:Soil Water Content (in mm m-2)*
\item \emph{LAI}:Leaf Area Index (in m2 m-2) *
\item \emph{ET}: Evapotranspiration (in mm m-2 day-1*
\item \emph{WS_f}: Wind speed (in m s-1)*
}

The input co2 data.frame must have the colums:
\itemize{
\item \emph{Year}: Reference year for the co2 data
\item \emph{CO2_ppm}: CO2 concentration (in ppm)
}

All the year of simulation must be included in the meteo and co2 input
files. For more info on the stand input data needed see the official \href{
http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
Guide}
}
\seealso{
\code{\link{run_3DFEM}}
}
