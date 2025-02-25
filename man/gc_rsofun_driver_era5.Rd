% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gc_rsofun_driver_era5.R
\name{gc_rsofun_driver_era5}
\alias{gc_rsofun_driver_era5}
\title{Format an ERA5 based \code{rsofun} driver}
\usage{
gc_rsofun_driver_era5(user, site_info)
}
\arguments{
\item{user}{a ECMWF CDS user name (number)}

\item{site_info}{site info data frame including a site name, location, and
other ancillary variables}
}
\value{
ERA5 based \code{rsofun} drivers for a point location specified in the
 site_info parameter
}
\description{
Uses the ECMWF CDS workflow API to summarize rsofun driver files
on a daily time step.
}
