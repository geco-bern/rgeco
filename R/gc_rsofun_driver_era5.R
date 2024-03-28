#' Format an ERA5 based {rsofun} driver
#'
#' Uses the ECMWF CDS workflow API to summarize rsofun driver files
#' on a daily time step.
#'
#' @param user a ECMWF CDS user name (number)
#' @param site_info site info data frame including a site name, location, and
#'  other ancillary variables
#'
#' @return ERA5 based {rsofun} drivers for a point location specified in the
#'  site_info parameter
#' @export

gc_rsofun_driver_era5 <- function(
    user,
    site_info
    ){

  print(site_info)

  # format settings to use  during
  # the download
  settings <- data.frame(
    variable = c(
      "2m_temperature",
      "2m_temperature",
      "2m_temperature",
      "2m_dewpoint_temperature",
      "surface_pressure",
      "total_cloud_cover",
      "snowfall",
      "total_precipitation",
      'surface_solar_radiation_downwards'
    ),
    method = c(
      "mean",
      "min",
      "max",
      "mean",
      "mean",
      "mean",
      "sum",
      "sum",
      "mean"
    ),
    product = c(
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels',
      'reanalysis-era5-single-levels'
    )
  )

  settings$filename <- paste0(settings$variable,"_",settings$method,".csv")

  # download all drivers, use site info
  # to determine locality etc
  requests <- apply(settings, 1, function(x){
    output <- gc_era5_request(
      user = user,
      lon = site_info$lon,
      lat = site_info$lat,
      product = x['product'],
      var = x["variable"],
      filename = x["filename"],
      start_date = site_info$date_start,
      end_date = site_info$date_end,
      method = x["method"]
    )
    return(output)
  })

  # download the data
  files <- wf_request_batch(
    user = user,
    requests,
    time_out = 3 * 3600, # 3 hour time out
    workers = 9
  )

  if(!inherits(files, "try-error")) {
    drivers <- lapply(files, function(file){

      df <- read.table(file, sep = ",", header = TRUE)
      df <- df[,c(1,5)]
    })

    # combine the drivers
    forcing <- bind_cols(drivers)

    # reformat the drivers / unit conversions etc

    # lapse rate corrections?

    # check units solar radiation

  } else {
    message("failed download")
  }

  return(dplyr::tibble(site_info, forcing))
}
