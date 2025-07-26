#' Tabular data regridding
#'
#' For spatial data read into a data frame with longitude and latitude specified
#' for each observation, this function aggregates data to a new set of longitude/
#' latitude combinations, i.e., to a new grid. This effectively takes the mean
#' across all gridcells that have their mid-point within the coarse-resolution
#' gridcell and without area-weighing of constituent (fine-resolution) gridcells.
#'
#' @param df A data frame. Must have columns named \code{'lon'} and \code{'lat'}.
#' @param res Resolution of the new grid in decimal degrees
#' @param lon_start The left (west) margin of the first (western-most) gridcell
#' along longitudes.
#' @param lon_end The right (east) margin of the first (eastern-most) gridcell
#' along longitudes.
#' @param lat_start The lower (south) margin of the first (southern-most) gridcell
#' along latitudes
#' @param lat_end The upper (north) margin of the first (northern-most) gridcell
#' along latitudes
#' @param varnam A character string specifying the variable name in the data frame.
#' When not specified, regrids all numeric variables.
#'
#' @return A data frame
#' @export
#'
regrid_df <- function(
    df,
    res,
    lon_start,
    lon_end,
    lat_start,
    lat_end,
    varnam = NA
    ){

  lon_breaks <- seq(from = lon_start, to = lon_end, by = res)
  lat_breaks <- seq(from = lat_start, to = lat_end, by = res)

  df <- df |>
    ungroup() |>
    mutate(
      ilon = cut(lon, breaks = lon_breaks),
      ilat = cut(lat, breaks = lat_breaks)) |>

    mutate(
      lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
      lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
      lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
      lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) |>

    mutate(
      lon_mid = (lon_lower + lon_upper)/2,
      lat_mid = (lat_lower + lat_upper)/2) |>

    ## create cell name to associate with climate input
    dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper, -lon, -lat)


  if (identical(varnam, NA)){
    df_agg <- df |>
      group_by(lon_mid, lat_mid) |>
      summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") |>
      rename(lon = lon_mid, lat = lat_mid)
  } else {
    df_agg <- df |>
      group_by(lon_mid, lat_mid) |>
      summarise(!!varnam := mean(!!varnam, na.rm = TRUE), .groups = "drop") |>
      rename(lon = lon_mid, lat = lat_mid)
  }

  return(df_agg)

}
