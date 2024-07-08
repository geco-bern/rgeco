#' Aligns data by events
#'
#' Uses a vectory specifying whether data falls into an event to reshape data, aligning by the onset of the event
#'
#' @param df A data frame containing all data continuously along time, required columns: \code{"site", "date"}.
#' @param events A data frame with columns \code{idx_start} and \code{len}, specifying event start and length, referring to the row index of \code{df}.
#' \code{events} is the output of a function call to \code{get_consecutive}.
#' @param dovars A vector of character strings specifying which columns (by column name) of \code{df} to re-arrange.
#' @param leng_threshold An integer specifying the minum number of consecutive dates required to define an event.
#' All events of length lower than \code{leng_threshold} are dropped.
#' @param before An integer specifying the number of days before the event onset to be retained in re-arranged data
#' @param after An integer specifying the number of days after the event onset to be retained in re-arranged data
#' @param do_norm A logical specifying whether re-arranged data is to be normalised by the median value of the bin
#' (number of bins given by argument \code{nbins}) before the event onset, given by argument \code{normbin}. Defaults to \code{FALSE}.
#' @param nbins An integer, specifying the number of bins used to determine median values before event onset. Only used when \code{do_norm=TRUE}. Defaults to 6.
#' @param normbin An integer, specifying the bin number just before the event onset, used for normalisation. Only used when \code{do_norm=TRUE}. Defaults to 2.
#'
#' @return A named list of data frames (\code{list( "df_idx_event", "df_idx_event_aggbyidx_event")}) containing data from all events and \code{before + after}
#' dates (relative to event onset) with additional columns named \code{"inst"}, defining the event number (instance), and \code{"idx_event"}, defining
#' the date relative to the respective event onset. The data frame \code{"df_idx_event"} contains rearranged, but otherwise unchanged data (unless
#' \code{do_norm}=TRUE). The data frame \code{"df_idx_event_aggbyidx_event"} containes data aggregated across events with the mean and quantiles given for each
#' \code{"idx_event"}.
#' @import dplyr
#' @import ggplot2
#' @importFrom stats setNames
#' @export
#'
align_events <- function(
    df,
    events,
    dovars = names(df),
    leng_threshold,
    before,
    after,
    do_norm=FALSE,
    nbins=6,
    normbin=2
    ){
  ##--------------------------------------------------------
  ## Re-arrange data, aligning by beginning of events
  ## Creates data frame where not all rows are retained from df
  ## and columns added for 'idx_event' (number of day relative to onset of event)
  ## and 'iinst' number of event to which row belongs.
  ##--------------------------------------------------------
  if (nrow(events) > 1){

    df_idx_event <- tibble()
    for ( iinst in 1:nrow(events) ){
      # idx_event = 0 is beginning of event
      idx_event <- seq( from = -before,
                   to = events$len[iinst],
                   by = 1
                   )
      idxs <- idx_event + events$idx_start[iinst]

      # avoid negative row indexes (possible consequence of using 'before > 0')
      drophead <- which( idxs < 1 )
      if (length(drophead) > 0){
        idxs <- idxs[ -drophead ]
        idx_event <- idx_event[ -drophead ]
      }
      addrows <- df |>
        slice( idxs ) |>
        mutate( idx_event = idx_event,
                inst = iinst
                )
      df_idx_event <- df_idx_event |>
        bind_rows( addrows )
    }

    ##--------------------------------------------------------
    ## Normalise re-arranged data relative to a certain bin's median
    ##--------------------------------------------------------
    if (do_norm){
      ## Bins for different variables
      bins  <- seq(
        from = -before,
        to = after,
        by = (after + before + 1)/nbins )

      ## add bin information based on idx_event to expanded df
      df_idx_event <- df_idx_event |>
        mutate(
          inbin  = cut( as.numeric(idx_event), breaks = bins )
          )

      tmp <- df_idx_event |>
        dplyr::filter(!is.na(inbin)) |>
        group_by( inbin ) |>
        summarise_at( vars(one_of(dovars)), funs(stats::median( ., na.rm=TRUE )) )

      norm <- slice(tmp, normbin)

      ## subtract from all values
      df_idx_event <- df_idx_event |> mutate_at( vars(one_of(dovars)), funs(. - norm$.) )

    }

  } else {

    df_idx_event <- NULL

  }

  return( df_idx_event )

}

q33 <- function( vec, ... ){
  stats::quantile( vec, 0.33, ...)
}

q66 <- function( vec, ... ){
  stats::quantile( vec, 0.66, ...)
}



