#' Identify events
#'
#' Identifies events as periods where of consecutively TRUE values in a boolean
#' vector.
#'
#' @param vec A vector of boolean values. Consecutive TRUE vakues designate an event.
#' @param merge_threshold An integer value specifying the threshold of the gap
#' length below in units of time steps which
#' gaps between events are ignored and the two events on either side of the gap
#' are merged into a single events. Defaults to NA (ignored). Is ignored if
#' \code{do_merge=FALSE}
#' @param leng_threshold An integer specifying the minimum length required for
#' creating an event. Defaults to 3.
#' @param do_merge A logical specifying whether to merge events if the gap between
#' them is small (smaller than \code{merge_threshold}).
#'
#' @return A data frame containing information about the start date and length
#' of each detected event
#' @export
#'
get_consecutive <- function(
    vec,
    merge_threshold = NA,
    leng_threshold = 3,
    do_merge = FALSE
){

  ## replace NAs with FALSE (no drought). This is needed because of NAs at head or tail
  vec[ which(is.na(vec)) ] <- FALSE

  ## identifies periods where 'vec' true for consecutive days of length>leng_threshold and
  ## creates data frame holding each instance's info: start of drought by index
  ## in 'vec' and length (number of days thereafter)
  instances <- data.frame( idx_start=c(), len=c() )
  consecutive_vec <- rep( NA, length( vec ) )
  nvec  <- 0
  ninst <- 0
  for ( idx in 1:length( vec ) ){
    if (vec[idx]){
      nvec <- nvec + 1
    } else {
      if (nvec >= leng_threshold) {
        ## create instance
        ninst <- ninst + 1
        addrow <- data.frame( idx_start = idx-(nvec), len = nvec )
        instances <- rbind( instances, addrow )
      }
      nvec <- 0
    }
    consecutive_vec[idx] <- nvec
  }
  if (nvec > leng_threshold){
    ## create a last instance if the last vec period extends to the end of the time series
    ninst <- ninst + 1
    addrow <- data.frame( idx_start=idx-(nvec), len=nvec )
    instances <- rbind( instances, addrow )
  }

  # get info about gap between events
  instances <- instances |>
    mutate(gap_before = idx_start - (lag(idx_start) + lag(len)))

  if (nrow(instances) > 0){
    if (do_merge && nrow(instances) > 1){

      instances_merged <- instances[1,]
      idx_merged <- 1
      for (idx in 2:nrow(instances)){
        if (instances$gap_before[idx] > merge_threshold){

          # create new sequence
          idx_merged <- idx_merged + 1
          instances_merged <- bind_rows(
            instances_merged,
            instances[idx,]
          )

          # edit length of previously recorded instance
          instances_merged$len[idx_merged - 1] <- instances$idx_start[idx - 1] + instances$len[idx - 1] - instances_merged$idx_start[idx_merged - 1]
        }
      }

      # if all is merged until the end
      instances_merged$len[idx_merged] <- instances$idx_start[idx] + instances$len[idx] - instances_merged$idx_start[idx_merged]

      instances <- instances_merged[,c("idx_start", "len")]
    } else {
      instances <- instances[,c("idx_start", "len")]
      if (nrow(instances) == 1){
        if (instances$idx_start == 0)
          instances$idx_start <- 1
      }
    }

  }

  return( instances )
}
