#' Analyse modelled values versus observed data.
#'
#' Calculates a set of performance statistics and optionally creates plots of modelled
#' versus observed values.
#'
#' @param df A data frame containing columns with names corresponding to arguments
#' \code{mod} and \code{obs}
#' @param mod A character string specifying the variable name (column) of the
#' modelled (simulated) values in data frame \code{df}.
#' @param obs A character string specifying the variable name (column) of the
#' observed values in data frame \code{df}.
#' @param type If \code{"points"}, uses \code{geom_points()}, if \code{"hex"}
#' uses \code{ggplot2::geom_hex()}, if \code{"heat"} uses adjusted
#' \code{geom_points()} with color indicating density, if \code{"density"} uses
#' \code{stat_density_2d()} to draw polygons of equal density.
#' @param relative A logical specifying whether the relative RMSE and bias (after
#' division by the mean) is to be showed in the subtitle labels.
#' @param shortsubtitle A logical specifying whether to display a reduced set of metrics
#' in the subtitle.
#' @param plot_subtitle A logical specifying whether to display any metrics. Defaults
#' to \code{TRUE}.
#' @param plot_linmod A logical specifying whether to display the fitted linear
#' regression as a red line. Defaults to \code{TRUE}.
#' @param nlabels An integer specifying how many points to be labelled, starting with points
#' that have the largest residuals from the linear regression fit. Only available
#' for \code{type == "points"}. Defaults to one.
#' @param pal A character string indicating the color palette. Currently available:
#' \code{"batlowW", "davos", "magma", "viridis", "cividis"}. Defaults to \code{"viridis"}.
#'
#' @export
#'
analyse_modobs2 <- function(
  df,
  mod,
  obs,
  type = "points",
  relative = FALSE,
  shortsubtitle = FALSE,
  plot_subtitle = TRUE,
  plot_linmod = TRUE,
  pal = "viridis",
  ...) {


  ## rename to 'mod' and 'obs' and remove rows with NA in mod or obs
  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(mod = mod, obs = obs) %>%
    tidyr::drop_na(mod, obs)

  ## get linear regression (coefficients)
  linmod <- lm(obs ~ mod, data = df)

  ## construct metrics table using the 'yardstick' library
  df_metrics <- df %>%
    yardstick::metrics(obs, mod) %>%
    dplyr::bind_rows(tibble(.metric = "n", .estimator = "standard", .estimate = summarise(df, numb = n()) %>% unlist())) %>%
    dplyr::bind_rows(tibble(.metric = "slope", .estimator = "standard", .estimate = coef(linmod)[2])) %>%
    # dplyr::bind_rows( tibble( .metric = "nse",      .estimator = "standard", .estimate = hydroGOF::NSE( obs, mod, na.rm=TRUE ) ) ) %>%
    dplyr::bind_rows(tibble(.metric = "mean_obs", .estimator = "standard", .estimate = summarise(df, mean = mean(obs, na.rm = TRUE)) %>% unlist())) %>%
    dplyr::bind_rows(tibble(
      .metric = "prmse", .estimator = "standard",
      .estimate = dplyr::filter(., .metric == "rmse") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric == "mean_obs") %>%
        dplyr::select(.estimate) %>%
        unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "pmae", .estimator = "standard",
      .estimate = dplyr::filter(., .metric == "mae") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric == "mean_obs") %>%
        dplyr::select(.estimate) %>%
        unlist()
    )) %>%
    dplyr::bind_rows(tibble(.metric = "bias", .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod - obs), na.rm = TRUE)) %>% unlist())) %>%
    dplyr::bind_rows(tibble(.metric = "pbias", .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod - obs) / obs, na.rm = TRUE)) %>% unlist()))

  rsq_val <- df_metrics %>%
    dplyr::filter(.metric == "rsq") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  rmse_val <- df_metrics %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  mae_val <- df_metrics %>%
    dplyr::filter(.metric == "mae") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  bias_val <- df_metrics %>%
    dplyr::filter(.metric == "bias") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  slope_val <- df_metrics %>%
    dplyr::filter(.metric == "slope") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  n_val <- df_metrics %>%
    dplyr::filter(.metric == "n") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()

  if (relative) {
    rmse_val <- rmse_val / mean(df$obs, na.rm = TRUE)
    bias_val <- bias_val / mean(df$obs, na.rm = TRUE)
  }

  rsq_lab <- format(rsq_val, digits = 2)
  rmse_lab <- format(rmse_val, digits = 3)
  mae_lab <- format(mae_val, digits = 3)
  bias_lab <- format(bias_val, digits = 3)
  slope_lab <- format(slope_val, digits = 3)
  n_lab <- format(n_val, digits = 3)

  results <- tibble(rsq = rsq_val, rmse = rmse_val, mae = mae_val, bias = bias_val, slope = slope_val, n = n_val)

  if (shortsubtitle) {
    subtitle <- bquote(italic(R)^2 == .(rsq_lab) ~ ~
                         RMSE == .(rmse_lab))
  } else {
    subtitle <- bquote(italic(R)^2 == .(rsq_lab) ~ ~
                         RMSE == .(rmse_lab) ~ ~
                         bias == .(bias_lab) ~ ~
                         slope == .(slope_lab) ~ ~
                         italic(N) == .(n_lab))
  }

  lims <- round(max(quantile(df$mod, 0.9999), quantile(df$obs, 0.9999)))

  if (type == "heat") {

    gg <- heatscatter(
      df$mod,
      df$obs,
      xlim = xlim,
      ylim = ylim,
      main = "",
      ggplot = TRUE
    )

    gg <- gg +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_linmod) gg <- gg + geom_smooth(method = "lm", color = "red", size = 0.5, se = FALSE)
    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)

  } else if (type == "hex") {

    ## ggplot hexbin
    gg <- df %>%
      ggplot2::ggplot(aes(x = mod, y = obs)) +
      geom_hex(bins = 50, show.legend = FALSE) +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      # geom_hline(yintercept = 0, linetype = "dotted") +
      # geom_vline(xintercept = 0, linetype = "dotted") +
      coord_fixed() +
      xlim(0, lims) +
      ylim(0, lims) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (pal == "batlowW"){
      gg <- gg + khroma::scale_fill_batlowW(trans = "log", reverse = TRUE)
    } else if (pal == "davos"){
      gg <- gg + khroma::scale_fill_davos(trans = "log", reverse = TRUE)
    } else {
      gg <- gg + scale_fill_viridis_c(option = pal, trans = "log10", direction = -1)
    }

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method = "lm", color = "red", size = 0.5, se = FALSE)

  } else if (type == "points") {

    ## points
    gg <- df %>%
      ggplot(aes(x = mod, y = obs)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      # coord_fixed() +
      xlim(0, lims) +
      ylim(0, lims) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method = "lm", color = "red", size = 0.5, se = FALSE)

  } else if (type == "density") {

    ## points
    gg <- df %>%
      ggplot(aes(x = mod, y = obs)) +
      stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
      scale_fill_gradientn(
        colours = colorRampPalette(c("gray65", "navy", "red", "yellow"))(5),
        guide = "legend"
      ) +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      coord_fixed() +
      xlim(0, lims) +
      ylim(0, lims) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method = "lm", color = "red", size = 0.5, se = FALSE)

  }

  return(list(df_metrics = df_metrics, gg = gg, linmod = linmod, results = results))
}
