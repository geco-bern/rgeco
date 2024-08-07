#' Plot a nice map with ggplot.
#'
#' Returns a cowplot object for a global map plot.
#'
#' @param obj An object, either a \code{SpatRaster} (returned from a \code{terra::rast()} function call),
#' or a list returned from a \code{rgeco::read_nc_onefile()} function call.
#' @param varnam A charachter string specifying the variable name. Optional and
#' used only if \code{obj} is a \code{SpatRaster} with multiple variables. If
#' \code{obj} is a rbeni-nc object (returned by \code{read_nc_onefile()}),
#' \code{varnam} must be provided (a character string specifying the variable
#' name in \code{obj$vars[[varnam]]}).
#' @param nbin An integer specifying the number of bins used for the color key.
#' @param maxval A numeric value specifying the maximum value for which the color key is to be extended. Defaults
#' to \code{NA} (the 99\% quantile of values is used).
#' @param breaks A numeric vector specifying the breaks for the color scale. Defaults to \code{NA}, i.e. breaks
#' are determined automatically based on \code{nbin} and \code{maxval}.
#' @param lonmin Left edge (longitude, in degrees), defaults to -180.
#' @param lonmax Right edge (longitude, in degrees), defaults to 180.
#' @param latmin Lower edge (latitude, in degrees), defaults to -90.
#' @param latmax Upper edge (latitude, in degrees), defaults to 90.
#' @param plot_title A character string specifying the plot title
#' @param plot_subtitle A character string specifying the plot subtitle
#' @param legend_title A character string specifying the legend title (annotation above the color key)
#' @param legend_direction Either \code{"vertical"} (default) or \code{"horizontal"}.
#' @param colorscale Either function that returns a set of colors or a vector of color names from which to interpolate.
#' Defaults to \code{virids::viridis}.
#' @param color_ocean A color specifyier for the fill color of the ocean layer. Defaults to \code{"azure"}.
#' @param invert One of 1 or -1, specifying the direction of the color scale. Defaults to -1.
#' @param do_reproj A boolean specifying whether to re-project the map to Robin projection
#' @param hillshade A logical specifying whether a hillshade layer should be added. Defaults to \code{FALSE}.
#' @param rivers A logical specifying whether to display rivers (the \code{ne_50m_rivers_lake_centerlines} layer from NaturalEarth.). Defaults to \code{FALSE}.
#' @param lakes A logical specifying whether to display rivers (the \code{ne_50m_lakes} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param coast A logical specifying whether to display coastlines (the \code{ne_50m_coastline} layer from NaturalEarth). Defaults to \code{TRUE}.
#' @param ocean A logical specifying whether to display the ocean layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param scale An integer specifying the resolutuion of NaturalEarth layers (coast, rivers, lakes). One of \code{110, 50, 10}. Defaults to \code{110} (coarsest resolution).
#' NaturalEarth layers for 110, 50, 10 m are used for low, medium, and high resolution (scale) layers, respectively. Defaults to \code{"small"}.
#' @param countries A logical specifying whether to display country borders (the \code{ne_50m_admin_0_countries} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param states A logical specifying whether to display sub-country administrative borders (e.g. US states) (the \code{ne_50m_admin_1_states_provinces} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param dir_ne A character string specifying where to download Naturalearth layers. Once downloaded, they can be quickly loaded. Defaults to \code{"~/data/naturalearth/"}.
#' @param make_discrete A logical scpecifying whether data layer is to be made discrete for plotting with colors
#' of discrete bins. Defaults to \code{TRUE}.
#' @param use_geom_raster A logical specifying whether to use the function \code{geom_raster()} for plotting the raster layer.
#' Defaults to \code{TRUE}. If \code{FALSE}, \code{geom_tile()} is used. The latter can yield nicer results when data is sparse.
#' @param is_boolean A logical specifying whether the raster contains boolean values (either \code{TRUE} or \code{FALSE}). Defaults to \code{FALSE}.
#' @param combine A boolean specifying whether the map and the colorscale should be combined using cowplot.
#' Defaults to \code{TRUE}. If \code{FALSE}, a list of elements are retruned, where elements are the ggplot2 plot object
#' and the coloscale object returned by the call to \code{plot_discrete_cbar}.
#' @param ... ...
#' @importFrom grDevices colorRampPalette
#' @importFrom methods as
#'
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map4 <- function(obj, varnam = NA, maxval = NA, breaks = NA, lonmin = -180, lonmax = 180, latmin = -90, latmax = 90,
                      nbin = 10, legend_title = waiver(), legend_direction = "vertical",
                      colorscale = viridis::viridis, color_ocean = "azure3", invert = -1, do_reproj = FALSE,
                      hillshade = FALSE, rivers = FALSE, lakes = FALSE, coast = TRUE, ocean = FALSE,
                      countries = FALSE, dir_ne = "~/data/naturalearth/",
                      states = FALSE, scale = 110, make_discrete = TRUE, use_geom_raster = TRUE,
                      is_boolean = FALSE,
											plot_title = waiver(), plot_subtitle = waiver(), combine = TRUE, ...){

	## define domain object
	domain <- c(xmin = lonmin, xmax = lonmax, ymin = latmin, ymax = latmax)

	# eps <- 0.0001
	# domain <- c(xmin = domain[["xmin"]] + eps,
	#             xmax = domain[["xmax"]] - eps,
	#             ymax = domain[["ymax"]] - eps,
	#             ymin = domain[["ymin"]] + eps)

	# create a bounding box for the robinson projection (following https://github.com/stineb/GECO_map/issues/1)
	eps <- 0.0
	bb <- sf::st_union(sf::st_make_grid(
	  sf::st_bbox(c(xmin = domain[["xmin"]] + eps,
	            xmax = domain[["xmax"]] - eps,
	            ymax = domain[["ymax"]] - eps,
	            ymin = domain[["ymin"]] + eps),
	          crs = sf::st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	          ),
	  n = 100)) |>
	  sf::st_union()

	## read 110 m resolution coastline from NaturalEarth data (is a shapefile)
	layer_coast <- rnaturalearth::ne_coastline(scale = scale, returnclass = "sf")
		# st_buffer(0) |>
	  # st_intersection(bb)

	## read 110 m resolution countries outline from NaturalEarth data (is a shapefile)
	layer_countries <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
		# st_buffer(0) |>
	  # st_intersection(bb)

	# download oceans
	if (ocean){
		if (length(list.files(path = dir_ne,
													pattern = paste0("ne_", as.character(scale), "m_ocean"))
													) == 0){
			layer_ocean <- rnaturalearth::ne_download(
			  scale = scale,
			  type = "ocean",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		} else {
			layer_ocean <- rnaturalearth::ne_load(
			  scale = scale,
			  type = "ocean",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		}

		## reduce to domain
		# layer_ocean <- st_crop(layer_ocean, domain)
		# layer_ocean <- layer_ocean |>
		# 	st_buffer(0) |>
		#   st_intersection(bb)
	}

	# download rivers
	if (rivers){
		if (length(list.files(path = dir_ne,
													pattern = paste0("ne_", as.character(scale), "m_rivers_lake_centerlines"))
													) == 0){
			layer_rivers <- rnaturalearth::ne_download(
			  scale = scale,
			  type = "rivers_lake_centerlines",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		} else {
			layer_rivers <- rnaturalearth::ne_load(
			  scale = scale,
			  type = "rivers_lake_centerlines",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		}

		## reduce to domain
		layer_rivers <- sf::st_crop(layer_rivers, domain)
		# layer_rivers <- layer_rivers |>
		# 	st_buffer(0) |>
		#   st_intersection(bb)

	}

	# download lakes
	if (lakes){
		if (length(list.files(path = dir_ne,
													pattern = paste0("ne_", as.character(scale), "m_lakes"))
													) == 0){
			layer_lakes <- rnaturalearth::ne_download(
			  scale = scale,
			  type = "lakes",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		} else {
			layer_lakes <- rnaturalearth::ne_load(
			  scale = scale,
			  type = "lakes",
			  category = "physical",
			  returnclass = "sf",
			  destdir = dir_ne
			  )
		}

		## reduce to domain
		layer_lakes <- try(sf::st_crop(layer_lakes, domain))
		# layer_lakes <- layer_lakes |>
		# 	st_buffer(0) |>
		#   st_intersection(bb)

	}

	# download hillshade
	if (hillshade){

		# if (length(list.files(path = dir_ne,
		# 											pattern = paste0("MSR_"))
		# 											) == 0){
		# 	scale_hillshade <- 50  # 110 doesn't seem to work
		# 	layer_hillshade <- rnaturalearth::ne_download(
		# 	  scale = scale_hillshade,
		# 	  type = paste0("MSR_", as.character(scale_hillshade), "M"),
		# 	  category = "raster",
		# 	  returnclass = "sf",
		# 	  destdir = dir_ne,
		# 	  load = FALSE
		# 	  )
		# 	raster_shade <- raster::stack(paste0(dir_ne, "MSR_", as.character(scale_hillshade), "M/MSR_", as.character(scale_hillshade), "M.tif"))
		# } else {
		# 	scale_hillshade <- 50  # 110 doesn't seem to work
		# 	raster_shade <- raster::stack(paste0(dir_ne, "MSR_", as.character(scale_hillshade), "M/MSR_", as.character(scale_hillshade), "M.tif"))
		# }

		# ## reduce to domain
		# raster_shade <- crop(raster_shade, y = extent(domain))

		## alternatively
		raster_shade <- terra::rast( raster::stack(paste0(dir_ne, "SR_50M/SR_50M.tif")))
		df_hs <- raster_shade |>
			terra::crop(bb) |>
		  as.data.frame(xy = TRUE) |>
		  as_tibble() |>
		  # rename(MSR_50M = SR_50M)
		  dplyr::rename(MSR_50M = layer)

	}


	##---------------------------------------------
	## interpret object
	##---------------------------------------------
	if (identical(class(obj), "character")){

		## read as raster brick
		rasta <- terra::rast(obj)

		##---------------------------------------------
		## re-project
		##---------------------------------------------
		# declare incoming CSR (should be done wayyyyyyy earlier than this)
		if (do_reproj){
		  crs(rasta) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
		  rasta_reproj <- terra::project(rasta, crs=CRS("+proj=robin"))
		} else {
		  rasta_reproj <- rasta
		}

		##---------------------------------------------
		## convert to data frame for ggplot
		##---------------------------------------------
		tstep <- 1
		df <- methods::as(rasta_reproj[[tstep]], "SpatialPixelsDataFrame")
		df <- as.data.frame(df)
		names(df) <- c("layer", "x", "y")


	} else if (identical(class(obj), "matrix")){

		## Complement info of matrix
    if (length(dim(obj))==2){

      if (dim(obj)[1] == 720 && dim(obj)[2]==360){
        grid <- "halfdeg"
      } else if (dim(obj)[1] == 360 && dim(obj)[2]==720){
        obj <- t(obj)
        grid <- "halfdeg"
      } else if (dim(obj)[1] == 360 && dim(obj)[2]==180){
        grid <- "1x1deg"
      } else if (dim(obj)[1] == 180 && dim(obj)[2]==360){
        obj <- t(obj)
        grid <- "1x1deg"
      }

      ## obj is a 2D matrix (grid)
      if (grid=="halfdeg"){
        lon <- seq(from = -179.75, to = 179.75, by = 0.5)
        lat <- seq(from = -89.75, to = 89.75, by = 0.5)
      } else if (grid=="1x1deg"){
        lon <- seq(from = -179.75, to = 179.75, by = 1)
        lat <- seq(from = -89.75, to = 89.75, by = 1)
      } else {
        warning("Dimensions not identified")
        lon <- seq(dim(obj)[1])
        lat <- seq(dim(obj)[2])
      }
      df <- grid_to_df(obj, grid = grid, dropna = FALSE) %>%
        setNames(c("x", "y", "layer"))

    } else {
      stop("Aborted. Argument obj is a matrix but does not have two dimensions.")
    }

  } else if (identical(class(obj)[[1]], "SpatRaster")){

    ## convert into data frame with longitude (x) and latitude (y)
    ## convert object into data frame
    if (identical(varnam, NA)){
      df <- as.data.frame(obj, xy = TRUE)
      names(df) <- c("x", "y", "layer")
    } else {
      df <- as.data.frame(obj, xy = TRUE) |>
        dplyr::select(x, y, layer = !!varnam)
    }

  } else if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){

    ## is a rbeni-nc element
    if (is.null(varnam)) stop("Error: provide the variable name to be plotted as argument varnam.")
    df <- nc_to_df(obj, varnam = varnam) %>%
      dplyr::rename(x=lon, y=lat, layer = !!varnam)

  } else if (is.data.frame(obj)){

    ## is already a data frame. thanks.
    df <- as_tibble(obj) %>%
      dplyr::filter(lon > lonmin & lon < lonmax & lat > latmin & lat < latmax) %>%
      dplyr::rename(x = lon, y = lat) |>
      dplyr::select(x, y, layer = !!varnam)
  }

	## of more than one variable is available, make varnam a required argument
	if (is.null(varnam)){
	  varnam <- names(df %>% dplyr::select(-x, -y))
	  if (length(varnam) > 1){
	    stop(paste("Aborting. Argument varnam not provided and more than one variable available. Which one to take?"))
	  }
	}

	if (is_boolean){
	  df <- df |>
	    mutate(layer = ifelse(layer == 1, TRUE,
	                          ifelse(layer == 0, FALSE, layer)))
	}

	##---------------------------------------------
	## Bin data
	##---------------------------------------------
	toptriangle <- FALSE
	bottomtriangle <- FALSE

	if (identical(NA, breaks)){
	  breaks <- scales::pretty_breaks(n = nbin)(df$layer)
	  warning("Overwriting nbin after defining breaks with scales::pretty_breaks().")
	  nbin <- length(breaks) - 1
	} else {
	  nbin <- length(breaks) - 1
	}

	breaks_with <- breaks

	if (is.infinite(breaks[length(breaks)])){
	  toptriangle <- TRUE
	  breaks <- breaks[-(length(breaks)-1)]
	}
	if (is.infinite(breaks[1])){
	  bottomtriangle <- TRUE
	  breaks <- breaks[-2]
	}

	## update
	nbin <- length(breaks) - 1

	## add dummy rows to make sure values in layer span the entire range
	if (!is_boolean){
	  df <- df %>%
	    bind_rows(
	      tibble(
	        x = NA,
	        y = NA,
	        layer = breaks[1:(length(breaks)-1)] + 0.5 * (breaks[2]-breaks[1])
	      )
	    )
	}

	## bin data
	if (!is_boolean){
	  if (make_discrete){
	    df$layercut <- as.factor(base::cut(df$layer, breaks=breaks, labels = FALSE, include.lowest = TRUE))
	  } else {
	    df$layercut <- df$layer
	  }
	}

	df <- df %>%
	  dplyr::filter(x > domain[1] & x < domain[2] & y > domain[3] & y < domain[4])

	# ## convert the hillshade layer to a data frame
	# if (hillshade){
	#   df_hs <- data.frame(
	#     xyFromCell(raster_shade, 1:ncell(raster_shade)),
	#     getValues(raster_shade/255)) %>%
	#     as_tibble()
	# }

	##---------------------------------------------
	## Create color scale
	##---------------------------------------------
	if (class(colorscale)=="function"){

	  colorscale <- colorscale(nbin, direction = invert)

	} else if (class(colorscale)=="character"){

	  if (colorscale %in% c("batlowK", "turku", "tokyo", "lapaz", "batlow",
	                        "batlowW", "oslo", "bamako", "navia", "lajolla",
	                        "lipari", "roma")){
	    colorscale <- scico::scico(nbin, palette = colorscale, direction = invert)
	  } else {
	    colorscale <- grDevices::colorRampPalette( colorscale )( nbin )
	  }

	} else if (class(colorscale)=="palette"){

	  ## nothing to do in this case
	  #colorscale <- colorscale

	} else {

	  stop("colorscale could not be set.")

	}

	if (toptriangle){
	  colorscale <- c(colorscale, colorscale[length(colorscale)])
	}
	if (bottomtriangle){
	  colorscale <- c(colorscale[1], colorscale)
	}

	##---------------------------------------------
	## Create ggplot object
	##---------------------------------------------
	if (is_boolean){

		ggmap <- ggplot() +
			geom_raster(data = df,
									aes(x = x, y = y, fill = layer, color = layer),
									show.legend = TRUE)
		# colorscale <- vhs("maxell_gu")[c(3,2)]
	  # vhs_palettes <- list( # https://github.com/cj-holmes/vhs/blob/10692a2de29bfcf1e57d62ce992940b0b07e1470/R/palettes.R#L32
	  #     #...
	  #     maxell_gu =c("#1e241eff", "#29a274ff", "#777055ff")
	  #     #...
	  #   )
		colorscale <- c("#777055ff", "#29a274ff")

	} else {

		if (use_geom_raster){
			ggmap <- ggplot() +

			  ## Note: geom_raster() is a fast special case of geom_tile() used when all the tiles are the same size.
			  geom_raster(data = df,
			              aes(x = x, y = y, fill = layercut, color = layercut),
			              show.legend = FALSE)
		} else {
			ggmap <- ggplot() +

				## Use geom_tile() to avoid that data sparsity emphasized.
			  geom_tile(data = df,
			              aes(x = x, y = y, fill = layercut, color = layercut),
			              show.legend = FALSE)
		}

	}

	ggmap <- ggmap +

	  scale_fill_manual(values = colorscale, na.value = "transparent", name = "") +
	  scale_color_manual(values = colorscale, na.value = "transparent", name = "") +

	  ## some layout modifications
	  xlab('') +
	  ylab('') +
	  theme_bw() +
	  theme(axis.ticks.y.right = element_line(),
	        axis.ticks.x.top = element_line(),
	        panel.grid = element_blank(),
	        panel.background = element_rect(fill = "grey50"),  # xxx for CWDX map
	        # panel.background = element_rect(fill = "white"),
	        plot.background = element_rect(fill = "white")
	        )

	## add coast layer
	if (coast){
	  ggmap <- ggmap +
		  geom_sf(data = layer_coast,
		          color = "grey10",
		          fill = NA,
		          size = 0.1)
	}

	## add rivers layer
	if (rivers){
	  ggmap <- ggmap +
		  geom_sf(data = layer_rivers,
		          color = "dodgerblue",
		          fill = NA,
		          size = 0.1)
	}

	## add lakes layer
	if (lakes){
	  if (class(layer_lakes) != "try-error"){
	    ggmap <- ggmap +
	      geom_sf(data = layer_lakes,
	              color = "grey10",
	              fill = NA,
	              size = 0.1)
	  } else {
	    warning("layer_lakes not succssfully cropped.")
	  }
	}

	## add country layer
	if (countries){
	  ggmap <- ggmap +
		  geom_sf(data = layer_countries,
		          color = "grey10",
		          fill = NA,
		          size = 0.1)
	}

	## add hillshade layer
	if (hillshade){
	  ggmap <- ggmap +
	    geom_raster(
	    	data = df_hs,
	    	aes(x = x, y = y, alpha = MSR_50M),
	    	show.legend = FALSE
	    	) +
	    scale_alpha(range=c(0.5, 0))
	}

	## add ocean
	if (ocean){
	  ggmap <- ggmap +
		  geom_sf(data = layer_ocean,
		          color = NA,
		          fill = color_ocean)
	}

  ## limit longitude and latitude extent
	ggmap <- ggmap  +
	  coord_sf(xlim = c(lonmin, lonmax),
	           ylim = c(latmin, latmax),
	           expand = FALSE   # to draw map strictly bounded by the specified extent
	           )

	if (!is_boolean){
		gglegend <- plot_discrete_cbar(
			breaks           = breaks_with, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
			colors           = colorscale,
			legend_title     = legend_title,
			legend_direction = legend_direction,
		  width = 0.03,
		  font_size = 3,  # of color key labels
			...
	    )

		if (combine){
		  if (legend_direction == "vertical"){
		    out <- cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.10))
		  } else {
		    out <- cowplot::plot_grid(ggmap, gglegend, ncol = 1, rel_heights = c(1, 0.10))
		  }
		} else {
		  out <- list(ggmap = ggmap, gglegend = gglegend)
		}

	} else {
		out <- list(ggmap = ggmap, gglegend = NA)
	}

  return(out)
}


## Copied from https://github.com/adrfantini/plot_discrete_cbar
plot_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", # RColorBrewer palette to use
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
    direction = 1, # Flip colors? Can be 1 or -1
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 3.5,
    expand_size = 0, # Controls spacing around legend plot
    expand_size_y = 0.5,
    spacing_scaling = 0.3, # Multiplicative factor for label and legend title spacing
    width = 0.01, # Thickness of color bar
    triangle_size = 0.05, # Relative width of +-Inf triangles
    color_text_legend = "black" # xxx set to "grey80" for SCWDX map
    ) {

    require(ggplot2)

    if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
    if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
    if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
    breaks = as.numeric(breaks)
    new_breaks = sort(unique(breaks))
    if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
    breaks = new_breaks
    if (class(colors) == "function") colors = colors(length(breaks) - 1)
    if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
    if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")

    if (direction == -1) colors = rev(colors)

    inf_breaks = which(is.infinite(breaks))
    if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
    plotcolors = colors

    n_breaks = length(breaks)

    labels = breaks

    if (spacing == "constant") {
        breaks = 1:n_breaks
    }

    r_breaks = range(breaks)
    d_breaks = breaks[2] - breaks[1]

    cbar_df = data.frame(stringsAsFactors = FALSE,
        y = breaks,
        yend = c(breaks[-1], NA),
        color = as.character(1:n_breaks)
    )[-n_breaks,]

    xmin = 1 - width/2
    xmax = 1 + width/2

    cbar_plot = ggplot(
      cbar_df,
      aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))
      ) +
      geom_rect(show.legend = FALSE, color=border_color)

    ## Add arrows
    if (any(inf_breaks == 1)) { # Add < arrow for -Inf
        firstv = breaks[1]
        polystart = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-1]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polystart, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[1],
                        color=border_color)
    }
    if (any(inf_breaks > 1)) { # Add > arrow for +Inf
        lastv = breaks[n_breaks]
        polyend = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-length(plotcolors)]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polyend, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[length(colors)],
                        color=border_color)
    }

    if (legend_direction == "horizontal") {
        #horizontal legend
        mul = 1
        x = xmin
        xend = xmax
        cbar_plot <- cbar_plot + coord_flip()
        angle = 0
        legend_position = xmax + 0.1 * spacing_scaling

    } else {
        # vertical legend
        mul = -1
        x = xmax
        xend = xmin
        angle = -90
        legend_position = xmin # xmax + 0.2 * spacing_scaling
    }

    ymid <- (breaks[length(breaks)] + breaks[1]) / 2
    dy <- breaks[length(breaks)] - breaks[1]
    ybottom_abs <- ymid - dy/2 * 1/expand_size_y
    ytop_abs    <- ymid + dy/2 * 1/expand_size_y

    # Create color key
    cbar_plot <- cbar_plot +
        geom_segment(data = data.frame(y = breaks, yend = breaks),
            aes(y=y, yend=yend),
            x = x - 0.01 * mul * spacing_scaling, xend = x, #+ 0.01 * mul * spacing_scaling, # xend = xend,
            inherit.aes = FALSE,
            color = color_text_legend
            ) +
        annotate(geom = 'text', x = x - 0.02 * mul * spacing_scaling, y = breaks,
                label = labels,
                size = font_size,
                hjust = 0,
                color = color_text_legend
                ) +
        # scale_x_continuous(expand = c(expand_size,expand_size)) +
        scale_fill_manual(values=plotcolors) +
        theme_void() +
        # theme(plot.background = element_rect(fill = "white")) +  # xxx remove for SCWDX map
        expand_limits(y = c(ybottom_abs, ytop_abs),
                      x = c(xend, x - 0.1 * mul * spacing_scaling)) +
        theme(plot.background = element_rect(fill = "white", color = NA))

    # Add legend title
    if (!is.null(legend_title)) {
        cbar_plot <- cbar_plot +
            annotate(
              geom = 'text',
              x = legend_position,
              # y = mean(r_breaks),
              y = max(r_breaks) + d_breaks * 1.5,
              label = legend_title,
              # angle = angle,
              angle = 0,
              size = font_size,
              fontface = 1,
              hjust = 0,
              color = color_text_legend
              )
    }

    return(cbar_plot)
}

mycrop <- function(x, domain){

  # domain should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)

  fortify(x, region="id") %>%
    as_tibble() %>%
    dplyr::left_join(x@data, by = "id") %>%
    dplyr::filter(long > domain[1] & long < domain[2] &
                    lat > domain[3] & lat < domain[4])
}
