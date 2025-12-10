map_by_fips_tidy <- function(
    data.to.map,
    state.border.col = "black",
    state.border.width = 0.75,
    county.border.col = "black",
    county.border.width = 0.5,
    state.to.plot = ".",
    continental.us = TRUE,
    missing.include = FALSE,
    color.break.type = "quantiles",
    color.break.values = seq(0, 1, by = 0.1),
    color.break.digits = 0,
    color.sequence = c("blue", "green", "yellow", "red"),
    na_color = "white",
    legend.title = "Value",
    mask.fips = NA,
    mask.color = "black",
    bivariate = FALSE,
    pal = "DkViolet",
    flip_axes = FALSE,
    rotate_pal = FALSE,
    dim = 3,
    bi_break_values = NA,
    xlab = "X Axis",
    ylab = "Y Axis",
    proj.info = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=km +no_defs",
    log_scale = FALSE,
    discrete_scale = FALSE
) {
  
  library(ggplot2)
  library(sf)
  library(dplyr)
  
  # ------------------------------------------------------------------------
  # Load shapefiles
  # ------------------------------------------------------------------------
  load("./data/MAP_county_census2016_5m.RData")
  load("./data/MAP_state_census2016_5m.RData")
  
  county_sf <- st_as_sf(county_boundary_2016_5m)
  state_sf  <- st_as_sf(state_boundary_2016_5m)
  
  county_sf <- st_transform(county_sf, crs = proj.info)
  state_sf  <- st_transform(state_sf,  crs = proj.info)
  
  # Filter continental US
  if (continental.us) {
    county_sf <- county_sf[county_sf$CONTINENTAL, ]
    state_sf  <- state_sf[state_sf$CONTINENTAL, ]
  }
  
  # Filter selected states
  if (!"." %in% state.to.plot) {
    county_sf <- county_sf[county_sf$STATE %in% state.to.plot, ]
    state_sf  <- state_sf[state_sf$NAME %in% state.to.plot, ]
  }
  
  # ------------------------------------------------------------------------
  # CASE 1: data.to.map has FIPS + geometry + facility columns
  # ------------------------------------------------------------------------
  if (ncol(data.to.map) >= 4) {
    
    colnames(data.to.map)[1:4] <- c("FIPS","slat","slon","metric")
    
    data.to.map$FIPS <- sprintf("%05d", as.numeric(as.character(data.to.map$FIPS)))
    fill_fips <- data.to.map %>% filter(!is.na(FIPS)) %>% distinct(FIPS) %>% mutate(val = 1)
    
    county_sf <- county_sf %>% left_join(fill_fips, by = c("GEOID"="FIPS"))
    
    # Base map
    p <- ggplot(county_sf) +
      geom_sf(aes(fill = factor(val)), color = county.border.col, size = county.border.width) +
      geom_sf(data = state_sf, color = state.border.col, size = state.border.width, fill = NA) +
      scale_fill_manual(values = color.sequence[1], na.value = na_color) +
      theme_void() + guides(fill="none")
    
    # Add facilities
    facs <- data.to.map %>% filter(!is.na(slat), !is.na(slon), !is.na(metric))
    if (nrow(facs) > 0) {
      facs_sf <- st_as_sf(facs, coords = c("slon","slat"), crs = 4326) |> st_transform(crs = proj.info)
      p <- p + geom_sf(data = facs_sf, aes(size = metric), fill = "yellow3", color="black",
                       alpha=.7, shape=21) +
        scale_size_continuous(range=c(2,12), name=legend.title)
    }
    
    return(p)
  }
  
  # ------------------------------------------------------------------------
  # CASE 2: Univariate OR bivariate mapping
  # ------------------------------------------------------------------------
  colnames(data.to.map) <- c("FIPS","val")
  data.to.map$FIPS <- sprintf("%05d", as.numeric(as.character(data.to.map$FIPS)))
  
  # Join onto shapefile
  county_sf <- county_sf %>% left_join(data.to.map, by = c("GEOID"="FIPS"))
  
  # Masking
  if (!is.null(mask.fips) && length(mask.fips)>0) {
    county_sf$val[county_sf$GEOID %in% mask.fips] <- NA
  }
  
  # ------------------------------------------------------------------------
  # BIVARIATE MAPPING
  # ------------------------------------------------------------------------
  if (bivariate) {
    
    county_sf <- county_sf %>% mutate(bi_class = as.character(val))
    if (!("bi_class" %in% colnames(county_sf))) stop("bivariate=TRUE requires bi_class column")
    
    p <- ggplot() +
      geom_sf(data=county_sf, aes(fill=bi_class),
              color=county.border.col, size=county.border.width, show.legend=FALSE) +
      bi_scale_fill(pal=pal, dim=dim, flip_axes=flip_axes, rotate_pal=rotate_pal,
                    na.value=na_color) +
      geom_sf(data=state_sf, color=state.border.col, size=state.border.width, fill=NA) +
      theme_void()
    
    legend <- bi_legend(
      pal=pal, dim=dim, xlab=xlab, ylab=ylab,
      size=14, flip_axes=flip_axes, rotate_pal=rotate_pal, arrows=FALSE,
      breaks=bi_break_values
    )
    
    return(
      cowplot::ggdraw() +
        cowplot::draw_plot(p,0,0,1,1) +
        cowplot::draw_plot(legend,0,0,0.30,0.30)
    )
  }
  
  # ------------------------------------------------------------------------
  # UNIVARIATE MAPPING
  # ------------------------------------------------------------------------
  
  val_orig <- county_sf$val
  county_sf$val_plot <- val_orig
  
  if (log_scale) {
    county_sf$val_plot <- ifelse(val_orig > 0, log10(val_orig), NA)
  }
  
  # breaks in original scale
  if (color.break.type=="quantiles") {
    breaks_orig <- quantile(val_orig, probs=color.break.values, na.rm=TRUE)
  } else if (color.break.type=="values") {
    breaks_orig <- color.break.values
  } else {
    breaks_orig <- seq(color.break.values[2], color.break.values[3],
                       length.out=color.break.values[1])
  }
  breaks_orig <- unique(round(breaks_orig, color.break.digits))
  
  # Breaks in mapped scale
  breaks_mapped <- if (log_scale) log10(breaks_orig) else breaks_orig
  
  col_palette <- colorRampPalette(color.sequence)(length(breaks_orig)-1)
  
  p <- ggplot(county_sf) +
    geom_sf(aes(fill = val_plot),
            color=county.border.col, size=county.border.width) +
    geom_sf(data=state_sf, color=state.border.col, size=state.border.width, fill=NA) +
    scale_fill_gradientn(
      colors = col_palette,
      na.value = na_color,
      values = scales::rescale(breaks_mapped),
      breaks = breaks_mapped,
      labels = breaks_orig,
      limits = range(breaks_mapped, na.rm=TRUE),
      name = legend.title,
      guide = guide_colorbar()
    ) +
    theme_void() +
    theme(
      legend.position="right",
      legend.text = element_text(size=12)
    )
  
  return(p)
}
