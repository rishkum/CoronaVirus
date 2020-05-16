#Color Plot

# Dectech corporate colors
dectech_colors <- c(
  `cool grey`        = "#545F66",
  `dectech green`    = "#377460",
  `light grey`      = "#DCDCDD",
  `dectech navy`     = "#003D59",
  `purple`           = "#52489C",
  `light green`      = "#78C091",
  `yellow`           = "#F4E04D")


#' Function to extract dectech colors as hex codes
#'
#' @param ... Character names of dectech_colors 
#'
dectech_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (dectech_colors)
  
  dectech_colors[cols]
}



library(ggplot2)

#Develop a pallete for future use
dectech_palettes <- list(
  `main`  = dectech_cols("cool grey", "dectech green", "cool grey"),
  
  `mixed` = dectech_cols("dectech navy", "purple",  "yellow", "dectech green"),
  
  `grey`  = dectech_cols("light grey", "cool grey")
)


#' Return function to interpolate a dectech color palette
#'
#' @param palette Character name of palette in dectech_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
dectech_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- dectech_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_dectech <- function(palette = "mixed", discrete = TRUE, reverse = FALSE, ...) {
  pal <- dectech_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("dectech_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_dectech <- function(palette = "mixed", discrete = TRUE, reverse = FALSE, ...) {
  pal <- dectech_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("dectech_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

