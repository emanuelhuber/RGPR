##------------- COLOR FUNCTIONS -------------------##
#' Color palettes for GPR data
#' @param pal [\code{character(1)}] Name of the color palette.
#' @param n [\code{integer(1)}] Number of colors to be in the palette.
#' @param power [\code{integer(1)}] Control parameter determining how chroma 
#' should be increased (1 = linear, 2 = quadratic, etc.).
#' @param returnNames [\code{logical(1)}] If \code{TRUE}, returns only the
#'                                        color palette names. 
#' @name palGPR
#' @rdname palGPR
#' @export
palGPR <- function(pal = "default", n = 101, power = 1, returnNames = FALSE){
  pal <- gsub("gray", "grey", x= pal)
  tmp <- structure(list(
    grey3 = colorspace::diverge_hcl(n, h = c(300, 1), c = 1, 
                                    l = c(1, 100), power=power), 
    #  too dark  
    grey2 = colorspace::sequential_hcl(n, h = c(300, 100), c = 0, 
                                       l = c(120, 10), power=power), 
    # too light
    grey1 = colorspace::sequential_hcl(n, h = c(1, 300), c = 0, 
                                       l = c(10, 100), power=power), 
    grey = colorspace::sequential_hcl(n, h = c(190, 1), c = 10, 
                                      l = c(1, 110), power=power),
    rainbow_hcl = colorspace::rainbow_hcl(n,c=100,l=60),
    rainbow = grDevices::colorRampPalette(rainbow(13),interpolate ="spline")(n),
    jet2 = grDevices::colorRampPalette(rev(c("blue", "#007FFF", "cyan",
                                         "#7FFF7F", "yellow", "#FF7F00", "red")))(n),
    jet = grDevices::colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan",
                                        "#7FFF7F", "yellow", "#FF7F00", "red", 
                                        "#7F0000")))(n),
    slice = colorRampPalette(rev(c("gray100", "gray60", "grey20", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red")))(n),
    # blue - white - red (fade)
    hcl_8 = colorspace::diverge_hcl(n, h = c(250, 10), c = 150, 
                                    l = c(30, 90), power=power),
    # blue - white - red (tern)
    hcl_7 = colorspace::diverge_hcl(n, h = c(220, 10), c = 100, 
                                    l = c(20, 90), power=power), 
    #  blue - white - red (vivid)
    hcl_6 = colorspace::diverge_hcl(n, h = c(246, 10), c = 120, 
                                    l = c(30, 90), power=power), 
    hcl_5 = colorspace::diverge_hcl(n, h = c(20, 200), c = 90, 
                                    l = c(70, 95), power=power),  
    # blue/violet - white - red/violet 
    hcl_4 = colorspace::diverge_hcl(n, h = c(255, 330), 
                                    l = c(40, 90), power=power), 
    #  green - white- orange (fade)
    hcl_3 = colorspace::diverge_hcl(n, h = c(130, 43), c = 100, 
                                    l = c(70, 90), power=power), 
    # blue - white - orange (fade)
    hcl_2 = colorspace::diverge_hcl(n, h = c(246, 40), c = 96, 
                                    l = c(65, 90), power=power),
    # blue - white - red (fade)
    hcl_1 = colorspace::diverge_hcl(n, c = 100, l = c(50, 90), power = power), 
    # rose - white - turquise (fade)
    hcl_0 = colorspace::diverge_hcl(n,power=1),
    nice = grDevices::colorRampPalette(c("#4e4286", "#3288BD", "#66C2A5", 
                                         "#ABDDA4",  "#FFFFFF",   "#FDAE61", 
                                         "#F46D43",  "#D53E4F",  "#770132"), space="Lab")(n),
    sunny = grDevices::colorRampPalette(c("#2b3d7b", "#83B8D7", "#EAEBCC", 
                                          "#FA9958", "#7c001d"), space="Lab")(n),
    # default = grDevices::colorRampPalette(c("#1C007C", "#1B0086", "#1A0091", 
    #                                         "#18009C",
    #                                         "#1600A7", "#1400B2", "#1100C3", "#0E00CF", "#0A00E0",
    #                                         "#0300F5", "#0001FF", "#080FFF", "#1521FF", "#2232FF",
    #                                         "#2E42FF", "#3B52FF", "#4862FF", "#5470FF", "#617FFF",
    #                                         "#6E8CFF", "#7F9EFF", "#8CAAFF", "#98B5FF", "#A5C1FF",
    #                                         "#B2CBFF", "#BFD5FF", "#CBDFFF", "#D8E7FF", "#E5F0FF",
    #                                         "#F2F7FF", "#FFFCFB", "#FFF4F0", "#FFECE5", "#FFE3DA",
    #                                         "#FFDACE", "#FFCEC0", "#FFC4B5", "#FFB9AA", "#FFAE9E",
    #                                         "#FF9F90", "#FF9485", "#FF877A", "#FF766B", "#FF6960",
    #                                         "#FF5B55", "#FF4946", "#FF3B4E", "#FF3045", "#FF253D",
    #                                         "#FF1632", "#FF0B2A", "#FF0022", "#F70023", "#EE0023",
    #                                         "#E50023", "#DC0024", "#D30024", "#CA0024", "#C20024",
    #                                         "#B70023", "#AF0023", "#A70023", "#9C0022"))(n)
    default = grDevices::colorRampPalette(c("#1C007C", "#1B0086", "#1A0091", "#18009C",
                                            "#1600A7", "#1400B2", "#1100C3", "#0E00CF", "#0A00E0",
                                            "#0300F5", "#0001FF", "#080FFF", "#1521FF", "#2232FF",
                                            "#2E42FF", "#3B52FF", "#4862FF", "#5470FF", "#617FFF",
                                            "#6E8CFF", "#7F9EFF", "#8CAAFF", "#98B5FF", "#A5C1FF",
                                            "#B2CBFF", "#BFD5FF", "#CBDFFF", "#D8E7FF", "#E5F0FF",
                                                        "#FFFFFF",                      
                                            "#FFDACE", "#FFCEC0", "#FFC4B5", "#FFB9AA", "#FFAE9E",
                                            "#FF9F90", "#FF9485", "#FF877A", "#FF766B", "#FF6960",
                                            "#FF5B55", "#FF4946", "#FF3B4E", "#FF3045", "#FF253D",
                                            "#FF1632", "#FF0B2A", "#FF0022", "#F70023", "#EE0023",
                                            "#E50023", "#DC0024", "#D30024", "#CA0024", "#C20024",
                                            "#B70023", "#AF0023", "#A70023", "#9C0022"))(n)
  ))
  if(returnNames){
    return( names(tmp) )
  }
  rev(tmp[[match(pal, names(tmp))]])
}

#' Plot single colour palette
#' 
#' source: vignette of the R-package "colorspace" (Color Space Manipulation) 
#' @param col [\code{character}] Colors to be plotted.
#' @param border [\code{character}] color for rectangle border(s). 
#' The default means par("fg"). Use border = NA to omit borders. 
#' If there are shading lines, border = TRUE means use the same colour 
#' for the border as for the shading lines.
#' @examples
#' palPlot(palGPR("hcl_5"))
#' @name palPlot
#' @rdname palGPR
#' @export
palPlot <- function(col, border = NA){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, 
       xlab = "",  ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = (col), border = border)
}

#' Colour palette
#'
#' @examples
#' palDisplay()
#' @name palDisplay
#' @rdname palGPR
#' @export
palDisplay <- function(){
  op <- par(no.readonly=TRUE)
  par(mai=c(1,1,1,0), oma = c(0,0,1,0))
  pNames <- palGPR(returnNames=TRUE)
  n <- 101
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, length(pNames)), 
       axes = FALSE, xlab = "", ylab = "")
  for(i in seq_along(pNames)){
    myPal  <- palGPR(pal = pNames[i], n = n)
    rect(0:(n-1)/n, i-1/3, 1:n/n, i + 1/3, col = (myPal), border = NA)
    mtext(pNames[i], side=2, at=i, adj = 1, las = 1)
  }
  title("Colour palettes from RGPR (palGPR)")
  par(op)
}

#' Return color from palette
#' @param x Values
#' @param col [\code{character}] Colors to be used.
#' @export
palCol <- function(x , col = palGPR(n=101)){
  CCY = (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  ClimY <- range(CCY, na.rm = TRUE)
  ClenY <- ClimY[2] - ClimY[1] + 1
  return(col[ (CCY)*(length(col) - 1) + 1 ] )
}
#--------------------------------#

