# Global constants and functions

# Figure specification --------------------------------------------

# fonts
library(showtext)
font_add_google('Roboto', 'roboto')
font_add_google('Roboto Condensed', 'robotocondensed')
showtext_auto()

# ggplot theme
# ggplot theme by Jonas Schöley
MyGGplotTheme <-
  function (
    size = 8,
    family = 'roboto',
    scaler = 1,
    axis = 'x',
    panel_border = FALSE,
    grid = 'y',
    minor_grid = '',
    show_legend = TRUE,
    ar = NA,
    axis_title_just = 'rt',
    axis_ticks = TRUE
  ) {

    size_med = size*scaler
    size_sml = round(size*0.7)*scaler
    base_linesize = 0.3*scaler

    # justification of axis titles
    xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0,
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
    yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0,
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

    list(
      theme_minimal(base_size = size_med, base_family = family),
      theme(
        # basic
        text = element_text(color = 'black'),
        line = element_line(size = base_linesize, lineend = 'square'),
        # axis
        axis.title = element_text(size = size_med, face = 'bold'),
        axis.title.x = element_text(hjust = xj),
        axis.title.y = element_text(hjust = yj),
        axis.title.y.right = element_text(hjust = yj, angle = 90),
        axis.text = element_text(size = size_med, color = 'black'),
        # strips
        strip.text = element_text(color = 'black', size = size_med),
        strip.background = element_blank(),
        # plot
        title = element_text(face = 'bold'),
        plot.subtitle = element_text(color = 'black', size = size_med, face = 'bold'),
        plot.caption = element_text(color = 'black', size = size_sml, face = 'plain'),
        plot.background = element_blank(),
        panel.background = element_blank(),
        #plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
        # grid
        panel.grid = element_blank()
      ),
      if (isTRUE(axis_ticks)) {
        theme(axis.ticks = element_line(size = rel(0.5), color = 'black'))
      },
      if (identical(grid, 'y')) {
        theme(panel.grid.major.y =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(grid, 'x')) {
        theme(panel.grid.major.x =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(grid, 'xy') | identical(grid, 'yx')) {
        theme(panel.grid.major.y =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'),
              panel.grid.major.x =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'y')) {
        theme(panel.grid.minor.y =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'x')) {
        theme(panel.grid.minor.x =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'xy') | identical(grid, 'yx')) {
        theme(panel.grid.minor.y =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'),
              panel.grid.minor.x =
                element_line(size = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (isTRUE(panel_border)) {
        theme(
          panel.border =
            element_rect(fill = NA)
        )
      },
      if (!isTRUE(show_legend)) {
        theme(legend.position = 'none')
      },
      if (axis == 'x') {
        theme(
          axis.line.x = element_line(linetype = 1, color = 'black')
        )
      },
      if (axis == 'y') {
        theme(
          axis.line.y = element_line(linetype = 1, color = 'black')
        )
      },
      if (axis == 'xy') {
        theme(
          axis.line = element_line(linetype = 1, color = 'black')
        )
      },
      if (!is.na(ar)) {
        theme(
          aspect.ratio = ar
        )
      }
    )
  }

# Misc ------------------------------------------------------------

#' Export ggplot
#'
#' @author Jonas Schöley
ExportFigure <-
  function(figure,
           path,
           filename,
           width = 170,
           height = 100,
           scale = 1,
           device = 'png',
           dpi = 300,
           add_date = FALSE) {
    require(ggplot2)

    if (missing(filename)) {
      filename <- tolower(gsub('\\.', '_', make.names(deparse(substitute(figure)))))
    }
    if (isTRUE(add_date)) {
      filename <- paste0(Sys.Date(), '-', filename)
    }

    arguments <-
      list(
        filename = paste0(filename, '.', device),
        plot = figure,
        path = path,
        width = width,
        height = height,
        units = "mm",
        scale = scale,
        dpi = dpi,
        device = device
      )
    if (device == 'pdf') {
      arguments$useDingbats <- FALSE
    }

    do.call(ggsave, arguments)
  }

#' Plot Matrix
#'
#' @param X A matrix.
#' @param N Number of colors to use for heatmap shading (default 100).
#' @param type Type of color shading. One of 'c' (continuous),
#'   'd' (divergent), 'q' qualitative (default 'c').
#' @param clip Vector of minimum and maximum values for color scale
#'   (default c(NA, NA)). Use NA for no clipping.
#' @param main Title of the plot (character scalar).
#' @param xlab X axis label (character scalar).
#' @param ylab Y axis label (character scalar).
#' @param concol Continuous color scale (default 'cubehelix').
#' @param divcol Divergent color scale (default 'RdBu').
PlotMatrix <- function (
    X, N = 100, type = 'c', clip = c(NA,NA),
    main = '', xlab = '', ylab = '',
    concol = 'cubehelix', divcol = 'RdBu'
) {
  require(rje)
  XX <- t(X[nrow(X):1,])
  if (!is.na(clip[1])) { XX[XX<=clip[1]] <- clip[1] }
  if (!is.na(clip[2])) { XX[XX>=clip[2]] <- clip[2] }
  XX <- RescaleToUnit(XX, clip[1], clip[2])
  col <- switch (concol[1],
                 'cubehelix' = cubeHelix(N),
                 colorRampPalette(concol)(N))
  if (type == 'd') {
    col <- switch (divcol[1],
                   'RdBu' = hcl.colors(N, 'RdBu'),
                   colorRampPalette(divcol)(N))
  }
  if (type == 'q') {
    N <- quantile(XX, probs = seq(0, 1, length.out = N),
                  na.rm = TRUE)
  }
  XX_col <- matrix(
    as.integer(cut(XX, breaks = N, labels = FALSE)),
    nrow = nrow(XX), ncol = ncol(XX)
  )
  image(XX_col, xaxt = 'n', yaxt = 'n', col = col, main = main,
        xlab = xlab, ylab = ylab, useRaster = TRUE)
}

RescaleToUnit <- function (X, a = NULL, b = NULL) {
  if (is.na(a)) {
    a <- min(X, na.rm = TRUE)
   }
  if (is.na(b)) {
    b <- max(X, na.rm = TRUE)
  }
  X_ <- (X-a)/(b-a)
  return(X_)
}

#' Plot PLC Fit Diagnostics
PLCplotFitDiagnostics <- function(dev, epsilon, eta, theta, maxit, outlier,
                                  theme = PLCdefaultTheme) {

  plot_layout_matrix <- matrix(NA, 3, 6)
  plot_layout_matrix[1:2,1:3] <- 1
  plot_layout_matrix[1,4:6] <- 2
  plot_layout_matrix[2,4:6] <- 3
  plot_layout_matrix[3,1:2] <- 4
  plot_layout_matrix[3,3:4] <- 5
  plot_layout_matrix[3,5:6] <- 6
  layout(plot_layout_matrix)
  par(
    bg = theme$bg,
    col = theme$col,
    col.axis = theme$col.axis,
    col.lab = theme$col.lab,
    col.main = theme$col.main,
    fg = theme$fg,
    mar = theme$mar
  )

  N = length(theta$ax)
  m = length(theta$kt)

  plot.new()
  ldev <- log(dev)
  plot.window(xlim = c(0, maxit), y = c(ldev[1]-4, ldev[1]), log = 'y')
  axis(1); axis(2)
  title(xlab = 'Iteration', ylab = 'Deviance', main = 'Log-deviance reduction profile')
  polygon(
    x = c(0, maxit, maxit, 0),
    y = c(ldev[1]-4, ldev[1]-4, dev[1], dev[1]),
    col = theme$deviance_bg_col, border = FALSE
  )
  grid(col = 'black', lty = 1, nx = NULL, ny = NULL, lwd = 0.2, equilogs = FALSE)
  points(x = 0:maxit, y = ldev, col = theme$deviance_line_col,
         pch = theme$deviance_pch, cex = theme$deviance_cex)
  lines(x = 0:maxit, y = ldev, col = theme$deviance_line_col)
  PlotMatrix(epsilon, type = 'd',
             clip = c(-0.4, 0.4), # clip at +- 50%
             main = 'Residuals', xlab = 't', ylab = 'x',
             divcol = theme$color_scale_cols_residuals,
             N = theme$color_scale_n_residuals)
  PlotMatrix(eta, type = 'c', clip = c(-6, 0),
             main = 'Estimated log m(x,t)', xlab = 't', ylab = 'x',
             concol = theme$color_scale_cols_log_mx,
             N = theme$color_scale_n_log_mx)
  plot(x = 1:N, y = theta$ax, xlab = 'x', ylab = 'a(x)',
       main = 'a(x) estimates')
  plot(x = 1:N, y = theta$bx, xlab = 'x', ylab = 'b(x)',
       main = 'b(x) estimates')
  plot(x = 1:m, y = theta$kt, xlab = 't', ylab = 'k(t)',
       main = 'k(t) estimates', pch = ifelse(outlier, 4, 1))
}

#' PLC Plot Fit TOS Theme
PLCtosTheme <- list(
  bg = 'black',
  col = '#EDAC31',
  col.axis = '#EDAC31',
  col.lab = '#E1511F',
  col.main = '#E1511F',
  fg = '#EDAC31',
  mar = c(2,2,2,2),
  deviance_bg_col = '#EDAC31',
  deviance_grid_col = 'black',
  deviance_grid_lwd = 0.2,
  deviance_line_col = '#F33826',
  deviance_pch = 16,
  deviance_cex = 1.5,
  color_scale_cols_residuals = c('#175223', 'black', '#E1511F'),
  color_scale_n_residuals = 5,
  color_scale_cols_log_mx = c('#0285D0', '#28A578', '#EDAC31', '#E1511F', '#F33826'),
  color_scale_n_log_mx = 10
)

#' PLC Plot Fit Default Theme
PLCdefaultTheme <- list(
  bg = 'black',
  col = 'white',
  col.axis = 'white',
  col.lab = 'white',
  col.main = 'white',
  fg = 'white',
  mar = c(2,2,2,2),
  deviance_bg_col = 'grey30',
  deviance_grid_col = 'black',
  deviance_grid_lwd = 0.2,
  deviance_line_col = 'white',
  deviance_pch = 16,
  deviance_cex = 1.5,
  color_scale_cols_residuals = c('#5EDF82', '#497252', 'black', '#6F6388', '#c682fa'),
  color_scale_n_residuals = 50,
  color_scale_cols_log_mx = 'cubehelix',
  color_scale_n_log_mx = 10
)

# Lifetables ------------------------------------------------------

LifeExpectancyFromMortality <- function (mx) {
  n <- length(mx)
  I <- diag(n)
  U <- head(rbind(0, diag(exp(-mx))),-1)
  N <- solve(I-U)
  ex <- colSums(N)
  return(ex)
}

LifespanVarianceFromMortality <- function (mx) {
  n <- length(mx)
  I <- diag(n)
  U <- head(rbind(0, diag(exp(-mx))),-1)
  N <- solve(I-U)
  ex <- colSums(N)
  n2 <- t(ex)%*%(2*N-I)
  vx <- n2 - (ex^2)
  return(vx)
}
