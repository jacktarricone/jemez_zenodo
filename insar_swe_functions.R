# functions used in InSAR SWE analysis
# jack tarricone
# july 18, 2022

leinss_swe <- function(phase, alpha, inc_angle) {
  
  wavelength <- 0.238403545
  k <- 2 * pi / wavelength
  return(phase / (alpha * k * (1.59 + inc_angle^2.5)))
}


depth_from_phase <-function(delta_phase, inc_angle, perm, wavelength = 23.8403545){
  
  delta_z = (-delta_phase * wavelength) / (4 * pi * (cos(inc_angle) - sqrt(perm - sin(inc_angle)^2)))
  
}

stat_smooth_func2 <- function(mapping = NULL, data = NULL,
                              geom = "smooth", position = "identity",
                              ...,
                              method = "auto",
                              formula = y ~ x,
                              se = TRUE,
                              n = 80,
                              span = 0.75,
                              fullrange = FALSE,
                              level = 0.95,
                              method.args = list(),
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              xpos = NULL,
                              ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq1 <- substitute(italic(y) == a + b %.% italic(x), 
                                              list(a = format(coef(m)[[1]], digits = 3), 
                                                   b = format(coef(m)[[2]], digits = 3),
                                                   r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq1))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)