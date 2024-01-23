#
# Author: Andrew Disher
# Date: 2024/01/14
# 

# ---------------------------
# ----- Package Imports -----
# ---------------------------

box::use(
  dplyr[`%>%`],
  fabletools[autoplot],
  feasts[ACF, PACF],
  ggplot2[aes, geom_histogram, geom_line, ggplot, labs, stat_qq, stat_qq_line, theme_minimal],
  gridExtra[grid.arrange],
  tsibble[as_tsibble, tsibble]
)

# ------------------------------------------
# ----- Utility Functions For Plotting -----
# ------------------------------------------

# ---------------------------------------------------------
# ----- Function to Create a Minimal Time Series Plot -----
# ---------------------------------------------------------

#' @title Make a minimal time series plot
#' @description
#' This function is called to create a simple time series plot with ggplot2 components. 
#' This is a minimal plot, but after calling the function, additional elements can be
#' added to it by calling the traditional `ggplot2` commands. 
#' @param data Data frame of data
#' @param data_var Character string specifying the date variable to plot along the x-axis (must be of type Date or POSIXct)
#' @param y_var Character string specifying variable to plot on the y-axis
#' @param color Character string specifying hex-color to use for the time series
#' @param ... Additional arguments to be passed to ggplot2 labs() function, to create plot labels. X-axis label is always "Date".
#' 
#' @author Andrew Disher
#' @md
#' @usage time_series$plotTS(data, date_var, y_var, color = "#007aff", ...)
#' 
#' @examples
#' # Create data frame                                                             
#' data = data.frame(date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"),
#'                   y_var = rnorm(n = 90, mean = 20, sd = 2))                     
#' # Basic usage                                                                   
#' time_series$plotTS(data = data,                                                 
#'                    date_var = "date",                                           
#'                    y_var = "y_var",                                             
#'                    color = "#1a7e2e")                                           
#'                                                                                 
#' # Using additional parameters for labels                                        
#' time_series$plotTS(data = data,                                                 
#'                    date_var = "date",                                           
#'                    y_var = "y_var",                                             
#'                    color = "#1a7e2e",                                           
#'                    y = "Some Variable Name",                                    
#'                    title = "This is a basic plot with some labels")             
#' 
#' 
#' @export
plotTS <- function(data, date_var, y_var, color = "#007aff", ...) {
  ts_plot <- ggplot(data = data, mapping = aes(x = .data[[date_var]], 
                                    y = .data[[y_var]])) +
    geom_line(color = color) + 
    theme_minimal() + 
    labs(x = "Date",
         ...)
  
  return(ts_plot)
}

# ----------------------------------------
# ----- Function to Plot a Histogram -----
# ----------------------------------------

#' @title Make a histogram
#' @description
#' Function to create a decently good looking histogram, given a numeric vector of
#' data.  
#' @param data_vec A numeric vector of data.
#' @param color A character string specifying the hex-color to use for the bars
#' @param title A character string specifying the title for the plot
#' @param bins Number of bins to separate the data into
#' 
#' @author Andrew Disher
#' @md
#' @usage time_series$plotHist(data_vec, color = "#007aff", title = "Histogram", bins = NULL)
#' 
#' @examples
#' # Histogram for some residuals                        
#' residuals <- rnorm(2000, mean = 0, sd = 1)             
#'                                                       
#' time_series$plotHist(data_vec = residuals,            
#'                      title = "Histogram of Residuals",
#'                      bins = 30)                       
#' 
#' @export
plotHist <- function(data_vec, color = "#007aff", title = "Histogram", bins = 30) {
  # Data frame for plotting
  plot_data <- data.frame(values = data_vec %>% 
                            as.numeric())
  
  # Construct the Histogram
  histogram <- ggplot(data = plot_data,
                      mapping = aes(x = values)) +
    geom_histogram(fill = color, color = "black", bins = bins) +
    theme_minimal() +
    labs(x = "Values",
         y = "Frequency",
         title = title)
  
  return(histogram)
}

# -------------------------------------------------------
# ----- Function to Create a Minimal Normal QQ Plot -----
# -------------------------------------------------------

#' @title Create a normal QQ plot
#' @description
#' This function abstracts the use of `ggplot2` commands to create a normal Quantile-Quantile
#' plot, given a vector of data. Typically used by passing a model's residuals into it
#' to compare the residual distribution to a normal distribution.
#' @param data_vec A numeric vector of data, typically a vector of model residuals.
#' @param color A character string specifying the color of the points
#' @param title A character string specifying the title for the plot 
#' @param dist_func A quantile distribution function provided by the `stats` package. Default is `stats::qnorm`
#' @param dparams Parameters to pass to specified distribution function. 
#' 
#' @author Andrew Disher
#' @md
#' @usage time_series$plotQQ(data_vec,        
#'                    color = "#007aff",      
#'                    title = "Normal QQ Plot", 
#'                    dist_func = stats::qnorm, 
#'                    dparams = NULL)                 
#' 
#' @examples
#' # QQ Plot for a random residual vector                   
#' residuals <- rnorm(n = 200, mean = 0, sd = 2)            
#'                                                          
#' time_series$plotQQ(data_vec = residuals,                 
#'                    title = "Normal QQ Plot of Residuals")
#' 
#' 
#' @export
plotQQ <- function(data_vec, color = "#007aff", title = "Normal QQ Plot", dist_func = stats::qnorm,
                   dparams = NULL) {
  # Data frame for plotting
  plot_data <- data.frame(values = data_vec %>% 
                            as.numeric())
  
  if (is.null(dparams)) {
    # Construct the Normal QQ Plot
    QQ_plot <- ggplot(data = plot_data, mapping = aes(sample = values)) +
      stat_qq_line(distribution = dist_func) +
      stat_qq(color = color, distribution = dist_func) +
      theme_minimal() +
      labs(x = "Theoretical Quantiles",
           y = "Sample Quantiles",
           title = title) 
  } else {
    # Construct the Normal QQ Plot with additional distribution params
    QQ_plot <- ggplot(data = plot_data, mapping = aes(sample = values)) +
      stat_qq_line(distribution = dist_func, dparams = dparams) +
      stat_qq(color = color, distribution = dist_func, dparams = dparams) +
      theme_minimal() +
      labs(x = "Theoretical Quantiles",
           y = "Sample Quantiles",
           title = title) 
  }
  
  
  return(QQ_plot)
}

# --------------------------------------------------------
# ----- Function to Produce Model Diagnostics Output -----
# --------------------------------------------------------

#' @title Create diagnostic plots for a model
#' @description
#' This function simply creates a panel containing all useful residual diagnostic
#' plots for an ARIMA model.
#' @param resid_vec A numeric vector of residuals
#' 
#' @author Andrew Disher
#' @md
#' @usage time_series$model_diagnostics(resid_vec = residuals)                
#' 
#' @examples
#' # Model diagnostics for a random vector of 'residuals'
#' residuals <- rnorm(n = 2000, mean = 0, sd = 2) 
#' 
#' time_series$model_diagnostics(resid_vec = residuals)
#' 
#' @export
model_diagnostics <- function(resid_vec) {
  # Data frame for plotting
  plot_data <- data.frame(values = as.numeric(resid_vec),
                          time_step = 1:length(resid_vec)) %>% 
    as_tsibble(index = time_step)
  
  # Create ordered residual time series
  ordered_residuals <- plotTS(data = plot_data, 
                              date_var = "time_step",
                              y_var = "values",
                              title = "Ordered Residuals")
  
  # Create histogram of residuals
  histogram <- plotHist(data_vec = plot_data$values)
  
  # Create Normal QQ Plot of residuals
  qq_plot <- plotQQ(data_vec = plot_data$values)
  
  # Create plots for ACF and PACF
  acf_plot <- plot_data %>% 
    ACF(values) %>% 
    autoplot() +
    theme_minimal()
  
  pacf_plot <- plot_data %>% 
    PACF(values) %>% 
    autoplot() +
    theme_minimal()
  
  # Create a layout matrix
  layout <- rbind(c(1, 2),
                  c(3, 4),
                  c(5, 5))
  
  
  # Arrange the plots in a grid
  plot_grid <- grid.arrange(histogram, qq_plot, acf_plot, pacf_plot, ordered_residuals, layout_matrix = layout)
  
  return(plot_grid)
}


