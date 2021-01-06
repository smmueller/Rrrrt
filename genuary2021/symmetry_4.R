library(data.table)
library(ggplot2)
# available at https://github.com/AndiKur4/MaizePal
library(MaizePal)

# available at https://github.com/smmueller/critical-period-ear-growth
data <- fread('~/Repositories/critical-period-ear-growth/Critical Period Ear Sampling Data.csv')

set.seed(14)

#' Function for creating plots
#' 
#' @param xvar string; column name of x variable
#' @param yvar string; column name of y variable
#' @param palvar string; name of color palette to use
#' @param shapevar integer; shape to be used (see ggplot point shapes)
#' @param samplen integer; number of observations to be included in plot
#' @param sizevar integer; multiplier for size of points
#' 
#' @return A ggplot object

symmetry_plot <- function(xvar, yvar, palvar, shapevar, samplen, sizevar){
  # get color choice
  color_choice <- maize_pal(palvar)[1]
  
  # create data set
  update_names <- c(xvar, yvar)
  
  sub_index <- sample(1:nrow(data), samplen)
  sub <- data[sub_index, ]
  
  sub_sub <- sub[, ..update_names]
  
  repeated <- rbindlist(list(original = sub_sub,
                             inverse = sub_sub,
                             inverse_left = sub_sub,
                             inverse_right = sub_sub),
                        idcol = 'set')
  
  repeated[set == 'inverse', (update_names) := lapply(.SD, '*', -1), .SDcols = update_names]
  repeated[set == 'inverse_right', (update_names[2]) := lapply(.SD, '*', -1), .SDcols = update_names[2]]
  repeated[set == 'inverse_left', (update_names[1]) := lapply(.SD, '*', -1), .SDcols = update_names[1]]
  
  
  g <- ggplot(repeated) +
    geom_point(aes_string(x = xvar, y = yvar), col = color_choice, fill = 'white', shape =  shapevar, size = 4*sizevar) +
    geom_point(aes_string(x = xvar, y = yvar), fill = color_choice, col = 'white', shape = shapevar, size = 2*sizevar) +
    theme_void()  +
    theme(panel.background = element_rect(fill = maize_pal(palvar)[4]),
          legend.position = 'none')
    
  return(g)
}

# make several random plots and save them
for(i in 1:15){
  # random colomns
  vars <- sample(c('EarDw', 'EarNper', 'GDD', 'EarNmg'), 2)
  # random color palette
  palvar <- sample(names(maize_palettes)[-c(1:2)], 1)
  # random shape
  shapevar <- sample(21:25, 1)
  # random number of samples
  sample_num <- sample(50:500, 1)
  # random size adjustment
  sizevar <- sample(seq(1, 2, 0.25), 1)
  
  g <- symmetry_plot(xvar = vars[1], yvar = vars[2], palvar = palvar, 
                     shapevar = shapevar, samplen = sample_num, sizevar = sizevar)
  ggsave(paste0('symmetry_4_plots/', paste(vars, collapse = '_'), palvar, shapevar, '.png'), dpi = 200)
}

symmetry_plot(xvar = 'GDD', yvar = 'EarNper', palvar = 'GlassGem', 
                   shapevar = 21, samplen = 50, sizevar = 1)
ggsave(paste0('symmetry_4_plots/', 'GDD_EarNper', 'GlassGem', 21, '.png'), dpi = 200)

