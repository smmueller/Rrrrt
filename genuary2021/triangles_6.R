# This experiment is copied from an example in the plotdesignr repo
# https://github.com/smmueller/plotdesignr/blob/master/docs/plotdesignr_introduce_concept.Rmd

################################################################################
# MAKE EXPERIMENT DATA ---------------------------------------------------------
################################################################################

library(data.table)
library(ggplot2)

# set block and treatment number
n_blocks <- 4
n_treats <- 4

# define the field latitude and longitude 
lat <- 1:(n_blocks*n_treats)
lon <- 1:(n_blocks*n_treats)

# create a design that ensures all treatments were present in both directions
order <- data.table('4' = c('A', 'B', 'C', 'D'), 
                    '3' = c('D', 'C', 'A', 'B'),
                    '2' = c('B', 'A', 'D', 'C'),
                    '1' = c('C', 'D', 'B', 'A'),
                    'ns_block' = 1:4)
# melt to long format
order_long <- melt(order, id.vars = 'ns_block', variable.name = 'ew_block', value.name = 'treat', variable.factor = FALSE)
order_long[, ew_block := as.numeric(ew_block)]

# create the experimental data
set.seed(77)
field_df <- data.table(expand.grid('lat' = lat, 'lon' = lon))

# sort by long and add ns_blocks
setorder(field_df, lon)
field_df[, ns_block := rep(1:n_blocks, each = n_blocks, length.out = nrow(field_df))]

# sort by lat and add ew_blocks
setorder(field_df, lat)
field_df[, ew_block := rep(1:n_blocks, each = n_blocks, length.out = nrow(field_df))]

# add plot ids
field_df[, plot := paste0(ns_block, ew_block)]

# merge field_df and treatment information
exp <- order_long[field_df, on = c('ns_block', 'ew_block')]

# create an east/west gradient to the field yields
exp[, yield_lat := lat*2]

# add a treatment effect
exp[treat == 'A', yield_t := rnorm(.N, 10, 1)]
exp[treat == 'B', yield_t := rnorm(.N, 5, 1)]
exp[treat == 'C', yield_t := rnorm(.N, 2, 1)]
exp[treat == 'D', yield_t := rnorm(.N, 1, 1)]

# make biased_yield by summing the treatment effect (yield_t) and spatial yield gradient (yield+lat)
exp[, yield := yield_lat + yield_t]

# set blocks to factors for lmer
exp[, ns_block := as.factor(ns_block)]
exp[, ew_block := as.factor(ew_block)]

# format for plotting
# function to find ymin/ymax and xmin/xmax for geom_rect
find_borders <- function(df, by_var){
  df[, paste0(by_var, '_xmin') := min(lat) - 0.5, by = by_var]
  df[, paste0(by_var, '_xmax') := max(lat) + 0.5, by = by_var]
  
  df[, paste0(by_var, '_ymin') := min(lon) - 0.5, by = by_var]
  df[, paste0(by_var, '_ymax') := max(lon) + 0.5, by = by_var]
}

find_borders(exp, 'plot')
find_borders(exp, 'ew_block')
find_borders(exp, 'ns_block')

# where the treatment labels should be placed
exp[, text_loc_x := mean(lat), by = plot]
exp[, text_loc_y := mean(lon), by = plot]

# nice labels for density plots
exp[, ew_block_labels := paste('Block', ew_block)]
exp[, ns_block_labels := paste('Block', ns_block)]

################################################################################
# MAKE PLOTS -------------------------------------------------------------------
################################################################################

theme_set(theme_void())
library(MaizePal)  

for(i in names(maize_palettes)){
  pal <- maize_pal(i)
  
  p <- ggplot(exp) +
    geom_polygon(aes(x = lat, y = lon, group = ew_block_labels), col = pal[1], 
                 fill = pal[2], alpha = 0.8) +
    geom_polygon(aes(x = lat, y = lon, group = plot), col = pal[3], fill = pal[4], 
                 alpha = 0.5) +
    theme(panel.background = element_rect(fill = pal[5]),
          legend.position = 'none')
  
  ggsave(paste0('genuary2021/triangles_6/triangles_', i, '.png'), plot = p, dpi = 200)
}

################################################################################
# CONVERT TO GIF ---------------------------------------------------------------
################################################################################

library(magick)
## list file names and read in
imgs <- list.files('genuary2021/triangles_6/', full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "genuary2021/triangles_6/triangles.gif")

