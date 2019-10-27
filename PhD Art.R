library(tripack)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvoronoi)
library(ggforce)

df <- read_excel('/Users/Sarah/Documents/Repositories/Rrrrt/2016-2017 ACRE Era Master Data.xlsx') %>% drop_na('R6Lodge')
colnames(df) <- gsub("[[:punct:]]", "_", colnames(df))



pick_column <- function(seed) {
  set.seed(seed)
  # col_nums <- list()
  # for (i in length(values))
  #   print(i)
    x <- sample(8:ncol(df), 1)
    while(colSums(is.na(df[x]) !=0)) {
      print(x)
      x <- sample(8:ncol(df), 1)
    }
    stupid_list <- list(names(df[x]), pull(df[x]))
  # col_nums[i] <- x
  return(stupid_list)
}

# pick_column(1)

# theme for no margins
no_margin_theme <- theme(panel.background=element_blank(), 
                         panel.grid.major=element_blank(), 
                         panel.grid.minor=element_blank(), 
                         panel.margin = unit(c(0, 0, 0, 0), "cm"),       
                         axis.ticks=element_blank(), 
                         axis.text.x=element_blank(), 
                         axis.text.y=element_blank(), 
                         axis.title.x=element_blank(), 
                         axis.title.y=element_blank(),
                         plot.background = element_rect(fill = "transparent",colour = NA),
                         plot.margin = unit(c(-1, -1.2, -1.2, -1.5), "cm"),  # Edited code
                         legend.position = 'none') 


# named variables
ggplot(df, aes(R1NNI, R6ComGYBu, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                    expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  scale_fill_manual(values=myCols_contrast)+
  no_margin_theme 
ggsave('NNIxGYCombine.png', path='/Users/Sarah/Desktop/',
       width = 4, height = 4, units = "in", dpi = 'retina')


ggplot(df, aes(R6GYHand, R6ComGYBu, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                    expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  scale_fill_manual(values=myCols_contrast)+
  no_margin_theme
ggsave('46GYHandxGYCombine.jpeg', path='/Users/Sarah/Desktop/Rrrrt prints', 
       width = 6, height = 4, units = "in", dpi = 'retina')
# 

# ggplot(df, aes(df$V14LAI, df$R5LAI)) +
#   geom_voronoi_tile(aes(fill = Hybrid), colour = T, 
#                     expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
#   theme_void()


# x_seed <- sample(1:1000, 15, replace=F)
# y_seed <- sample(1:1000, 15, replace=F)
# # add 5 more for a few more options
# x_seed <- append(x_seed, sample(1:1000, 5, replace=F))
# y_seed <- append(y_seed, sample(1:1000, 5, replace=F))

# save the generated seeds
x_seed <- c(789, 855,  64, 425,  54, 781, 513, 558, 631,  24,  29, 979, 306, 914, 473, 467, 987, 209, 761, 175)
y_seed <- c(531, 164, 349, 341, 226, 990, 461, 969, 169, 223, 822, 891, 783, 608, 495, 733, 865, 896, 519, 909)
# ggplot(df, aes(x=pick_column(3)[[2]], y=pick_column(4)[[2]])) +
#   geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
#                     expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
#   xlab(pick_column(3)[[1]]) +
#   ylab(pick_column(4)[[1]])+
#   scale_fill_manual(values=myCols_contrast)+
#   # scale_fill_grey(start = 0.01, end = 0.99) +
#   theme_void()

for (i in seq_along(x_seed)) {
    x_name <- pick_column(x_seed[i])[[1]]
    y_name <- pick_column(y_seed[i])[[1]]
    plot <- ggplot(df, aes(x=pick_column(x_seed[i])[[2]], y=pick_column(y_seed[i])[[2]], group = -1L)) +
            geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                      expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
            # xlab(pick_column(x_seed[i])[[1]]) +
            # ylab(pick_column(y_seed[i])[[1]]) +
            scale_fill_manual(values=myCols_contrast) +
            no_margin_theme
            # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            # panel.background = element_blank(), axis.line = element_line(colour = "black"))
    ggsave(paste0('44_grouped ', x_name, ' x ', y_name, '.jpeg'), path='/Users/Sarah/Desktop/Rrrrt prints/',
           width = 4, height = 4, units = "in", dpi = 'retina')
    ggsave(paste0('46_grouped ', x_name, ' x ', y_name, '.jpeg'), path='/Users/Sarah/Desktop/Rrrrt prints/',
           width = 6, height = 4, units = "in", dpi = 'retina')
    print(plot)
}

for (i in seq_along(x_seed)) {
  x_name <- pick_column(x_seed[i])[[1]]
  y_name <- pick_column(y_seed[i])[[1]]
  plot <- ggplot(df, aes(x=pick_column(x_seed[i])[[2]], y=pick_column(y_seed[i])[[2]])) +
    geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                      expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
    # xlab(pick_column(x_seed[i])[[1]]) +
    # ylab(pick_column(y_seed[i])[[1]]) +
    scale_fill_manual(values=myCols_contrast) +
    no_margin_theme
  ggsave(paste0('44_ungrouped ', x_name, ' x ', y_name, '.jpeg'), path='/Users/Sarah/Desktop/Rrrrt prints/',
         width = 4, height = 4, units = "in", dpi = 'retina')
  ggsave(paste0('46_ungrouped ', x_name, ' x ', y_name, '.jpeg'), path='/Users/Sarah/Desktop/Rrrrt prints/',
         width = 6, height = 4, units = "in", dpi = 'retina')
  # print(plot)
}


