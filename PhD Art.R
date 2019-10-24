library(tripack)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvoronoi)
library(ggforce)

df = read_excel('/Users/Sarah/Documents/Repositories/Rrrrt/2016-2017 ACRE Era Master Data.xlsx') %>% drop_na('R6Lodge')

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

pick_column(1)

# named variables
ggplot(df, aes(R1NNI, R6ComGYBu, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                    expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  scale_fill_manual(values=myCols_contrast)+
  theme_void()


ggplot(df, aes(R6GYHand, R6ComGYBu, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = F, 
                    expand = unit(-0.3, 'mm'), radius = unit(0.5, 'mm'), normalize = TRUE)+
  scale_fill_manual(values=myCols_contrast)+
  theme_void()
# 

# ggplot(df, aes(df$V14LAI, df$R5LAI)) +
#   geom_voronoi_tile(aes(fill = Hybrid), colour = T, 
#                     expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
#   theme_void()


x_seed <- sample(1:1000, 5, replace=F)
y_seed <- sample(1:1000, 5, replace=F)

ggplot(df, aes(x=pick_column(3)[[2]], y=pick_column(4)[[2]])) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                    expand = unit(-0.5, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
  xlab(pick_column(3)[[1]]) +
  ylab(pick_column(4)[[1]])+
  scale_fill_manual(values=myCols_contrast)+
  # scale_fill_grey(start = 0.01, end = 0.99) +
  theme_void()


for (i in seq_along(x_seed)) {
    plot <- ggplot(df, aes(x=pick_column(x_seed[i])[[2]], y=pick_column(y_seed[i])[[2]], group = -1L)) +
            geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                      expand = unit(-0.7, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
            xlab(pick_column(x_seed[i])[[1]]) +
            ylab(pick_column(y_seed[i])[[1]]) +
            scale_fill_manual(values=myCols_contrast)
    print(plot)
}

for (i in seq_along(x_seed)) {
  plot <- ggplot(df, aes(x=pick_column(x_seed[i])[[2]], y=pick_column(y_seed[i])[[2]])) +
    geom_voronoi_tile(aes(fill = Hybrid), colour = T, 
                      expand = unit(-0.7, 'mm'), radius = unit(0, 'mm'), normalize = TRUE) +
    xlab(pick_column(x_seed[i])[[1]]) +
    ylab(pick_column(y_seed[i])[[1]]) +
    scale_fill_manual(values=myCols)
  print(plot)
}


