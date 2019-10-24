library(tripack)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvoronoi)
library(ggforce)

df = read_excel('/Users/smueller/Desktop/2016-2017 ACRE Era Master Data.xlsx') %>% drop_na('R6Lodge')

pick_column <- function() {
  # set.seed(seed)
  # col_nums <- list()
  # for (i in length(values))
  #   print(i)
    x <- sample(8:ncol(df), 1)
    while(colSums(is.na(df[x]) !=0)) {
      print(x)
      x <- sample(8:ncol(df), 1)
    }
  # col_nums[i] <- x
  return(x)
}

pick_column()

# named variables
ggplot(df, aes(V14LAI, R5LAI, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = F, 
                    expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  theme_void()

# 

ggplot(df, aes(df$V14LAI, df$R5LAI)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = T, 
                    expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  theme_void()




