# https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html

library(ggvoronoi)
set.seed(45056)
x <- sample(1:200,100)
y <- sample(1:200,100)
points <- data.frame(x, y,
                     distance = sqrt((x-100)^2 + (y-100)^2))
circle <- data.frame(x = 100*(1+cos(seq(0, 2*pi, length.out = 2500))),
                     y = 100*(1+sin(seq(0, 2*pi, length.out = 2500))),
                     group = rep(1,2500))

ggplot(data=points, aes(x=x, y=y, fill=distance)) + 
  geom_voronoi(outline = circle)

ggplot(points) +
  geom_point(aes(x,y,color=distance)) +
  geom_path(data=circle,aes(x,y,group=group))

ggplot(points) +
  geom_voronoi(aes(x,y,fill=distance))

ggplot(points,aes(x,y)) +
  stat_voronoi(geom="path") +
  geom_point()

#### Try with some real data
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
df = read_excel('/Users/smueller/Desktop/2016-2017 ACRE Era Master Data.xlsx')

# df2 <- filter(df, Site == 'Pergamino')

mean(dist(df$SC_Early, method = "euclidean"))



df2$score <- apply(df2[,29:36], 1, function(x) mean(dist(x))) 



#df3 = distinct(df)
ggplot(df2) +
  geom_voronoi(aes(BiomassR1_V, BiomassR8_V, fill=YldDryHH), color='black') 
  geom_point(aes(BiomassR1_V, BiomassR8_V)) +
  theme_minimal()

ggplot(df2,aes(BiomassR1_V, BiomassR8_V)) +
  stat_voronoi(geom="path")
+
  geom_point()


### Try another method
library(tripack)
plot(voronoi.mosaic(runif(50), runif(50), duplicate="remove"))
voronoi.mosaic(runif(50), runif(50), duplicate="remove")


## library ggforce
## ideas:
### randomly choose variables?
### how to choose colors? Maybe both colored and B&W?
### A shape other than square? But maybe square is nice.
### make a bunch of individual that I can arrange and swap out whenever?
### complexity? How many colors do we want to use?
my_circle <- data.frame(x = 50*(1+cos(seq(0, 2*pi, length.out = 250))),
                     y = 50*(1+sin(seq(0, 2*pi, length.out = 250))),
                     group = rep(1,250))


ggplot(df, aes(V14LAI, R5LAI, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = F, 
                    expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  theme_void()

ggplot(df, aes(V14LAI, R5LAI, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', 
                    expand = unit(-0.8, 'mm'), radius = unit(0, 'mm'), normalize = TRUE)+
  # scale_fill_grey(start=1, end=0.1)+
  theme_void()


ggplot(df, aes(V14LAI, R5LAI, group = -1L)) +
  geom_voronoi_tile(aes(fill = Hybrid), colour = 'black', expand = unit(0, 'mm'), radius = unit(2, 'mm'))+
  theme_void()

ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_segment() +
  geom_point()


### could you plot a bunch of different plots on top of each other as points to make a cool collage?
library(deldir)
library(ggforce)
# Voronoi
# You usually wants all points to take part in the same tesselation so set
# the group aesthetic to a constant (-1L is just a convention)
ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species)) +
  geom_voronoi_segment() +
  geom_text(aes(label = stat(nsides), size = stat(vorarea)),
            stat = 'delvor_summary', switch.centroid = TRUE
  )

# Difference of normalize = TRUE (segment layer is calculated without
# normalisation)
ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species),  normalize = TRUE) +
  geom_voronoi_segment()

# Set a max radius
ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species), colour = 'black', max.radius = 0.25)

# Set custom bounding polygon
triangle <- cbind(c(3, 9, 6), c(1, 1, 6))
ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species), colour = 'black', bound = triangle)

# Use geom_shape functionality to round corners etc
ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species), colour = 'black',
                    expand = unit(-.5, 'mm'), radius = unit(2, 'mm'))

# Delaunay triangles
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_delaunay_tile(alpha = 0.3, colour = 'black')

# Use geom_delauney_segment2 to interpolate aestetics between end points
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_delaunay_segment2(aes(colour = Species, group = -1), size = 2,
                         lineend = 'round')
