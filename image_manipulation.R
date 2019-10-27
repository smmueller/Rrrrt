# http://mfviz.com/r-image-art/

# Load an image into R (make sure your working directory is set!)

# Packages (for the entire project)
library(imager)    # image loading and processing
library(dplyr)     # data manipulation
library(ggplot2)   # data visualization
library(tidyr)     # data wrangling
library(ggvoronoi) # visualization

# Load an image into R
# img <- image_read("/Users/Sarah/Documents/IMG-1790.jpg")
img <- load.image(file = "/Users/Sarah/Documents/IMG-1790_small.jpg")

# Print the image object out
# img %>% image_resize('500')
plot(img)

# Represent the image as a data frame
img_df <- as.data.frame(img)

# Show head of the data frame
head(img_df)

# Add more expressive labels to the colors
img_df <- img_df %>%
  mutate(channel = case_when(
    cc == 1 ~ 'Red',
    cc == 2 ~ 'Green',
    cc == 3 ~ 'Blue'
  ))

# Reshape the data so that each row is a point
img_wide <- img_df %>%
  select(x, y, channel, value) %>% # dropping cc
  spread(key = channel, value = value) %>%
  mutate(color = rgb(Red, Green, Blue))

# now we can recreate the image in ggplot (very slow!)
ggplot(img_wide) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() #use the actual value in the `color` column

# plot better (didn't actually run because very slow)
ggplot(img_wide) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Take a sample of rows from the data frame
sample_size <- 5000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Create random weights for point size
img_sample$size <- runif(sample_size)

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Use the amount of blue present in each point to determine the size
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = Blue)) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Create a Voronoi Diagram of the sampled points
ggplot(img_sample) +
  geom_voronoi(mapping = aes(x = x, y = y, fill = color), color='white', size=0) +
  scale_fill_identity() +
  scale_y_reverse() +
  theme_void()

# Edge detection
# detect edges in the image
edges <- cannyEdges(img)
plot(edges)

# convert the edge image to a data frame for manipulation
edges_df <- edges %>%
  as.data.frame() %>%
  select(x, y) %>% #originally contained x, y, z, cc
  distinct(x, y) %>% # remove duplicates
  mutate(edge = 1) # indicate that these overvations represent an edge)

# uses edges to weight sampling points in image
img_wide <- img_wide %>%
  left_join(edges_df)

# apply a low weight to the non-edge points
img_wide$edge[is.na(img_wide$edge)] <- 0.05 # replaces NA with 0.05

# re-sample from the image, applying a higher probability to the edge points
img_edge_sample <- img_wide[sample(nrow(img_wide), sample_size, prob = img_wide$edge),]

# Re-create the scatter plot with the re-sampled data (add random sizing of circles)
ggplot(img_edge_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color,  size = edge * runif(sample_size))) +
  guides(fill = FALSE, size= FALSE) +
  scale_color_identity() +
  scale_y_reverse() +
  theme_void() # Remove axes, background

# Re-create the voronoi diagram with the re-sampled data
# ggplot(img_edge_sample) +
#   geom_voronoi(mapping = aes(x = x, y = y, fill = color), color='black') +
#   scale_fill_identity() +
#   guides(fill = FALSE) +
#   scale_y_reverse() +
#   theme_void() # Remove axes, background

common_colors <- img_wide %>% group_by(color, Red) %>% summarize(count = n()) %>% arrange(desc(Red))
tail(common_colors)
cc <- common_colors[1:8,]$color
show_col(cc)
