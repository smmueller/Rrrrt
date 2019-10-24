#https://chichacha.netlify.com/2019/01/19/extracting-colours-from-your-images-with-image-quantization/

library(tidyverse) ## I love ggplot and tidy data.... so this is a must for anything. 
library(magick) ## Hello magick!!! 
library(scales) ## I find rescale function so useful!  and i love show_col function :)
library(imager) ## i don't know how else to convert image to data frame at the moment. 


## I'll use this plum flower image I took while back to extract colour. 
## using image_read function in magick I can read image as below. 
im <- image_read("/Users/Sarah/Documents/IMG-1790_small.jpg") %>% image_contrast() #image_equalize()

## now display image with 500px wide
im %>% image_resize("500")

im %>%
  image_resize("500") %>%
  image_quantize(max=20)

im %>% image_resize("500") %>% image_contrast(sharpen=1)


## Function to get n number of colours out of your image. (optionally you can specify different colour space)
get_colorPal <- function(im, n=9, cs="HCL"){
  # print(cs)
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

## Here's example using plum flower image 
myCols_contrast <- get_colorPal(im)$hex
show_col(myCols_contrast)

### What would different colorspaces do?
params <- list(im=list(im), 
               n=12, ## number of colour you want 
               cs=colorspace_types()[-5]) ## gray fails so I've removed it...

my_colors <- pmap_df(params,get_colorPal)

## Let's see what got spitted out as results for different colourspace specifiction in image_quantize function.

## I want to view reduced colours by different colourspaces all at once! 
my_colors %>%  
  group_by(colorspace) %>%
  mutate(ypos=row_number(value)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x=fct_infreq(colorspace),y=ypos, fill=hex)) +  
  geom_tile() +
  geom_text(aes(label=hex), color="#ffffffbe", 
            size=4) +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  theme_void() +
  coord_flip(ylim=c(1,12)) +
  theme(axis.text = element_text(color = "black", hjust=1)) +
  labs(caption="Using different colourspce to reduce the colour used in images")
