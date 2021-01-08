library(data.table)
library(ggplot2)
library(ggridges)
# available at https://github.com/AndiKur4/MaizePal
library(MaizePal)

# available at https://github.com/smmueller/critical-period-ear-growth
data <- fread('~/Repositories/critical-period-ear-growth/Critical Period Ear Sampling Data.csv')
# subset data for secondary plots
data_1946 <- data[YOR == '1946']
# y min and max for plot
ymin <- min(data_1946$GDD, na.rm = T)
ymax <- max(data_1946$GDD, na.rm = T)

# designate accent colors
accent <- maize_pal('FloweringTime')[4]
accent2 <- maize_pal('RubyGold')[6]

theme_set(theme_void())
ggplot(data) +
  geom_density_ridges(aes(x = GDD, y = as.factor(interaction(YOR, Time))), 
                      fill = 'white', scale = 1.5, show.legend = FALSE, col = accent) +
  geom_density_ridges(data = data_1946, 
                      aes(x = GDD, y = as.factor(interaction(Nlevel, Time))), 
                      fill = 'white', size = 0.7,
                      scale = 1.5, show.legend = FALSE) +
  xlim(c(ymin, ymax))
ggsave('genuary2021/curves_8/thin_green.png', dpi = 200)


ggplot(data) +
  geom_density_ridges(aes(x = GDD, y = as.factor(interaction(YOR, Time))), 
                      fill = 'white',  scale = 1.5, show.legend = FALSE, size = 0.8) +
  geom_density_ridges(data = data_1946, 
                      aes(x = GDD, y = as.factor(interaction(YOR, Time))), 
                      fill = 'white',  scale = 1.5, show.legend = FALSE, col = accent2, 
                      size = 0.6) +
  xlim(c(ymin, ymax))
ggsave('genuary2021/curves_8/wide_orange.png', dpi = 200)


