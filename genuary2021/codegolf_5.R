par(bg = 'black', mar = rep(0, 4))

plot(x = sample(1:1000, 300, replace = TRUE),
     y = sample(1:1000, 300, replace = TRUE),
     pch = 8, 
     col = sample(c('ivory', 'cornsilk', 'lightgoldenrod'), 300, replace = T),
     cex = sample(seq(0.1, 2, 0.1), 300, replace = T))


