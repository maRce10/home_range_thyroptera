# uno de tus grupos
obs <- data.frame( lat = c(100, 110, 90, 85,60, 130), lot = c(49, 59,30, 23, 56, 63))

# de tus datos, promedio de sd de cada grupo
sd <- sd(c(obs$lat, obs$lon))



area.limits.y <- c(0, 200)
area.limits.x <- c(0, 300)

out <- lapply(1:11, FUN = function(x){
  
  # sim x  
  center.x <- sample(seq(area.limits.x[1], area.limits.x[2], length.out = 10), 1)
    
    min.x <- center.x - sd 
    
    if (min.x < area.limits.x[1]) min.x <- area.limits.x[1]

    max.x <- center.x + sd 
    
    if (max.x > area.limits.x[2]) max.x <- area.limits.x[2]
    
    xs <- runif(30, min = min.x, max = max.x)
  
    # sim y
    center.y <- sample(seq(area.limits.y[1], area.limits.y[2], length.out = 10), 1)
    
    min.y <- center.y - sd 
    
    if (min.y < area.limits.y[1]) min.y <- area.limits.y[1]

    max.y <- center.y + sd 
    
    if (max.y > area.limits.y[2]) max.y <- area.limits.y[2]

    ys <- runif(30, min = min.y, max = max.y)
    
    df <- data.frame(xs, ys)
    return(df)
}
)


plot(1, col = "white", xlim = area.limits.x, ylim = area.limits.y)


for(i in 1:length(out))
points(out[[i]], pch =20, col = i)



