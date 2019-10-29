library(ggplot2)
library(ggdark)

saveplot <- function(param) {
  for (i in 1:length(param)) {
    png(paste("Frame",sprintf("%06d", i),".png",sep=""),width=480)
    print(DrawGraph(param[i]))
    dev.off()
  }
}
## Stitch frames together with
## ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output.mp4

totalframes <- 299

## Binomial Distribution

# Probability

DrawGraph <- function(p) {
  ggplot(mapping=aes(x=0:50))  + 
    stat_function(fun=dbinom,
                  color="red",
                  args=list(size=50,prob=p),
                  n=51,
                  geom = "point"
                  ) +
    scale_x_continuous(limits = c(0, 50)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("probability") +
    ggtitle(paste("Binomial distribution with 50 trials and probability of success p=",format(p,digits = 3, nsmall = 2, width = 3),sep="")) +
    dark_theme_gray()

}

?format

param <- seq(0,1,length.out = (totalframes+1)/2)
param <- c(param,rev(param)) # totalframes + 1 frames
saveplot(param[-length(param)]) # totalframes

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output1.mp4")

# Trials

DrawGraph <- function(trials) {
  ggplot(mapping=aes(x=0:50))  + 
    stat_function(fun=dbinom,
                  color="red",
                  args=list(size=trials,prob=0.3),
                  n=51,
                  geom = "point"
    ) +
    scale_x_continuous(limits = c(0, 50)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("probability") +
    ggtitle(paste("Binomial distribution with ",format(trials, width = 2), " trials and probability of success p=0.3", sep="")) +
    dark_theme_gray()
  
}

DrawGraph(50)

param <- 1:((totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output2.mp4")

## Poisson Distribution

DrawGraph <- function(lambda) {
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dpois,
                  color="orange",
                  args=list(lambda = lambda),
                  n=21,
                  geom = "point"
    ) +
    scale_x_continuous(limits = c(0, 20)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("probability") +
    ggtitle(paste("Poisson distribution with rate parameter lambda = ",format(lambda,digits=3, nsmall = 2, width = 3),sep="")) +
    dark_theme_gray()
  
}

DrawGraph(2.3)

param <- seq(0.01,15,length.out=(totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output3.mp4")

## Uniform distribution

DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dunif,
                  color="blue",
                  fill="blue",
                  alpha=0.5,
                  args=list(min = param[1], max=param[2]),
                  geom="area"
    ) +
    scale_x_continuous(limits = c(0, 20)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Uniform distribution on the interval [",format(param[1],digits=3, nsmall = 2, width = 3), ",",format(param[2],digits=3, width=3, nsmall =2),"]",sep="")) +
    dark_theme_gray()
  
}
DrawGraph(c(1,4))

bparam <- seq(4, 15, length.out=(totalframes+1)/2)
aparam <- seq(1, 12, length.out=(totalframes+1)/2-1)
n <- length(bparam)
m <- length(aparam)
bparam <- c(bparam,rep(15,m)) # b moves then holds still
aparam <- c(rep(1,n),aparam) # a holds still then moves # 202 frames

saveplotunif <- function() {
  for (i in 1:length(bparam)) {
    png(paste("Frame",sprintf("%06d", i),".png",sep=""),width=480)
    print(DrawGraph(c(aparam[i],bparam[i])))
    dev.off()
  }
}
saveplotunif()
system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output4.mp4")

## Normal Distribution
# Mean
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=-5:5))  + 
    stat_function(fun=dnorm,
                  color="red",
                  fill="red",
                  alpha=0.5,
                  args=list(mean=param, sd=1),
                  geom="area"
    ) +
    scale_x_continuous(limits = c(-5, 5)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Normal distribution with mean ",format(param,digits=3, nsmall = 2, width = 3), " and variance 1",sep="")) +
    dark_theme_gray()
  
}
DrawGraph(1)

param <- seq(-3, 3, length.out=(totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output5.mp4")

## Normal Distribution
# Variance
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=-5:5))  + 
    stat_function(fun=dnorm,
                  color="red",
                  fill="red",
                  alpha=0.5,
                  args=list(mean=0, sd=sqrt(param)),
                  geom="area"
    ) +
    scale_x_continuous(limits = c(-5, 5)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Normal distribution with mean 0 and variance ",format(param,digits=3, nsmall = 2, width = 3),sep="")) +
    dark_theme_gray()
  
}
DrawGraph(1)

param <- seq(0.4,3,length.out=(totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output6.mp4")


## Chi-square
DrawGraph <- function(param) {
  if (param == 1) {
    titletext <- " degree of freedom."
  } else {
  titletext <- " degrees of freedom."
  }
  
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dchisq,
                  color="blue",
                  fill="blue",
                  alpha=0.5,
                  args=list(df=param),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,1), xlim=c(0,20)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Chi-square distribution with ",format(param,width = 2),titletext, sep="")) +
    dark_theme_gray()
  
}
DrawGraph(15)

param <- 1:15
param <- rep(param,each = 10)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames


system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output7.mp4")

## Exponential Distribution
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dexp,
                  color="green",
                  fill="green",
                  alpha=0.5,
                  args=list(rate=param),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,1), xlim=c(0,20)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Exponential distribution with rate lambda = ",format(param,digits=3, nsmall = 2,width=3), sep="")) +
    dark_theme_gray()
  
}
DrawGraph(0.1)

param <- seq(0.1,3,length.out = (totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output8.mp4")

## Gamma Distribution
## Shape
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dgamma,
                  color="orange",
                  fill="orange",
                  alpha=0.5,
                  args=list(shape=param,scale = 3),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,1), xlim=c(0,20)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Gamma distribution with shape k = ",format(param,digits=3, nsmall = 2,width = 3), " and scale = 3", sep="")) +
    dark_theme_gray()
  
}
DrawGraph(0.1)

param <- seq(0.1,6, length.out = (totalframes+1)/2 )
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output9.mp4")

## Scale
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:20))  + 
    stat_function(fun=dgamma,
                  color="orange",
                  fill="orange",
                  alpha=0.5,
                  args=list(shape=2,scale = param),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,1), xlim=c(0,20)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Gamma distribution with shape k = 2 and scale = ",format(param, digits=3, nsmall=2, width = 3) , sep="")) +
    dark_theme_gray()
  
}
DrawGraph(3)

param <- seq(0.1,6,length.out = (totalframes+1)/2 )
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output10.mp4")


## Beta Distribution
## Alpha
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:1))  + 
    stat_function(fun=dbeta,
                  color="cornflowerblue",
                  fill="cornflowerblue",
                  alpha=0.5,
                  args=list(shape1=param, shape2 = 2),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,4), xlim=c(0,1)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Beta distribution with alpha = ",format(param,digits=3, nsmall = 2, width = 3), " and beta = 2", sep="")) +
    dark_theme_gray()
  
}
DrawGraph(10)

param <- seq(0.1,10,length.out = (totalframes+1)/2 )
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output11.mp4")


## Beta
DrawGraph <- function(param) {
  ggplot(mapping=aes(x=0:1))  + 
    stat_function(fun=dbeta,
                  color="cornflowerblue",
                  fill="cornflowerblue",
                  alpha=0.5,
                  args=list(shape1=2, shape2 = param),
                  geom="area"
    ) +
    coord_cartesian(ylim=c(0,4), xlim=c(0,1)) +
    #scale_x_continuous(limits = c(0, 5)) + 
    #scale_y_continuous(limits = c(0, 1)) + 
    xlab("x") +
    ylab("density") +
    ggtitle(paste("Beta distribution with alpha = 2 and beta = ",format(param,digits=3, nsmall = 2, width = 3), sep="")) +
    dark_theme_gray()
  
}
DrawGraph(2)

param <- seq(0.1,10,length.out = (totalframes+1)/2)
param <- c(param,rev(param))
saveplot(param[-length(param)]) # 299 frames

system("ffmpeg -i Frame%06d.png -vcodec libx264 -pix_fmt yuv420p Output12.mp4")


paste('ffmpeg',
       ' "-i Output1.mp4" -i "Output2.mp4" -i "Output3.mp4" -i "Output4.mp4" -i "Output5.mp4" -i "Output6.mp4" ',
        ' "-i Output7.mp4" -i "Output8.mp4" -i "Output9.mp4" -i "Output10.mp4" -i "Output11.mp4" -i "Output12.mp4" ',
      
'



