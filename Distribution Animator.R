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
    ggtitle(paste("Binomial distribution with 50 trials and probability of success p=",p,sep="")) +
    dark_theme_gray()

}

param <- seq(0,1,0.01)
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Binomial distribution with ",trials, " trials and probability of success p=0.3", sep="")) +
    dark_theme_gray()
  
}

DrawGraph(50)

param <- 1:150
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Poisson distribution with rate parameter lambda = ",lambda,sep="")) +
    dark_theme_gray()
  
}

DrawGraph(2.3)

param <- seq(0.01,15,0.05)
param <- c(param,rev(param))
saveplot(param[-length(param)])


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
    ylab("probability") +
    ggtitle(paste("Uniform distribution on the interval [",param[1], ",",param[2],"]",sep="")) +
    dark_theme_gray()
  
}
DrawGraph(c(1,4))

bparam <- seq(4,15,0.1)
aparam <- seq(1,10,0.1)
n <- length(bparam)
m <- length(aparam)
bparam <- c(bparam,rep(15,m)) # b moves then holds still
aparam <- c(rep(1,n),aparam) # a holds still then moves

saveplot <- function() {
  for (i in 1:length(bparam)) {
    png(paste("Frame",sprintf("%06d", i),".png",sep=""),width=480)
    print(DrawGraph(c(aparam[i],bparam[i])))
    dev.off()
  }
}
saveplot()

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
    ggtitle(paste("Normal distribution with mean ",format(param,digits=3), " and variance 1",sep="")) +
    dark_theme_gray()
  
}
DrawGraph(1)

param <- seq(-3,3,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Normal distribution with mean 0 and variance ",format(param,digits=3),sep="")) +
    dark_theme_gray()
  
}
DrawGraph(1)

param <- seq(0.4,3,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])


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
    ggtitle(paste("Chi-square distribution with ",param,titletext, sep="")) +
    dark_theme_gray()
  
}
DrawGraph(20)

param <- 1:20
param <- c(param,rev(param))
saveplot(param[-length(param)])


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
    ggtitle(paste("Exponential distribution with rate lambda = ",param, sep="")) +
    dark_theme_gray()
  
}
DrawGraph(0.1)

param <- seq(0.1,3,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Gamma distribution with shape k = ",format(param,digits=2, nsmall = 1), " and scale = 3", sep="")) +
    dark_theme_gray()
  
}
DrawGraph(0.1)

param <- seq(0.1,6,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Gamma distribution with shape k = 2 and scale = ",param, sep="")) +
    dark_theme_gray()
  
}
DrawGraph(3)

param <- seq(0.1,6,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])


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
    ggtitle(paste("Beta distribution with alpha = ",format(param,digits=2, nsmall = 1), " and beta = 2", sep="")) +
    dark_theme_gray()
  
}
DrawGraph(10)

param <- seq(0.1,10,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])

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
    ggtitle(paste("Beta distribution with alpha = 2 and beta = ",format(param,digits=2, nsmall = 1), sep="")) +
    dark_theme_gray()
  
}
DrawGraph(2)

param <- seq(0.1,10,0.1)
param <- c(param,rev(param))
saveplot(param[-length(param)])

