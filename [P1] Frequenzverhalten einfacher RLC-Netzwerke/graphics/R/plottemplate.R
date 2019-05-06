library(ggplot2)

options(scipen=10000)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

ybreaks = c(seq(0,1.0,0.1),round(1/sqrt(2),digits = 3));
ybreaks = ybreaks[-8] # ybreaks without 0.7 (8th element)

#data <- read.csv(file = "31.csv")

#dataX <- data$x/100
#dataB <- data$B/1000

#dat <- data.frame(x=dataX, y=dataB)

#C <- 0.001;
#R <- 1;

#fg <- 1 / (2 * pi * R* C)

rc_fun<- function(f) {1 / sqrt(1 + (f)^2)}

pdf("RC_LP.pdf")

p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = rc_fun, colour = "dodgerblue3")+
        #geom_point(aes(dataX,dataB),  colour="deeppink1", shape="plus", size=2) +
        #geom_point(aes(-dataX,dataB),  colour="deeppink1", shape="plus", size=2) +

        xlab("log(f / fg)") +
        ylab("U2 / U1") +
        scale_y_continuous(limits=c(0, 1), breaks = ybreaks) +
        scale_x_continuous(trans = "log10", breaks = breaks, minor_breaks = minor_breaks, limits=c(0.001, 1000))+
        annotation_logticks(sides = "b", colour = "grey69")

p9 <- p9 + theme_bw()
p9
