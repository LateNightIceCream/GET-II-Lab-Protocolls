#RC Tiefpassfilter

library(ggplot2)
library(RColorBrewer)

options(scipen=10000)

mod<-function(x,m)
{
    t1<-floor(x/m)
    return(x-t1*m)
}

epsilon <- 10
tau     <- 4
T1      <- tau * 1.6180339887498948

output_width  <- 10
output_height <- output_width * 0.6180339887498948


ylabel <- bquote( "x" )
xlabel <- bquote( "t" )


xbreaks <- c()
loops <- 5

for(n in -loops:loops) {
  xbreaks <- c(xbreaks, n*tau/2, n*T1/2)
}

function_color <- "red"
tempcolor      <- "blue"


####################################################################

p <- ggplot(data.frame(x=c(0,2), y=c(0,2)), aes(x=x)) +

        theme_minimal() + # maybe theme_bw()

        ggtitle("x(t)") +

        theme(axis.title.y = element_text(angle = 0, vjust = .5)) + # rotate axis title

        xlab(xlabel)  +
        ylab(ylabel)  +

        scale_y_continuous(limits=c(0, 1.6180339887498948), breaks=c(0,1, 1.6180339887498948), labels=c(0, "Xm", "")) +
        scale_x_continuous(limits=c(-epsilon, epsilon) , breaks=xbreaks) #labels=c(0, bquote("-"~over("T1",2)), bquote(over(-tau,2)), bquote(over(-tau,2)), bquote(over(tau,2))) )

delta <- T1

for (n in -1:1) {

  # solid horizontal lines
  p <- p + geom_segment( x = -tau/2 + n*delta, y = 1, xend = tau/2  + n*delta,  yend = 1, color=tempcolor, linetype="solid") +
      geom_segment( x = -T1/2  + n*delta, y = 0, xend = -tau/2 + n*delta,  yend = 0, color=tempcolor, linetype="solid") +
      geom_segment( x = T1/2   + n*delta, y = 0, xend = tau/2  + n*delta,  yend = 0, color=tempcolor, linetype="solid")

  # dashed vertical lines
  p <- p + geom_segment( x = -tau/2 + n*delta, y = 0, xend = -tau/2 + n*delta, yend = 1, color=function_color, linetype="dashed")+
           geom_segment( x =  tau/2 + n*delta, y = 0, xend = tau/2 + n*delta,  yend = 1, color=function_color, linetype="dashed")
}

pdf("2_1_function.pdf", width = output_width, height = output_height, )
p
