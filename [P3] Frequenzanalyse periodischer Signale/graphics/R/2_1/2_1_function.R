# square wave plot

library(ggplot2)

options(scipen=10000)

epsilon <- 10
tau     <- 4
T1      <- tau * 1.6180339887498948
Xm      <- 1 #1.381966011

output_width  <- 10
output_height <- output_width * 0.6180339887498948


ylabel <- bquote( "x" )
xlabel <- bquote( "t" )


xbreaks <- c(0, -tau/2, tau/2, -T1/2, T1/2, -T1, T1)
xlabels <- c(0, bquote("-"~over(tau, 2)), bquote(over(tau, 2)), bquote("-"~over("T"[1], 2)), bquote(over("T"[1], 2)), bquote("-T"[1]), bquote("T"[1]))

#for(n in 1:3) {
  #xbreaks <- c(xbreaks, n*T1)
  #xlabels <- c(xlabels, bquote(.(n)~T))
#}

function_color <- "#0CB5DF"


####################################################################

p <- ggplot(data.frame(x=c(0,2), y=c(0,2)), aes(x=x)) +

        theme_minimal() + # maybe theme_bw()
        theme(legend.position="none", plot.title = element_text(color = "gray21", size=1.6180339887498948^5), plot.subtitle = element_text(color = "grey80", size=1.6180339887498948^5)) +
        ggtitle("x(t)") +

        theme(text = element_text(size=16.180339887498948), axis.title.y = element_text(angle = 0, vjust = .5), panel.grid.minor.x = element_blank()) + # rotate axis title

        xlab(xlabel) +
        #ylab(ylabel)  +

        scale_y_continuous(limits=c(0, 1.6180339887498948), breaks=c(0,Xm, 1.6180339887498948), labels=c(0, "Xm", "")) +
        scale_x_continuous(limits=c(-epsilon, epsilon), breaks=xbreaks, labels=xlabels)
        #scale_x_discrete(limits=1, breaks=c(1,2,3)) #labels=c(0, bquote("-"~over("T1",2)), bquote(over(-tau,2)), bquote(over(-tau,2)), bquote(over(tau,2))) )

delta <- T1

for (n in -1:1) {

  # solid horizontal lines
  p <- p + geom_segment( x = -tau/2 + n*delta, y = Xm, xend = tau/2  + n*delta,  yend = Xm, color=function_color, linetype="solid") +
      geom_segment( x = -T1/2  + n*delta, y = 0, xend = -tau/2 + n*delta,  yend = 0, color=function_color, linetype="solid") +
      geom_segment( x = T1/2   + n*delta, y = 0, xend = tau/2  + n*delta,  yend = 0, color=function_color, linetype="solid")

  # test segment
  #p <- p + geom_segment( x = T1/2, y = 0, xend = T1/2-tau/2 + T1/2, yend = 0, aes(color="blue"), linetype="solid")

  # dashed vertical lines
  p <- p + geom_segment( x = -tau/2 + n*delta, y = 0, xend = -tau/2 + n*delta, yend = Xm, color=function_color, linetype="dashed")+
           geom_segment( x =  tau/2 + n*delta, y = 0, xend = tau/2 + n*delta,  yend = Xm, color=function_color, linetype="dashed")
}

pdf("2_1_function.pdf", width = output_width, height = output_height, )
p
