# square wave plot

library(ggplot2)

options(scipen=10000)

#/home/zamza/Documents/HS/Semester 2/GET II/Labs/[P3] Frequenzanalyse periodischer Signale
data <-read.csv(file = "/home/zamza/Documents/HS/Semester 2/GET II/Labs/[P3] Frequenzanalyse periodischer Signale/graphics/3_3.csv")

print(data)

epsilon <- 20 * 10^(-6)
Xm      <- 1
D       <- 0.25
T1      <- 10^(-3)
R1      <- 400
R2      <- 1000
C       <- 10^(-6)
tau     <- D * T1

deltax  <- 1

m <- 15

line_xcoordinates <- data$Fn..kHz
line_ycoordinates <- data$Xamp
theoretical_ys    <- data$Xamptheo


#print(line_ycoordinates)

output_width  <- 10
output_height <- output_width * 0.6180339887498948

ylabel <- bquote( "U / mV" )
xlabel <- bquote( "f / kHz" )


xbreaks <- c(0, -tau/2, tau/2, -T1/2, T1/2, -T1, T1)
xlabels <- c(0, bquote("-"~over(tau, 2)), bquote(over(tau, 2)), bquote("-"~over("T"[1], 2)), bquote(over("T"[1], 2)), bquote("-T"[1]), bquote("T"[1]))

function_color <- "#0CB5DF"
indie_wave_color <- "#0CB5DF22"
other_color <- "#FF7C96"

#df <- data.frame(x=xsteps, y=wave1)
####################################################################

p <- ggplot(data.frame(x=c(0,2), y=c(0,2)), aes(x=x)) +

        theme_minimal() + # maybe theme_bw()
        theme(legend.position="none", plot.title = element_text(color = "gray21", size=1.6180339887498948^5), plot.subtitle = element_text(color = "grey80", size=1.6180339887498948^5)) +
        ggtitle(bquote("Amplitudenspektrum von U"[2]~"(t), Rot: theoretische Werte"))+
        #labs(subtitle="D=0.25, T=1us")+

        theme(text = element_text(size=16.180339887498948), axis.title.y = element_text(angle = 0, vjust = .5), panel.grid.minor.x = element_blank()) + # rotate axis title

        xlab(xlabel) +
        ylab(ylabel) +

        scale_y_continuous(limits=c(0, 250), breaks=seq(0,250, 100)) +
        scale_x_continuous(limits=c(0, 15), breaks=seq(0, 15,  1))

        #geom_segment( aes(x = line_xcoordinates[1], y = 0, xend = line_xcoordinates[1], yend = line_ycoordinates[1] ), color=function_color,   linetype="solid", size = 1.618)
        #scale_x_discrete(limits=1, breaks=c(1,2,3)) #labels=c(0, bquote("-"~over("T1",2)), bquote(over(-tau,2)), bquote(over(-tau,2)), bquote(over(tau,2))) )

linewidth <- (1-0.6180339887498948)*10

for( i in 1:m ) {

  if(!is.na(line_ycoordinates[i])) {

  if(theoretical_ys[i] > line_ycoordinates[i]) {

    p <- p + geom_segment( x = line_xcoordinates[i], y = 0, xend = line_xcoordinates[i], yend = theoretical_ys[i] , color=other_color,   linetype="solid", size = linewidth)
    p <- p + geom_segment( x = line_xcoordinates[i], y = 0, xend = line_xcoordinates[i], yend = line_ycoordinates[i] , color=function_color,   linetype="solid", size = linewidth)


  } else {

    p <- p + geom_segment( x = line_xcoordinates[i], y = 0, xend = line_xcoordinates[i], yend = line_ycoordinates[i] , color=function_color,   linetype="solid", size = linewidth)
    p <- p + geom_segment( x = line_xcoordinates[i], y = 0, xend = line_xcoordinates[i], yend = theoretical_ys[i] , color=other_color,   linetype="solid", size = linewidth)

  }
 }
}

print(theoretical_ys)

pdf("3_3_ASpektrum.pdf", width = output_width, height = output_height, )
p
