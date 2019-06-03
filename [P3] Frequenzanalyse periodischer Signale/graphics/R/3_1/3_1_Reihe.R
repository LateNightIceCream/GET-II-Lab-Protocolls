# square wave plot

library(ggplot2)

options(scipen=10000)

#/home/zamza/Documents/HS/Semester 2/GET II/Labs/[P3] Frequenzanalyse periodischer Signale
data <-read.csv(file = "/home/zamza/Documents/HS/Semester 2/GET II/Labs/[P3] Frequenzanalyse periodischer Signale/graphics/3_1_10_0125.csv")

print(data)

epsilon <- 20 * 10^(-6)
Xm      <- 1
D       <- 0.25
T1      <- 10 * 10^(-6)
tau     <- D * T1

deltax  <- 100

m <- 15

line_xcoordinates <- c()
line_ycoordinates <- c()

deltacounter <- 0

theoretical_ys <- c()


amplifun <- function(f) {

  2 * abs(Xm * D * sin(pi*f*D) / (f * pi * D))

}

phase <- function(l) {

  if(l == 1) {

    return(0)

  }

 for(k in 1:m) {

  if(l == (4*k+1)/(2*D)) {

    return(0)

  } else if(l == (4*k-1)/(2*D)) {

    return(pi)

  }
 }
}

for(i in 1:m) {

  deltacounter<-deltacounter+deltax
  line_xcoordinates <- c(line_xcoordinates, deltacounter)

  line_ycoordinates <- c(line_ycoordinates, data$n.amplitude[i])

  theoretical_ys <- c(theoretical_ys, amplifun(i)*1000)

}

print(line_ycoordinates)

# hardcoded bc i dont have time, i sometimes hate R
# very hackish and ugly but does the job :D
wave1 <- function(t) {
  Xm/2 + amplifun(1) * cos(2*pi/T1*t + phase(1))
}
wave2 <- function(t) {
  Xm/2 +  amplifun(2)* cos(2*2*pi/T1*t + phase(2))
}
wave3 <- function(t) {
  Xm/2 +amplifun(3) * cos(3*2*pi/T1*t + phase(3))
}
wave4 <- function(t) {
  Xm/2 +amplifun(4) * cos(4*2*pi/T1*t + phase(4))
}
wave5 <- function(t) {
  Xm/2 +amplifun(5) * cos(5*2*pi/T1*t + + phase(5))
}
wave6 <- function(t) {
  Xm/2 +amplifun(6) * cos(6*2*pi/T1*t + phase(6))
}
wave7 <- function(t) {
  Xm/2 +amplifun(7) * cos(7*2*pi/T1*t+ phase(7))
}
wave8 <- function(t) {
  Xm/2 +amplifun(8) * cos(8*2*pi/T1*t+ phase(8))
}
wave9 <- function(t) {
  Xm/2 +amplifun(9) * cos(9*2*pi/T1*t + phase(9))
}
wave10 <- function(t) {
  Xm/2 +amplifun(10) * cos(10*2*pi/T1*t + phase(10))
}
wave11 <- function(t) {
  Xm/2 +amplifun(11) * cos(11*2*pi/T1*t+ phase(11))
}
wave12 <- function(t) {
  Xm/2 +amplifun(12) * cos(12*2*pi/T1*t+ phase(12))
}
wave13 <- function(t) {
  Xm/2 +amplifun(13) * cos(13*2*pi/T1*t+ phase(13))
}
wave14 <- function(t) {
  Xm/2 +amplifun(14) * cos(14*2*pi/T1*t+ phase(14))
}
wave15 <- function(t) {
  Xm/2 +amplifun(15) * cos(15*2*pi/T1*t+ phase(15))
}

totalwave <- function(t) {

   -Xm/2 * 7 + wave1(t) + wave3(t) + wave5(t) + wave7(t) + wave9(t) + wave11(t) + wave13(t) + wave15(t)

}

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
        ggtitle(bquote("Amplitudenspektrum von x(t), Rot: theoretische Werte")) +
        labs(subtitle="D=0.25")+

        #stat_function(fun = wave1, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave2, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave3, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave4, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave5, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave6, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave7, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave8, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave9, colour   = indie_wave_color, n=2000)+
        #stat_function(fun = wave10, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = wave11, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = wave12, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = wave13, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = wave14, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = wave15, colour  = indie_wave_color, n=2000)+
        #stat_function(fun = totalwave, colour  = function_color, n=2000)+

        theme(text = element_text(size=16.180339887498948), axis.title.y = element_text(angle = 0, vjust = .5), panel.grid.minor.x = element_blank()) + # rotate axis title

        xlab(xlabel) +
        ylab(ylabel)  +

        scale_y_continuous(limits=c(0, 700), breaks=seq(0,700, 100)) +
        scale_x_continuous(limits=c(0, 1500), breaks=seq(0, 1500, 100))

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

pdf("3_1_ASpektrum_0125.pdf", width = output_width, height = output_height, )
p
