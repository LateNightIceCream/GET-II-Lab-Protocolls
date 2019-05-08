library(ggplot2)

options(scipen=10000)

breaks       <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

ybreaks = c(seq(0,1.0,0.1),round(1/sqrt(2),digits = 3));
ybreaks = ybreaks[-8] # ybreaks without 0.7 (8th element)

output_width  <- 10
output_height <- output_width * 0.6180339887498948

data <- read.csv(file = "RC_TP.csv")

dataU1    <- data$u1
dataU2    <- data$u2
dataPhase <- data$phase
dataX     <- data$ffg
dataY     <- dataU1/dataU2



ylabel <- bquote( over( "|" ~ underline("U")[2] ~ "|"  , "|" ~ underline("U")[1] ~ "|") )
xlabel <- bquote( over( "f", "f"[g]))

rc_fun<- function(f) {1 / sqrt(1 + (f)^2)}

pdf("RC_LP.pdf", width = output_width, height = output_height, )

####################################################################

p9 <- ggplot(data.frame(x=dataX, y=dataU1), aes(dataX, dataU2)) +

        theme_minimal() + # maybe theme_bw()

        ggtitle("Betragsgang: RC - Tiefpassfilter") +

        stat_function(aes(dataX, dataU2), fun = rc_fun, colour = "dodgerblue3")+

        geom_point(aes(dataX , dataU2/dataU1), colour="deeppink1", shape="plus", size=2) +

        theme(axis.title.y = element_text(angle = 0, vjust = .5)) +


        xlab(xlabel)  +
        ylab(ylabel)    +

        scale_y_continuous(limits=c(0, 1), breaks = ybreaks) +
        scale_x_continuous(trans = "log10", breaks = breaks, minor_breaks = minor_breaks, limits=c(0.01, 100))+

        annotation_logticks(sides = "b", colour = "grey69", size = .333) # maybe remove


p9
