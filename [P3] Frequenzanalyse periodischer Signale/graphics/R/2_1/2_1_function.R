#RC Tiefpassfilter

library(ggplot2)
library(RColorBrewer)

options(scipen=10000)

breaks       <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

#ybreaks = c(seq(0,1.0,0.1),round(1/sqrt(2),digits = 3));
#ybreaks = ybreaks[-8] # ybreaks without 0.7 (8th element)

output_width  <- 10
output_height <- output_width * 0.6180339887498948

epsilon <- 10

R  <- 1000;
RP <- 1000;
L  <- 0.001;
C  <- 0.000001;


fga <- (sqrt( ((RP+R)/(2*RP*R*C))^2 + 1/(L*C)) + (RP+R)/(2*RP*R*C) ) / (2*pi)
fgb <- (sqrt( ((RP+R)/(2*RP*R*C))^2 + 1/(L*C)) - (RP+R)/(2*RP*R*C) ) / (2*pi)

print(fga)
print(fgb)

ylabel <- bquote( over( "|" ~ underline("U")[2] ~ "|"  , "|" ~ underline("U")[1] ~ "|") )
xlabel <- bquote( over( "f", "f"[g]))

rc_fun<- function(f) {1 / sqrt( 4 + (R*2*pi*f * C - R/(2*pi*f * L) )^2 ) }

pdf("2_7_betrag_clean.pdf", width = output_width, height = output_height, )

####################################################################

p <- ggplot(data.frame(x=c(0,2), y=c(0,2)), aes(x=x)) +

        theme_minimal() + # maybe theme_bw()
        scale_colour_brewer(palette = "Set2") +

        ggtitle("Betragsgang") +

        stat_function(fun = rc_fun, colour = "dodgerblue3")+

        theme(axis.title.y = element_text(angle = 0, vjust = .5)) + # rotate axis title

        #labs(title="", subtitle="", y="", x="", caption="Midwest Demographics")+

        xlab(xlabel)  +
        ylab(ylabel)  +

        scale_y_continuous(limits=c(0, 1), breaks=c(0,1), labels=c(0, "Xm")) +
        scale_x_continuous(limits=c(-epsilon, epsilon) ) +

        #lines
        #geom_segment( aes(x = 4500, y = 1/(2*sqrt(2)),  xend = fga, yend = 1/(2*sqrt(2)) ), color="grey69", linetype="dashed") +
        geom_segment( aes(x = 1/(2*pi*sqrt(L*C)),    y = 0,          xend = 1/(2*pi*sqrt(L*C)), yend = 0.5 ), color="grey69", linetype="dashed") +
        geom_segment( aes(x = fgb,    y = 0,          xend = fgb, yend = 1/(2*sqrt(2)) ), color="grey69", linetype="dashed") +
        geom_segment( aes(x = fga,    y = 0,          xend = fga, yend = 1/(2*sqrt(2)) ), color="grey69", linetype="dashed")

p
