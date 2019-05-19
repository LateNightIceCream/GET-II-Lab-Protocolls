#Komplexe Zeiger von R, L, C

library(ggplot2)

options(scipen=10000)

output_width  <- 10
output_height <- output_width


ylabel <- "Im"
xlabel <- "Re"

Rcolor <- "#00BF7D"
Ccolor <- "#00B0F6"
Lcolor <- "#F8766D"

pdf("RLC_Zeiger_Impedanz.pdf", width = output_width, height = output_height)

####################################################################

p9 <- ggplot(data.frame(x=c(0, 2),y=c(0,2)), aes(x,y)) +

        theme_minimal() + # maybe theme_bw()

        ggtitle("Zeigerbild der Impedanzen von R (grün), L (rot) und C (blau) (nicht maßstäblich)") +

        theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=16.18), axis.text.y = element_text(size = 13.82, color = c(Ccolor, "grey60", Lcolor) )) + # rotate axis title
        theme(axis.title.x = element_text(angle = 0, hjust = 0.5, size=16.18), axis.text.x = element_text(size = 13.82, color = c("grey60", Rcolor))) +

        xlab(xlabel)  +
        ylab(ylabel)  +

        scale_y_continuous(limits=c(-1.382, 1.382), breaks = c(-1,0,1), labels=c(bquote("-" ~ over(1,omega ~ "C")), 0, bquote(omega ~ "L") ) ) +
        scale_x_continuous(limits=c(0, 1.382),  breaks = c(0,1), labels=c(0, "R" ))+

        #lines "#E58700"
        # L
        geom_segment( aes(x = 0, y = 0, xend = 0, yend = 1 ), color=Lcolor,   linetype="solid", arrow = arrow(length = unit(0.5, "cm")), size = 1.618)+
        # R
        geom_segment( aes(x = 0, y = 0, xend = 1, yend = 0 ), color=Rcolor,   linetype="solid", arrow = arrow(length = unit(0.5, "cm")), size = 1.618)+
        # C
        geom_segment( aes(x = 0, y = 0, xend = 0, yend = -1 ), color=Ccolor,  linetype="solid", arrow = arrow(length = unit(0.5, "cm")), size = 1.618)+

        annotate(geom="text", x=0.0618, y=1, label=bquote("90°"),   color=Lcolor, size=6.18) +
        annotate(geom="text", x=1, y=-0.1, label=bquote("0°"),   color=Rcolor, size=6.18) +
        annotate(geom="text", x=0.0618, y=-1, label=bquote("-90°"), color=Ccolor, size=6.18)

p9
