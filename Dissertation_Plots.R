#===============================
# DISSERATION: Graphics & Plots
#===============================
setwd("E:/St. Xavier's College/DISSERTATION")
getwd()

library(ggplot2)
library(dplyr)
library(latex2exp)
library(truncdist)

# Theme
#-------
theme_new = theme(plot.title = element_text(size = 22, 
                                            face = "bold", 
                                            hjust = 0.5), 
                  plot.subtitle = element_text(size = 20, 
                                               hjust = 0.5), 
                  plot.caption = element_text(size = 14, 
                                              face = "bold"), 
                  plot.margin = margin(1, 0.7, 0.7, 0.7, "cm"), 
                  legend.title = element_text(size = 17, 
                                              face = "bold"), 
                  legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 15, 
                                            face = "bold"), 
                  axis.text = element_text(size = 13, 
                                           face = "bold"))

theme_set(theme_light() + theme_new)

#-----------------------------------------------------------

# Different Functional Form
#------------------------------
f = function(x, a, k) k/(x^2 + a^2)

ggplot() + 
     stat_function(fun = f, args = list(a = 1, k = 1), 
                   lwd = 2, aes(col = "a = 1, k = 1")) + 
     stat_function(fun = f, args = list(a = 0.5, k = 0.3), 
                   lwd = 2, aes(col = "a = 0.5, k = 0.3")) + 
     stat_function(fun = f, args = list(a = 1.5, k = 2), 
                   lwd = 2, aes(col = "a = 1.5, k = 2")) + 
     labs(title = TeX(r"(f(x) = $k\cdot\frac{1}{x^2+a^2}$)", 
                      bold = T), 
          col = "", x = "\nx", y = "f(x)\n") + 
     xlim(-8, 8) + 
     theme_classic() + theme_new + 
     theme(legend.position = c(0.85, 0.9), 
           legend.text = element_text(size = 20))

ggsave("01_genesis.jpeg", width = 17, height = 10, units = "in")

#-------------------------------------------------------------

# PDF 
#-----
ggplot() + 
     stat_function(fun = dcauchy, args = list(0, 0.5), 
                   aes(col = "1"), lwd = 2.5) + 
     stat_function(fun = dcauchy, args = list(0, 1), 
                   aes(col = "2"), lwd = 2.5) + 
     stat_function(fun = dcauchy, args = list(0, 2), 
                   aes(col = "3"), lwd = 2.5) + 
     stat_function(fun = dcauchy, args = list(1, 1), 
                   aes(col = "4"), lwd = 2.5) + 
     labs(title = "Probability Density Function", 
          x = "\nVariable", y = "Density\n", 
          col = "") + 
     xlim(-5, 5) + 
     scale_color_discrete(labels = c(bquote(mu == 0 ~","~ 
                                            sigma == 0.5), 
                                     bquote(mu == 0 ~","~ 
                                            sigma == 1), 
                                     bquote(mu == 0 ~","~ 
                                            sigma == 2), 
                                     bquote(mu == 1 ~","~ 
                                            sigma == 1))) + 
     theme(legend.position = c(0.85, 0.9), 
           legend.text = element_text(size = 20))

ggsave("02_PDF.jpeg", width = 17, height = 10, units = "in")

# CDF
#-----
ggplot() + 
     stat_function(fun = pcauchy, args = list(0, 0.5), 
                   aes(col = "1"), lwd = 3) + 
     stat_function(fun = pcauchy, args = list(0, 1), 
                   aes(col = "2"), lwd = 3) + 
     stat_function(fun = pcauchy, args = list(0, 2), 
                   aes(col = "3"), lwd = 3) + 
     stat_function(fun = pcauchy, args = list(1, 1), 
                   aes(col = "4"), lwd = 3) + 
     labs(title = "Cumulative Distribution Function", 
          x = "\nVariable", y = "Cumulative Probability\n", 
          col = "") + 
     xlim(-4, 4) + 
     scale_color_discrete(labels = c(bquote(mu == 0 ~","~ 
                                            sigma == 0.5), 
                                     bquote(mu == 0 ~","~ 
                                            sigma == 1), 
                                     bquote(mu == 0 ~","~ 
                                            sigma == 2), 
                                     bquote(mu == 1 ~","~ 
                                            sigma == 1))) + 
     theme(legend.position = c(0.85, 0.4), 
           legend.text = element_text(size = 20))

ggsave("03_CDF.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# Nature of the curve 
#---------------------
ggplot() + 
     stat_function(fun = dcauchy, col = 4, lwd = 2.5) + 
     labs(title = TeX(r"(Cauchy ($\mu = 0,\,\sigma = 1$))", 
                      bold = T), 
          x = "\nx", y = "f(x)\n") + 
     xlim(-5, 5) + 
     geom_segment(aes(0, 0, xend = 0, yend = 1/pi), 
                  col = "red3", lty = 2, lwd = 2) + 
     annotate("text", x = 0, y = 0.02 + 1/pi, 
                   label = TeX(r"($\mu = 0$)"), 
                   parse = T, size = 8) + 
     coord_cartesian(y = c(0, 0.35), clip = "off") + 
     theme_classic() + theme_new

ggsave("04_NoC.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# Truncated Distribution
#------------------------
ggplot() + 
     stat_function(fun = dtrunc, lwd = 2.5, 
                   aes(col = "Truncated Cauchy"), 
                   args = list("cauchy", -5, 5)) + 
     stat_function(fun = dcauchy, lwd = 2.5, lty = 2, 
                   aes(col = "Standard Cauchy")) + 
     geom_segment(aes(5*c(-1, 1), 0, xend = 5*c(-1, 1), 
                      yend = 0.38), 
                  col = "red3", lty = 2, lwd = 2) + 
     annotate("text", x = 5*c(-1, 1), y = 0.4, 
                   label = paste("k ==", 5*c(-1, 1)), 
                   parse = T, size = 8) + 
     coord_cartesian(y = c(0, 0.4), clip = "off") + 
     scale_color_manual(values = c(4, 7)) + 
     xlim(-10, 10) + 
     labs(title = "Truncated Cauchy Distribution", 
          subtitle = "Support: (-5, 5)", 
          x = "\nVariable", y = "Density\n", 
          col = "") + 
     theme_classic() + theme_new + 
     theme(legend.position = c(0.9, 0.9), 
           legend.text = element_text(size = 20))

ggsave("05_TrC.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# Points of Inflection
#----------------------
ggplot() + 
     stat_function(fun = dcauchy, col = 4, lwd = 2.5) + 
     geom_segment(aes(1/sqrt(3)*c(-1, 1), 0, 
                      xend = 1/sqrt(3)*c(-1, 1), 
                      yend = 0.31), 
                  col = "red3", lty = 2, lwd = 2) + 
     annotate("text", x = 1/sqrt(3)*c(-1, 1), y = 0.33, 
                   label = c(bquote(mu-frac(sigma, sqrt(3))==-0.577), bquote(mu+frac(sigma, sqrt(3))==0.577)), 
                   parse = T, size = 6) + 
     coord_cartesian(y = c(0, 0.35), clip = "off") + 
     xlim(4*c(-1, 1)) + 
     labs(title = "Points of Inflection", 
          subtitle = TeX(r"(Cauchy ($\mu = 0,\,\sigma = 1$))"), 
          x = "\nVariable", y = "Density\n") + 
     theme_classic() + theme_new

ggsave("06_PoI.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# X and 1/X: Cauchy(0,1)
#-----------------------
x = rcauchy(1000)

inv = function(data, fill_col)
{
     ggplot() + 
     geom_histogram(aes(data, y = ..density..), col = 1, 
                    fill = fill_col, bins = 20) + 
     stat_function(fun = dcauchy, lwd = 2, 
                   aes(col = "Standard Cauchy Curve")) + 
     scale_color_manual(values = 1) + 
     xlim(10*c(-1, 1)) + 
     labs(x = "\nVariable", y = "Density\n", col = "") + 
     theme(legend.text = element_text(size = 22))
}

p1 = inv(x, "#FFD124") + ggtitle("Distribution of X",
                                 subtitle = "")
p2 = inv(1/x, 4) + 
     ggtitle(TeX(r"(Distribution of $\,\frac{1}{X}$)", bold = T))

ggpubr::ggarrange(p1, p2, common.legend = T, legend = "bottom") 

ggsave("07_Inv.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# X+Y identically distributed with 2X
#-------------------------------------
x = rcauchy(1000); y = rcauchy(1000)

idt = function(data, fill_col)
{
     ggplot() + 
     geom_histogram(aes(data, y = ..density..), col = 1, 
                    fill = fill_col, bins = 20) + 
     stat_function(fun = dcauchy, lwd = 2, 
                   args = list(scale = 2), 
                   aes(col = "Cauchy(0,2) Curve")) + 
     scale_color_manual(values = 1) + 
     xlim(20*c(-1, 1)) + 
     labs(x = "\nVariable", y = "Density\n", col = "") + 
     theme(legend.text = element_text(size = 22))
}

p1 = idt(x+y, "#FFD124") + ggtitle("Distribution of X+Y")
p2 = idt(2*x, 4) + ggtitle("Distribution of 2X")

ggpubr::ggarrange(p1, p2, common.legend = T, legend = "bottom") 

ggsave("08_Idt.jpeg", width = 17, height = 10, units = "in")

#------------------------------------------------------------

# Cauchy and Normal Peaks
#-------------------------
ggplot() + 
     stat_function(fun = dcauchy, lwd = 2, 
                   aes(col = "Cauchy (0,1)")) + 
     stat_function(fun = dnorm, lwd = 2, lty = 2, 
                   aes(col = "Normal (0,1)")) + 
     scale_color_manual(values = c(4, "#FFD124")) + 
     labs(title = "Cauchy and Normal", 
          x = "\nVariable", y = "Density\n", col = "") + 
     geom_vline(xintercept = 0, lwd = 1.6, lty = 3, 
                col = "red3") + 
     xlim(5*c(-1, 1)) + 
     theme_classic() + theme_new + 
     theme(legend.position = c(0.9, 0.9), 
           legend.text = element_text(size = 20))

ggsave("09_CN.jpeg", width = 17, height = 10, units = "in")

#-------------------------------------------------------------