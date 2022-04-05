#===============================
# DISSERATION: Graphics & Plots
#===============================
library(ggplot2)
library(latex2exp)

theme_set(theme_light())

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

#-----------------------------------------------------------

# Introduction Image
#--------------------
x = rcauchy(1000)

ggplot() + 
     geom_histogram(aes(x, ..density..), 
                    bins = 80, col = 1, fill = 4) + 
     xlim(-10, 10) + 
     stat_function(fun = dcauchy, lwd = 1.5) + 
     theme_void()

ggsave("00_Intro.jpeg", 
       path = "E:/St. Xavier's College/DISSERTATION", 
       width = 15, height = 10, units = "in")

#------------------------------------------------------------

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

ggsave("01_genesis.jpeg", 
       path = "E:/St. Xavier's College/DISSERTATION", 
       width = 17, height = 10, units = "in")

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
     theme_new + 
     theme(legend.position = c(0.85, 0.9), 
           legend.text = element_text(size = 20))

ggsave("02_PDF.jpeg", 
       path = "E:/St. Xavier's College/DISSERTATION", 
       width = 17, height = 10, units = "in")

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
     theme_new + 
     theme(legend.position = c(0.85, 0.4), 
           legend.text = element_text(size = 20))

ggsave("03_CDF.jpeg", 
       path = "E:/St. Xavier's College/DISSERTATION", 
       width = 17, height = 10, units = "in")

#------------------------------------------------------------