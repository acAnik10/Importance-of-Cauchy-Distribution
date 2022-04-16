#=============================
# DISSERTATION: Simulation_02
#=============================
library(ggplot2)
library(dplyr)

# Theme ----
theme_new = theme(plot.title = element_text(size = 22, 
                                            face = "bold", 
                                            hjust = 0.5), 
                  legend.title = element_text(size = 15, 
                                              face = "bold"), 
                  plot.subtitle = element_text(size = 19, 
                                               hjust = 0.5), 
                  plot.caption = element_text(size = 14), 
                  plot.margin = margin(1, 1.5, 0.7, 0.7, "cm"), 
                  legend.text = element_text(size = 13), 
                  axis.title = element_text(size = 15, 
                                            face = "bold"), 
                  axis.text = element_text(size = 13, 
                                           face = "bold"), 
                  strip.background = element_blank(),
                  strip.placement = "outside", 
                  strip.text = element_text(size = 16, 
                                            color = 1, 
                                            face = "bold"))

theme_set(theme_light() + theme_new)

#------------------------------------------------------------

# Sample Mean vs Sample Median
#------------------------------
cauchy = function(size_max, mu)
{
     n = 1:size_max
     sigma = c(1, 4, 10)
     k = length(sigma)
     s = 100
     
     # storage of probability
     data = vector(mode = "list", length = k)
     names(data) = seq_along(sigma)
     
     for (i in 1:k)
     {
          mean_med = function(size)
          {
               set.seed(s)
               y = rcauchy(size, mu, sigma[i])
               
               m = mean(y)
               med = median(y)
               
               return(c(m, med))
          }
          
          est = matrix(0, size_max, 2)
          colnames(est) = c("Sample Mean", "Sample Median")
          
          for (j in n)
               est[j,] = mean_med(j)
          
          data[[i]] = est
     }
     
     data = list2DF(data) %>% 
          mutate(size = rep(n, 2), 
                 estimator = rep(colnames(est), 
                                 each = size_max) %>% 
                      factor()) %>% 
          tidyr::pivot_longer(-c(estimator, size), "sigma") %>% 
          mutate(sigma = factor(sigma))
     
     # levels(data$sigma) = paste("Scale =", sigma)
     sgm_names = c(`1` = "sigma==1", 
                   `2` = "sigma==4", 
                   `3` = "sigma==10")
     
     data %>% 
          ggplot(aes(size, value)) +
          geom_point(size = 1.2) + 
          labs(title = "Consistency for Cauchy Distribution", 
               subtitle = bquote(mu == .(mu)), 
               x = "\nSample Size", 
               y = "Estimator\n") + 
          geom_segment(aes(0, mu, xend = size_max, yend = mu),
                       col = "red3", lwd = 1) + 
          ylim(mu-10, mu+10) + 
          facet_grid(sigma ~ estimator, 
                     labeller = labeller(sigma = as_labeller(sgm_names, 
                                                             label_parsed)))
}

cauchy(300, 0)

ggsave("Sim_02.jpeg", width = 17, height = 10, units = "in")

#-------------------------------------------------------------
