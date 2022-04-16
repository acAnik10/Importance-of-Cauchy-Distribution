#=============================
# DISSERTATION: Simulation_01
#=============================
# Cauchy and Normal
#-------------------
emp_lvl = function(sizes, mu0 = 0, los = 0.05)
{
     # Specifications of the argument
     # mu0: Hypothesized value of the location parameter
     # los: level of significance
     
     # Testing -> H0: mu = mu0 against H1: mu != mu0
     # Cauchy(mu,1) vs Normal(mu,1)
     
     R = 1000                     # simulation number
     n = sizes                    # sample sizes
     sigma = 1                    # Scale parameter
     
     # Storing Empirical Levels
     emp.level = data.frame(Sample_Size = 0, Cauchy = 0, Normal = 0)
     
     for (i in seq_along(n))
     {
          # Drawing Samples
          x.c = replicate(R, mean(rcauchy(n[i], mu0, sigma)))   # Cauchy
          x.n = replicate(R, mean(rnorm(n[i], mu0, sigma)))     # Normal
          
          # Test Statistic (Normal theory based)
          ts.c = sqrt(n[i])*(x.c - mu0)/sigma   # Cauchy
          ts.n = sqrt(n[i])*(x.n - mu0)/sigma   # Normal
          
          # Critical Point
          crit = qnorm(los/2, lower.tail = F)
          
          # Empirical Levels
          emp.c = mean(abs(ts.c) > crit)
          emp.n = mean(abs(ts.n) > crit)
          
          emp.level[i,] = c(n[i], emp.c, emp.n)
     }
     
     return(emp.level)
}

# Choosing Sample Sizes
n = c(5, 10, 25, 50, 100, 200, 500, 1000)

# Simulation Results
set.seed(2022)                # Initializing the seed at '2022'
output = emp_lvl(n); output   # Output of the Program

#==============================================================