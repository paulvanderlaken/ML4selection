


# AUTOMATED SELECTION SIMULATION
library(MASS)
library(ggplot2)
library(dplyr)

# library(devtools)
# install_github("dgrtwo/gganimate")
# library(gganimate)

# set wd
setwd("C:/Users/u1235656/stack/PhD/Dissertatie/selection_evolution")

# set options
options(scipen = 999)
theme_set(theme_light())

# set overall seed
set.seed(1)

# determine population size
pop_n = 100000
org_n = 1000
applicant_n = 1000
attrition_rate = 0.10

# determine means
true_means = c(0, 0)
true_cov = matrix(c(1, 0.4,
                    0.4, 1), nrow = 2)

# simulate population data
pop = mvrnorm(n = pop_n, mu = true_means, Sigma = true_cov, empirical = TRUE)

# retrieve random organizational population
org_rows <- sample(1:nrow(pop), org_n)
org <- pop[org_rows,]
pop <- pop[-org_rows,]

# determine number of years 
years = 100

for(year in seq(years)){
  
  print(year)
  # simulate new population
  pop = mvrnorm(n = pop_n, mu = true_means, Sigma = true_cov, empirical = TRUE)
  
  # retrieve regression coefficients
  lm_res1 <- lm(org[,1] ~ org[,2])
  
  # visualize relationship
  p <- ggplot(data.frame(org), aes(x = X2, y = X1)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
    labs(title = paste0("Organization after ", year," iterations"),
         subtitle = paste0("Correlation = ", round(lm_res1$coefficients[2], 3)),
         y = "Performance", 
         x = "Selection Test Score")
  
  # save plot
  ggsave(filename = paste0("images/scatterplot_org_pop_", year, ".png"),
         plot = p)
  
  # simulate 10% leavers
  org_leavers_rows <- sample(1:nrow(org), org_n*attrition_rate)
  org <- org[-org_leavers_rows,]
  
  # retrieve applicants
  applicant_rows <- sample(1:nrow(pop), applicant_n)
  # assess applicant estimated future performance
  predicted_performance <- pop[applicant_rows, 2] * lm_res1$coefficients[2]
  # determine top estimated performers
  top_applicants <- order(predicted_performance, decreasing = T)[1:(org_n*attrition_rate)]
  
  # select top estimated performers
  org <- rbind(org, pop[applicant_rows[top_applicants], ])
  pop <- pop[-applicant_rows[top_applicants], ]
  
}