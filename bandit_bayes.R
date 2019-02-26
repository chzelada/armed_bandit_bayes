library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

bayes_example1 <- function(clicks=1,
                           impressions=1,
                           n_samples=1000,
                           prior,
                           proportion_clicks){
  n_visitors <- seq(0, impressions, by = 1)
  pars <- expand.grid(proportion_clicks = proportion_clicks,
                      n_visitors = n_visitors)
  
  pars$prior <- prior
  n_visitors <- rbinom(n = n_samples, 
                       size = impressions, 
                       prob = proportion_clicks)
  pars$likelihood <- dbinom(pars$n_visitors, 
                            size = impressions, 
                            prob = pars$proportion_clicks)
  pars$probability <- pars$likelihood * pars$prior
  pars$probability <- pars$probability / sum(pars$probability)
  pars <- pars[pars$n_visitors ==clicks, ]
  pars$probability <- pars$probability / sum(pars$probability)
  prior_out <- data_frame(proportion_clicks=pars$proportion_clicks,
                          probability=pars$probability)
  return(prior_out)
}




which_arm_v3 <- function(prior,ctrs){
  arms_ctrs <- lapply(prior %>% map(1), 
                      sample,
                      x=ctrs,
                      size=100,
                      replace=TRUE) %>% 
    lapply(max) %>% 
    unlist()
  return(which.max(arms_ctrs))
}




#### begin simulation ####
set.seed(161)

sim_ctr<-seq(0, 1, by = 0.001)
arms<-list()
for(j in 1:15){
  print(j)
  ctr<-sample(seq(0.85,0.95,by = 0.02),1)
  arm_i<-list(prior=dunif(sim_ctr, min = 0, max = 1),
              arm_click=0,
              arm_ctr=ctr,
              arm_pull=0)
  arms<-c(arms,list(arm_i))
}



sim_info<-data_frame()
sim_priors<-list()
for(i in 1:400){
  print(i)
  winner_arm<-which_arm_v3(arms,sim_ctr)
  isClick<-sample(0:1,size=1,prob = c(1-arms[[winner_arm]]$arm_ctr,arms[[winner_arm]]$arm_ctr))
  arms[[winner_arm]]$arm_click<-arms[[winner_arm]]$arm_click+isClick
  arms[[winner_arm]]$arm_pull<-arms[[winner_arm]]$arm_pull+1
  
  x <- bayes_example1(clicks = arms[[winner_arm]]$arm_click,
                      impressions =  arms[[winner_arm]]$arm_pull,
                      prior = arms[[winner_arm]]$prior,
                      proportion_clicks = sim_ctr)
  prior_i<-data_frame(ctr=x$proportion_clicks,
                      probability=x$probability)
  arms[[winner_arm]]$prior <- prior_i$probability
  sim_info <- rbind(sim_info,data_frame(imp=i,winner_arm,clicks=arms[[winner_arm]]$arm_click))
  sim_priors<-append(sim_priors,list(arms))
}
