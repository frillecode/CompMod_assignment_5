create_population <- function() {
  print(">>>> Creating population")
  
  id <- c(1:n_people)
  sex <- c(rep(0, n_people/2), rep(1, n_people/2))
  d_base <- rnorm(n_people/4, 0.0, var_base)
  d_base <- c(d_base, 0-d_base, d_base, 0-d_base)
  return(data.frame(id, sex, d_base))
}

create_datasets <- function() {
  print(">>>>>>>> Creating data sets")
  
  data_set <- vector()
  participant_id <- vector()
  sex <- vector()
  condition <- vector()
  response <- vector()
  
  for (experiment in 1:n_experiments_per_repeat) {
    # add to the data_set, participant_id and sex columns
    data_set <- c(data_set, rep(experiment, n_participants_per_experiment))
    participant_id <- c(participant_id, c(1:n_participants_per_experiment))
    sex <- c(sex, rep(0, n_participants_per_experiment/2), rep(1, n_participants_per_experiment/2))
    
    # choose your sex 0 and sex 1 participants
    sex_0_sample <- sample(c(1:(n_people/2)), n_participants_per_experiment/2)
    sex_1_sample <- sample(c(((n_people/2)+1):n_people), n_participants_per_experiment/2)
    participants <- population[c(sex_0_sample, sex_1_sample),]
    
    # assign them to conditions with equal numbers of each sex in each condition
    dum <- c(rep(0, n_participants_per_experiment/4), rep(1, n_participants_per_experiment/4))
    temp_condition <- c(sample(dum, n_participants_per_experiment/2), sample(dum, n_participants_per_experiment/2))
    condition <- c(condition, temp_condition)
    
    # get the data from each participant
    temp_response <- vector()
    for (ppt in 1:n_participants_per_experiment) {
      lp <- b_base + participants$d_base[ppt] + b_sex*participants$sex[ppt] + b_cond*temp_condition[ppt] + b_sex_cond*participants$sex[ppt]*temp_condition[ppt]
      prob <- exp(lp)/(1+exp(lp))               
      temp_response[ppt] <- mean(runif(n_trials_per_participant, 0, 1) < prob)
    }
    response <- c(response, temp_response)
    rm(lp, prob, temp_response, temp_condition, sex_0_sample, sex_1_sample, dum)
  }
  return(data.frame(data_set, participant_id, sex, condition, response))
  #data_sets$response_z <- scale(data_sets$response)              #??????????????????????????????
  #library(ggplot2)
  #ggplot(data = this_data_set, aes(x=response_z)) + geom_density()
  #ggplot(data = this_data_set, aes(x=response)) + geom_density()
  #hist(this_data_set$response_z)
  #hist(data_sets$)
}

#mean(runif(10, 0, 1) < 0.7)
#generates random uniform distribution from 0-1 with n number of observations. for each observation it checks
#whether it is < prob and returns either true/false (corresponding to 1/0). then the mean is calculated of 
#these 0s and 1s - e.g. mean 0.9 means 9 TRUE and 1 FALSE