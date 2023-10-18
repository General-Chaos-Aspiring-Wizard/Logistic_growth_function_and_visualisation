#code that will implement a logistic simulation of population based on -
#given criteria. Numerical approximation of the function

#defining variables
N_0 <- 1    #starting population
r <- 0.03    #min^-1 # rate of change
K <- 100000     #coefficient needed for logistic function -
#defines carrying capacity of the environment

dt <- 0.01      #defining the time difference
total_time <- 430       #time in the simulation
number_of_steps <- round(total_time/dt, 1)
time <- c(0)        #here the time of each step will be recorded
N_t <- rep(0, number_of_steps)      #here the change in popultion -
#will be recorded
N_t[1] <- N_0       #first recording is initial population

for (t in 1:number_of_steps){
      #calculating parts of logistic equation and final equation
      positive_change <- r*dt*N_t[t]
      negative_change <- r*(N_t[t]/K)*N_t[t]
      #calculate and update population record
      N_t[t+1] <- N_t[t] + positive_change - negative_change
      time[t+1] <- time[t]+dt     #update the time record
}

#ploting of the results
plot(time, N_t,
     t="l",
     col="lightblue",
     lwd=3,
     xlab = "Time [min]",
     ylab = "Number of bacteria")
