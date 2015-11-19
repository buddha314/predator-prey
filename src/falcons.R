WD = "/Users/buddha/github/buddha314/predator-prey/src"
setwd(WD)

# Decide how effective everyone is
falcon_hunting_success_rate = 0.005
falcon_reproductive_rate = 0.001

mouse_reproductive_rate = 0.4

# What does it look like on day one?
n_falcons_day_one = 10
n_mice_day_one = 300
n_days_to_run = 60

results = data.frame(
  day=1
, falcons=n_falcons_day_one
, falcons_fed = n_falcons_day_one
, falcons_born = 0
, mice=n_mice_day_one
, mice_born = 0
, mice_eaten = 0
)

for (i in 2:n_days_to_run) {
  print(paste("Evaluating day", i))
  
  # Look at what we had yesterday
  falcons_yesterday = results[(i-1),"falcons"]
  mice_yesterday = results[(i-1), "mice"]
  
  # Figure out how many survived from yesterday
  falcons_born = falcons_yesterday * falcon_reproductive_rate # How many born yestery
  falcons_fed =  falcons_yesterday * falcon_hunting_success_rate * mice_yesterday # How many got to eat
  # So today we have
  falcons_today = falcons_fed + falcons_born
  
  mice_born = mice_yesterday * mouse_reproductive_rate # How many born
  # Below we use "min" to say that the falcons can't eat more mice than exist
  mice_eaten = min(mice_yesterday, mice_yesterday * falcons_yesterday * (1-falcon_hunting_success_rate)) # How many did the falcons get
  # So today we have
  mice_today = mice_yesterday + mice_born - mice_eaten
  

  # Add the new numbers to our results.  We use the max(x,y) function to keep things above 0,
  # you can't have -1 falcons
  results = rbind(
      results
    , data.frame(day=i
                 , falcons=max(0,falcons_today)
                 , falcons_born = max(0,falcons_born) 
                 , falcons_fed = max(0,falcons_fed)
                 , mice=max(0,mice_today)
                 , mice_born = max(0,mice_born)
                 , mice_eaten = max(0,mice_eaten)
                 )
  )
  
}
print(results)
plot(falcons ~ day, type="l", col="gold", data=results)
lines(mice ~ day, col="blue", data=results)

