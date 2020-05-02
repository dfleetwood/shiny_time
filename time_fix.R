tasks = read.csv ("all_tasks.csv", as.is = TRUE)
tasks = tasks [!is.na (tasks$Most_Likely_Hours),]


library (lubridate)
library (mc2d)
library (dplyr)

set_task_order <- function(){
  pert_hours = NULL
  for (i in 1:nrow (tasks)){
    pert_hours = c(pert_hours, qpert (0.8, tasks$Min_Hours[i], tasks$Most_Likely_Hours[i], tasks$Max_Hours[i]))
  }
  tasks$pert_hours = pert_hours
  
  
  time_now = now()
  tasks$hours_to_deadline = as.numeric (ymd_hms (tasks$Deadline) - time_now)
  
  tasks$percent_work_rem = tasks$hours_to_deadline / tasks$pert_hours
  
  tasks = tasks [order (tasks$percent_work_rem),]
  
  tasks$order = 1:nrow (tasks)
  
  #start_ooo = dmy_hm ("29/04/2020 15:00")
  #end_ooo = dmy_hm ("29/04/2020 16:00")
  
  #ooos <- data.frame (start = start_ooo, end = end_ooo)
  #ooos = rbind (ooos, data.frame (start = dmy_hm ("29/04/2020 17:00"), end = dmy_hm ("30/04/2020 09:00")))
  
  task_starts = NULL
  task_finishes = NULL
  current_finish= time_now
  for (i in 1:nrow (tasks)){
    #print (current_start)
    adjusted_start_finish = adjust_times(current_finish, round (tasks$pert_hours[i]*60), ooos)
    #print (adjusted_start_finish)
    current_start = adjusted_start_finish[[1]]
    current_finish = adjusted_start_finish[[2]] 
    
    task_starts = c(task_starts, as.character (current_start))
    task_finishes = c(task_finishes, as.character (current_finish))
  }
  
  tasks$task_start = task_starts
  tasks$task_finish = task_finishes
}

#------------
#Percent chance hitting deadline


real_time_delta_mins <- function (start, finish, ooos){
  ooos = merge_overlapping_times(ooos)
  
  time_diff_mins = as.numeric (difftime (finish, start, units= "mins"))
  
  ooos_to_add = ooos [(ooos$start >= ymd_hms (start)) & (ooos$end <= ymd_hms (finish)),]
  total_ooo_mins =  0
  if (nrow (ooos_to_add) > 0){
    total_ooo_mins = sum (as.numeric (difftime (ymd_hms (ooos_to_add$end), ymd_hms (ooos_to_add$start), units = "mins")))
  }
  
  time_diff_mins = time_diff_mins - total_ooo_mins
  return (time_diff_mins)
}

#start = time_now #"2020-04-29 11:00:00"
start = "2020-04-29 11:00:00"

prob_hitting_deadline (time_now, tasks, ooos){
  start = time_now
  prob_hit_deadline = NULL
  for (i in 1:nrow (tasks)){
    finish = tasks$Deadline [i] 
    
    cum_most_likely = sum (tasks$Most_Likely_Hours [1:i])
    cum_min = sum (tasks$Min_Hours [1:i])
    cum_max = sum (tasks$Max_Hours [1:i])
    
    
    real_min_available = real_time_delta_mins (start, finish, ooos)
    
    prob_hit_deadline = c(prob_hit_deadline, ppert (real_min_available/60, cum_min, cum_most_likely, cum_max))
  }
  return (prob_hit_deadline)
}



