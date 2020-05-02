library (lubridate)
library (tidyr)
library (reticulate)

#reticulate::source_python ("time_cal_api.py")


#Needs a dataframe with 'start' and 'end' dates as dates
merge_overlapping_times <- function (df) {
  df = df %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(start)) >
                                cummax(as.numeric(end)))[-n()])) %>%
    group_by(indx) %>%
    summarise(start = min(start), end = max(end))
  
  return (as.data.frame (df[,c("start", "end")]))
}


adjust_start_times <- function (start_time, minutes_to_add, start_ooo, end_ooo){ 
  curr_start_time = start_time
  curr_end_time = curr_start_time + minutes (minutes_to_add)

  #If a task is due to start within an OOO, move it to start after the OOO
  if ((curr_start_time >= start_ooo) & (curr_start_time < end_ooo)){
    #print ("start within ooo")
    curr_start_time = end_ooo
    curr_end_time = curr_start_time + minutes (minutes_to_add)
  }
  
  return (list (curr_start_time, curr_end_time))
}


adjust_end_times <- function (start_time, minutes_to_add, start_ooo, end_ooo){ 
  
  curr_start_time = start_time
  curr_end_time = curr_start_time + minutes (minutes_to_add)
  
  #If the end time falls within an ooo, move it to be after the end of the ooo by
  #the amount that it's currrently within it
  if ((curr_end_time > start_ooo) & (curr_end_time < end_ooo)){
    #print ("end within ooo")
    curr_end_time = end_ooo + minutes (curr_end_time - start_ooo)
  }  else if ((curr_start_time <= start_ooo) & (curr_end_time >= end_ooo)){   #If the ooo falls within the task, add the ooo length to the current end time
    #print ("fully within")
    ooo_length = end_ooo - start_ooo
    #diff_to_add = task_length - ((start_ooo - curr_start_time) + (curr_end_time - end_ooo))
    diff_to_add = ooo_length - (start_ooo - curr_start_time)# + (curr_end_time - end_ooo))
    #print (diff_to_add)
    
    curr_end_time = curr_end_time + ooo_length #hours (diff_to_add)#ooo_length) #hours (diff_to_add)
  }
  
  return (list (curr_start_time, curr_end_time))
}


adjust_times <- function (current_start_time, minutes_to_add, ooos){
  ooos = merge_overlapping_times(ooos)
  
  ooos = ooos [order (ooos$start),]
  for (ooo in ooos){
    adjusted_times = adjust_start_times (current_start_time, minutes_to_add, ooo[1], ooo[2])
    current_start_time= adjusted_times[[1]]
    current_end_time= adjusted_times[[2]]
  }
  # current_start_time
  # current_end_time
  
  
  
  ooos = ooos [order (ooos$end),]
  for (ooo in ooos){
    adjusted_times = adjust_end_times (current_start_time, minutes_to_add, ooo[1], ooo[2])
    current_start_time= adjusted_times[[1]]
    current_end_time= adjusted_times[[2]]
  }
  
  return (list (current_start_time, current_end_time))
}

#----------------------------------------------------------


start_ooo = dmy_hm ("29/04/2020 15:00")
end_ooo = dmy_hm ("29/04/2020 16:00")

ooos <- data.frame (start = start_ooo, end = end_ooo)

ooos = rbind (ooos, data.frame (start = dmy_hm ("29/04/2020 17:00"), end = dmy_hm ("30/04/2020 09:00")))
ooos


#ooos = merge_overlapping_times(ooos)
#ooos


start_time = dmy_hm ("29/04/2020 15:30")
hours_to_add = 3

current_start_time = start_time
current_end_time = curr_start_time + hours (hours_to_add)

#current_start_time
#current_end_time



adjust_times(current_start_time = current_start_time, hours_to_add = hours_to_add, ooos = ooos)

