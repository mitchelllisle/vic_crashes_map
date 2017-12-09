incidents_by_day_func <- function(map_data){
  tryCatch({
    incidents_by_day <- map_data %>% 
      group_by(ACCIDENT_DATE) %>% 
      summarise(total_incidents = n()) %>% 
      mutate(ACCIDENT_DATE = dmy(ACCIDENT_DATE)) %>% 
      arrange(desc(ACCIDENT_DATE))  
    
    incidents_by_day$total_incidents_weekly_average <- zoo::rollmean(incidents_by_day$total_incidents, 30, fill = "extend")
    
    return(incidents_by_day)
  }, error = function(e){
    return(NULL)
  })
}

type_percent_func <- function(map_data){
  tryCatch({
    type_percent <- map_data %>% 
      group_by(ACCIDENT_TYPE) %>% 
      summarise(total_incidents = n()) %>%
      ungroup() %>%
      mutate(total_type_percent = sum(total_incidents)) %>%
      mutate(total_incidents_percent = total_incidents/total_type_percent * 100) %>%
      arrange(desc(total_incidents_percent))

    return(type_percent)
  
    }, error = function(e){
    return(e)
  })
}

day_of_week_func <- function(map_data){
  tryCatch({
    day_of_week <- map_data %>% 
      filter(DAY_OF_WEEK != "") %>%
      group_by(DAY_OF_WEEK) %>% 
      summarise(total_incidents = n()) %>%
      ungroup() %>%
      mutate(total_day_percent = sum(total_incidents)) %>%
      mutate(total_incidents_percent = total_incidents/total_day_percent * 100) %>%
      arrange(desc(DAY_OF_WEEK)) %>%
      mutate(sortOrder = c(3, 2, 4, 7, 6, 1, 5)) %>%
      arrange(sortOrder)
    
    return(day_of_week)
    
  }, error = function(e){
    return(e)
  })
}