# #library(deckgl)
# library (mapdeck)
# library("viridis") 
# library (colorspace)
# library (ggplot2)
# library(DT)
# #library(semantic.dashboard)
# library (pdist)
# library (stringi)
library (reticulate)
# #library (shinyjs)
library (shiny)
#library (rhandsontable)
library(DT)
#library(ggplot2)
library (lubridate)
library (mc2d)
library (dplyr)
library (tidyr)

server <- function(input, output, session) {

  #-------------------------
  #Time functions
  #---------------------------
  real_time_delta_mins <- function (start, finish, ooos){
    ooos$start = ymd_hms (ooos$start)
    ooos$end = ymd_hms (ooos$end)

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

  prob_hitting_deadline <- function (time_now, tasks, ooos){
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


  #--------------------------
  #Connect to neo4j
  #--------------------------

  get_tasks <- function (project){
    graph = get_graph()
    tasks = graph$graph$run ("
        MATCH (project:Project {name:$current_project})
        MATCH (project) -[:HasTask]-> (task:Task)
        RETURN task.name AS Name, 
               task.deadline AS Deadline,
               task.most_likely_hours AS Most_Likely_Hours,
               task.min_hours AS Min_Hours,
               task.max_hours AS Max_Hours,
               task.task_start AS Task_Start,
               task.task_end AS Task_End,
               task.prob_hitting_deadline AS Prob_Hitting_Deadline

    ", current_project = project)$to_data_frame()
    return (tasks)
  }

  get_all_tasks <- function (){
    graph = get_graph()
    tasks = graph$graph$run ("
        MATCH (project:Project)
        MATCH (project) -[:HasTask]-> (task:Task)
        MATCH (task:Task)
        RETURN task.name AS Name, 
               task.deadline AS Deadline,
               task.most_likely_hours AS Most_Likely_Hours,
               task.min_hours AS Min_Hours,
               task.max_hours AS Max_Hours,
               project.name AS project
               ")$to_data_frame()
    return (tasks)
  }

  get_events <- function (){
    graph = get_graph()
    events = graph$graph$run(
      "
        MATCH (event :CalendarEvent)
        RETURN event.start AS start, event.end AS end, event.id AS id, event.name AS name, event.editable AS event 
      ")$to_data_frame()
    return (events)
  }

#----------------------
  neo <- import ("neo4jlite")
  get_graph <- function (){
    graph = neo$base$Neo4jGraph("neo4j", host = "neo4j", username = "neo4j", password = "bitnami", block_switch_db=TRUE)
    return (graph)
  }

  graph = get_graph()

  #-----------
  #Define data
  #----------
  data <- reactiveValues()
  # data$tasks = data.frame(id=integer(),
  #                         text=character()) 
  data$ready = FALSE
  data$projects = graph$graph$run ("MATCH (project:Project) RETURN DISTINCT project.name AS name ")$to_data_frame()  #data.frame(name=character()) 
  data$tasks = data.frame(name=character()) 
  data$current_task = data.frame(name=character()) 
  data$ooos = data.frame(id=integer()) 
  data$all_tasks = data.frame(id=integer()) 


  #-----------
  #Login to calendar
  #-----------
  calendarOAuthModal <- function(calendarUrl) {
    modalDialog(
      a (calendarUrl),
      textInput("calendarOAuthUrl", "Enter return url"),
       
      footer = tagList(
        actionButton("calendarOAuthOk", "OK")
      )
    )
  }

  source_python ("get_calendar.py")
  print ("Getting auth url")
  auth <<- get_authorization_url()
  showModal(calendarOAuthModal(auth[[1]]))

  observeEvent(input$calendarOAuthOk, {
      print ("Completing verification")
      # Check that data object exists and is data frame.
      complete_verification (input$calendarOAuthUrl, auth[[2]], auth[[3]])
      data$ready = TRUE
      #ooos = get_calendar(auth[[3]])

      #all_tasks = get_all_tasks()
      data$all_tasks = get_all_tasks()
      removeModal()
  })

  #-------------
  #Get calendar events (asych polling after data$ready is flagged) and put them in the database
  #-------------
  is_ready <- function (){
    ready = as.character (data$ready)
    showNotification(ready)
    return (data$ready)
  }
  get_calendar_events <- function(){
    #Get calendar event from O365 API
    events = get_calendar(auth[[3]])

    events_lists = list()
    for (i in 1:nrow (events)){
      event_list = list()
      event_list [['id']] = events[i, "id"]
      event_list [['name']] = events[i, "name"]
      event_list [['start']] = events[i, "start"]
      event_list [['end']] = events[i, "end"]
      event_list [['editable']] = FALSE
      events_lists[[i]] = event_list
    }
    #Update database with events
    graph = get_graph()
    graph$graph$run(
      "
        UNWIND $data as row
        MERGE (event :CalendarEvent {id: row.id})
        SET event = row      
      ",
      data = events_lists)

    #Get all calendar events out of the database, including OOO (out of office) ones not obtained from the O365 API
    data$ooos = get_events()
     #data$ooos = get_calendar(auth[[3]])
     #ooos_len = as.character (nrow (events))
     #showNotification(ooos_len)
  }
  #Trigger calendar refresh
  observe ({
    invalidateLater(600000, session)
    if (is_ready()){
      get_calendar_events()
    }
  })



  #--------------
  observe ({
    #-----------
    #Order tasks
    #----------
    tasks = data$all_tasks
    ooos = data$ooos

    #tasks = tasks [!is.na(tasks$most_likely_hours),]

    if ((nrow (tasks) > 0) & (nrow (ooos) > 0)){

      ooos$start = ymd_hms (ooos$start)
      ooos$end = ymd_hms (ooos$end)


    
      #Calcuate pert hours (e.g. 80% certainty of completing job within) 
      pert_hours = NULL
      for (i in 1:nrow (tasks)){
        pert_hours = c(pert_hours, qpert (0.8, as.numeric (tasks$Min_Hours[i]), as.numeric (tasks$Most_Likely_Hours[i]), as.numeric (tasks$Max_Hours[i])))
      }
      tasks$pert_hours = pert_hours
      
      #Order tasks based on pert hours (percent of job versus time to deadline)
      time_now = now()
      tasks$hours_to_deadline = as.numeric (ymd_hms (tasks$Deadline) - time_now)
      tasks$percent_work_rem = tasks$hours_to_deadline / tasks$pert_hours
      
      tasks = tasks [order (tasks$percent_work_rem),]
      tasks$order = 1:nrow (tasks)
     
      #-----
      #Calculate starts and finishes
      #-----  
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

      #Probability of hitting deadlines
      tasks = tasks[order (tasks$task_start),]
      tasks$prob_hitting_deadline = prob_hitting_deadline (time_now, tasks, ooos)

      #--------
      #Add to neo4j
      #--------
      task_schedules = list()
      for (i in 1:nrow (tasks)){
        task_schedule = list()
        task_schedule[['tsk']] = as.character (tasks [i, "Name"])
        task_schedule[['proj']] = as.character (tasks [i, "project"])
        task_schedule[['task_start']] = as.character (tasks [i, "task_start"])
        task_schedule[['task_end']] = as.character (tasks [i, "task_finish"])
        task_schedule[['prob_hitting_deadline']] = as.character (tasks [i, "prob_hitting_deadline"])
        
        task_schedules[[i]] = task_schedule
      }
      #Clear the task starts/finishes
      graph = get_graph()
      graph$graph$run ("
        MATCH (task:Task)
        SET task.task_start = null, task.task_end = null
        ")
      
      #Merge the starts/finishes
      graph = get_graph()
      graph$graph$run ("
        UNWIND $data AS row
        MATCH (project:Project {name:row.proj})
        MATCH (project) -[:HasTask]-> (task:Task {name: row.tsk})
        SET task.task_start = row.task_start, task.task_end = row.task_end, task.prob_hitting_deadline = row.prob_hitting_deadline
        RETURN count (task)
      ", data = task_schedules)$to_data_frame()

    }
  })


  # output$taskTable <- renderDataTable ({
  #   data$ooos
  # })

  # trueFunc <- function (){
  #   return (TRUE)
  # }
  # data$ooos <- reactivePoll(
  #   100,
  #   trueFunc,
  #   get_calendar(auth[[3]])
  # )
 
  

  #-----------------------------------------------
  #Projects
  #-----------------------------------------------

  observe ({
    updateTextInput (session = session, inputId = "newProjectTxt", value = input$projectSelect)
  })

  observe ({
#     if (req (data$ready)){
#       if (nrow (req (data$article_points)) > 0){
    project_opts <- as.character (req (data$projects$name))
    project_opts = c("All", project_opts)
    project_opts <- project_opts [order (project_opts)]
    updateSelectInput(session = session, inputId = "projectSelect", selected = project_opts[0], choices = project_opts)
 #      }
#     }
   })


  observeEvent(input$newProjectBtn,{
    graph = get_graph()
    isolate (graph$graph$run ("MERGE (project:Project {name: $name}) RETURN count (project)", name = input$newProjectTxt)$to_data_frame())
    graph = get_graph()
    data$projects = graph$graph$run ("MATCH (project:Project) RETURN DISTINCT project.name AS name")$to_data_frame()
  })


  observeEvent(input$renameProjectBtn,{
    if (isolate (input$projectSelect != "All")){
      graph = get_graph()
      isolate (graph$graph$run ("MATCH (project:Project {name: $old_name}) 
                                SET project.name = $new_name
                                RETURN count (project)", old_name = input$projectSelect, new_name = input$newProjectTxt)$to_data_frame())
      graph = get_graph()
      data$projects = graph$graph$run ("MATCH (project:Project) RETURN DISTINCT project.name AS name")$to_data_frame()
      }
  })

  observeEvent(input$deleteProjectBtn,{
    if (isolate (input$projectSelect != "All")){
      graph = get_graph()
    
      isolate (graph$graph$run ("MATCH (project:Project {name: $name}) DETACH DELETE project", name = input$projectSelect)$to_data_frame())
      graph = get_graph()
      data$projects = graph$graph$run ("MATCH (project:Project) RETURN DISTINCT project.name AS name")$to_data_frame()
    }
  })

#---------------------------------------------------------------------------------------------------------
# TASKS
#---------------------------------------------------------------------------------------------------------

  # observeEvent (input$calculateBtn{
  #   data$all_tasks <- get_all_tasks() 

  #   data$all_tasks

  # })
  
 # write.csv (get_all_tasks(), "all_tasks.csv")


  observe({
    updateTextInput (session, "taskPert50", value = round (qpert (0.5, min = input$taskMinDuration, mode= input$taskLikelyDuration, max = input$taskMaxDuration), 1))
    updateTextInput (session, "taskPert80", value = round (qpert (0.8, min = input$taskMinDuration, mode= input$taskLikelyDuration, max = input$taskMaxDuration), 1))
    updateTextInput (session, "taskPert90", value = round (qpert (0.9, min = input$taskMinDuration, mode= input$taskLikelyDuration, max = input$taskMaxDuration), 1))
  })


  observe ({
    task_names = data$tasks$Name
    task_names = task_names [task_names != input$taskName]

    updateSelectInput(session = session, "taskDependencies", choices = task_names)
  })

  observe ({
    likely_duration = input$taskLikelyDuration
    updateNumericInput(session = session, "taskMaxDuration", value = likely_duration*2)
    updateNumericInput(session = session, "taskMinDuration", value = likely_duration*0.8)
  })
  


  #Update task boxes with current task
  observe ({
    if (nrow (data$current_task) > 0){
      updateTextInput(session = session, "taskName", value = data$current_task$Name)
      updateAirDateInput(session = session, "taskDeadline", value = ymd_hms(data$current_task$Deadline))
      updateNumericInput(session = session, "taskLikelyDuration", value = data$current_task$Most_Likely_Hours)
      updateNumericInput(session = session, "taskMinDuration", value = data$current_task$Min_Hours)
      updateNumericInput(session = session, "taskMinDuration", value = data$current_task$Max_Hours)
    }
  })

  observeEvent (input$taskTable_rows_selected, {
    data$current_task = data$tasks [input$taskTable_rows_selected,]
  })

  output$taskTable <- renderDataTable({
     data$tasks
   }, selection = "single")

  observeEvent (input$projectSelect,{
    data$tasks = get_tasks(input$projectSelect)
  })

  observeEvent (input$addTaskBtn, {
    if (input$projectSelect != "All"){
      #Merge task
      graph = get_graph()
      graph$graph$run ("
        MERGE (task :Task {name: $name}) <-[:HasTask]- (project:Project {name: $project})
        SET task.deadline = $deadline, 
            task.most_likely_hours = $most_likely_hours,
            task.min_hours = $min_hours,
            task.max_hours = $max_hours
      ", name = as.character (input$taskName), 
         project = as.character (input$projectSelect), 
         deadline = as.character (input$taskDeadline),
         most_likely_hours = as.numeric (input$taskLikelyDuration),
         min_hours = as.numeric (input$taskMinDuration),
         max_hours = as.numeric (input$taskMaxDuration)
         )

        #Clear dependencies
        graph = get_graph()
        graph$graph$run ("
          MATCH (task :Task {name: $name}) <-[:HasTask]- (project:Project {name: $project})
          MATCH (task) <-[r:HasDownstreamTask]- (dependency_task) DELETE r
        ", name = as.character (input$taskName), 
          project = as.character (input$projectSelect), 
          dependencies = input$taskDependencies
          )

        #Merge dependencies
        if (length (input$taskDependencies) > 0){
          graph = get_graph()
          graph$graph$run ("
            MATCH (task :Task {name: $name}) <-[:HasTask]- (project:Project {name: $project})
            UNWIND $dependencies AS dependency
              MATCH (dependency_task :Task {name: dependency})
              MERGE (task) <-[:HasDownstreamTask]- (dependency_task)
          ", name = as.character (input$taskName), 
            project = as.character (input$projectSelect), 
            dependencies = input$taskDependencies
            )
        }

      data$tasks = get_tasks(input$projectSelect)
    }
  })

  observeEvent (input$deleteTaskBtn, {
    if (input$projectSelect != "All"){
      graph = get_graph()
      graph$graph$run ("
        MATCH (task :Task {name: $name}) <-[:HasTask]- (project:Project {name: $project})
        DETACH DELETE task
      ", name = as.character (input$taskName), project = as.character (input$projectSelect))

      data$tasks = get_tasks(input$projectSelect)
    }
  })


  # observeEvent(input$updateTaskBtn, {
  #   if (input$projectSelect != "All"){
  #     showNotification(input$current_task$Name)

  #     graph = get_graph()
  #     graph$graph$run ("
  #       MATCH (task :Task {name: $old_name}) <-[:HasTask]- (project:Project {name: $project})
  #       SET task.name = $name, task.deadline = $deadline
  #     ", old_name = as.character (data$current_task$Name),
  #        name = as.character (input$taskName), 
  #        deadline = as.character (input$taskDeadline),
  #        project = as.character (input$projectSelect))

  #     data$tasks = get_tasks(input$projectSelect)
  #   }
  # })

}

# server <- function(input, output, session) {

#   #MAX_POINTS = 300000


  
#   data <- reactiveValues()
#   data$article_points = data.frame(id=integer(),
#                                  text=character()) 
#   data$topic_tags = list ("None")
#   data$current_point = "None"
#   data$map_labels = data.frame(x=numeric(),
#                                y=numeric(),
#                                name=character())
#   data$topic_points = data.frame(id=integer(),
#                                  text=character()) 
#   data$ready = FALSE
#   data$connected_to_db = FALSE

#   neo <- import ("neo4jlite")


#   get_graph <- function (host, password){
#     graph = neo$base$Neo4jGraph("neo4j", host = host, password = password, block_switch_db=TRUE)
#     return (graph)
#   }

#   # observe ({
#   #   if (data$connected_to_db){
#   #     shinyjs::enable("databaseLabel")     
#   #   } else {
#   #     shinyjs::disable("databaseLabel")  
#   #   }
#   # })

#   # observe ({
#   #   if (input$databaseLabel == "Not connected"){
#   #     shinyjs::disable("databaseField")
#   #   } else {
#   #     shinyjs::enable("databaseField")
#   #   }
#   # })

#   # observe ({
#   #   if (input$databaseField == "Not connected"){
#   #     shinyjs::disable("databaseSearchIndex")
#   #   } else {
#   #     shinyjs::enable("databaseSearchIndex")
#   #   }
#   # })

#   #observe (showNotification (input$databaseField))


#   observeEvent (input$databaseLabel, {
#     showNotification(input$databaseLabel)

#     if (req (input$databaseLabel) != "Not connected"){
#       showNotification("hello")
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       fields = graph$graph$run (paste0 ("MATCH (n:", input$databaseLabel, ") WITH DISTINCT n LIMIT 1 RETURN apoc.meta.cypher.types(n) AS fields"))$to_ndarray()
#       fields = fields[[1]]
#       #showNotification(as.character (ncol (fields$fields[[0]])))
#       updateSelectInput(session, "databaseField", choices = names (fields))
#     }
#   })



#   observeEvent(input$connectToDB,{
#     #showNotification("Loading, this might take a while...",duration = 10, type = "message")
#     #Load_data

#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     # schema_nodes = graph$graph$run (
#     #     'CALL apoc.meta.schema() YIELD value as schemaMap
#     #         UNWIND keys(schemaMap) as label
#     #         WITH label, schemaMap[label] as data
#     #         WHERE data.type = "node"
#     #         RETURN label')$to_data_frame()
    

#     schema_nodes = graph$graph$run (
#         'MATCH (node) -[:HasUmapEmbedding]- (:UmapEmbedding)
#          RETURN DISTINCT labels(node) AS labels')$to_data_frame()
    
#     #shinyjs::enable("databaseLabel")
#     updateSelectInput(session, "databaseLabel", choices = schema_nodes$labels)


#     #showNotification(schema_nodes$),duration = 10, type = "message")

#   })

#   #This is a bit of a hack to make sure we're on the map panel before we try to load the map (it won't load properly otherwise)
#   observeEvent (input$start,{updateTabsetPanel(session, "tabs", selected = "mapPanel")})
#   observeEvent (input$tabs,{
#     if (input$tabs == "mapPanel"){
#       #updateTabsetPanel(session, inputId="tabSet", selected = "map")
#       showNotification("Getting data from database",duration = 10, type = "message")
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       #showNotification(paste0 ("Label: ", input$databaseLabel))
#       #showNotification(paste0 ("Field: ", input$databaseField))
#       full_article_points <<- graph$graph$run (
#                                         paste0 ("MATCH (object:", input$databaseLabel, ") -[:HasUmapEmbedding]- (umap:UmapEmbedding) 
#                                         WITH umap, object LIMIT 30000
#                                         RETURN toString (object.id) AS object_id, object.", input$databaseField, " AS text, umap.x AS x, umap.y AS y, umap.density AS density"))$to_data_frame()
      
#       #shinyjs::logjs(names (full_article_points))

#       #Get existing tags
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       topic_tags = graph$graph$run ("MATCH (tag:TopicTags) RETURN DISTINCT tag.name AS name")$to_data_frame()

#       data$article_points <- full_article_points
#       #shinyjs::logjs(names (data$article_points))
#       data$topic_tags <- topic_tags
#       data$ready = TRUE
#     }
#   })




#   #-------------------------------------------------------------------
  
  
#   # print (head (article_points))
#   # print (dim (article_points))

#   #---------------------------------------------------------------------
#   #MAP
#   #---------------------------------------------------------------------
#   output$map <- renderMapdeck({
#           mapdeck(style = mapdeck_style('dark'), height = 800)
#       })


#   output$mapLoaderHolder <- renderText ({"                                            "})
#   observe ({
#     if (req (data$ready)){
#         if (nrow (req (data$article_points)) > 20){
#         showNotification("Visualizing data.",duration = 10, type = "message")
#         pal = colourvalues::colour_values(1:20, alpha = 1)
#         data$article_points$colour = pal [as.numeric (cut_number (data$article_points$density, 20))]
        
#         data_length = nrow (data$article_points)
#         #shinyjs::logjs(data_length)
#         #shinyjs::logjs(names (data$article_points))
#        # showNotification(as.character (data_length),duration = 10, type = "message")
#        # showNotification(as.character (data_names),duration = 10, type = "message")
#         mapdeck_update(map_id = "map")  %>% add_scatterplot(
#               data = data$article_points
#               , lat = "x"
#               , lon = "y"
#             # , id = "id"
#               , tooltip = "text"
#               , radius = input$articleSizeSlider
#               , fill_colour = "colour"
#               , auto_highlight = TRUE
#              # , brush_radius = input$brush
#               #, palette = "viridis"
#               , update_view = FALSE
#             )
#          showNotification("displayed map",duration = 10, type = "message")
#       }
#     } else {
#       showNotification("Not enough data to display",duration = 10, type = "warning")
#     }
#   })


#   observe ({
#     if (req (data$ready)){
#         if (nrow (req (data$article_points)) > 20){
#         #showNotification("Visualizing data.",duration = 10, type = "message")
#         pal = colourvalues::colour_values(1:20)
#         article_points_copy = data$article_points

#         article_points_copy$colour = pal [as.numeric (cut_number (article_points_copy$density, 20))]
        
#         #data_length = nrow (data$article_points)
#         #shinyjs::logjs(data_length)
#         #shinyjs::logjs(names (data$article_points))
#        # showNotification(as.character (data_length),duration = 10, type = "message")
#        # showNotification(as.character (data_names),duration = 10, type = "message")
#         mapdeck_update(map_id = "map")  %>% add_scatterplot(
#               data = article_points_copy
#               , lat = "x"
#               , lon = "y"
#              , layer_id = "brushmap"
#              , id = "brushmap"
#               , tooltip = "text"
#               , radius = input$articleSizeSlider
#               , fill_colour = "colour"
#               , auto_highlight = FALSE
#               , brush_radius = input$brush
#               #, palette = "viridis"
#               , update_view = FALSE
#             )
#          showNotification("displayed map",duration = 10, type = "message")
#       }
#     } else {
#       showNotification("Not enough data to display",duration = 10, type = "warning")
#     }
#   })




# observe ({
#     if (req (data$ready)){
#       if (nrow (req (data$map_labels)) > 0){
#       mapdeck_update(map_id = "map")  %>%  add_text(
#             data = data$map_labels
#             , lat = "x"
#             , lon = "y"
#             , text = "name",
#             , size = 18
#             , anchor = "middle"
#             , alignment_baseline = "center"
#             , billboard = TRUE
#             , fill_colour  = "#FFFFFF"
#             , fill_opacity  = 255
#             , font_weight = 900
#             #, sizeUnits ='meters'
#             , update_view = FALSE
#           )
#       }
#     }
    
#   })


# observe ({
#     if (req (data$ready)){
#       data$topic_points
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       data$map_labels = graph$graph$run (
#           paste0 ("
#           MATCH (tag: TopicTags) -[:HasTopicTag]- (object:",  input$databaseLabel, ")
#           MATCH (object) -[:HasUmapEmbedding]- (umap:UmapEmbedding)
#           RETURN tag.name AS name, percentileCont(umap.x, 0.5) AS x, percentileCont(umap.y, 0.5) AS y
#         "))$to_data_frame()
#     }
#   })
  

#   # output$map <- renderMapdeck({
#   #       mapdeck() %>% add_scatterplot(
#   #         data = data$article_points
#   #         , lat = "x"
#   #         , lon = "y"
#   #         , id = "id",
#   #         , tooltip = "text"
#   #         , radius = 1000
#   #         , fill_colour = "density"
#   #         , palette = "plasma"
#   #       )
#   #   })

# #  --------------------------------------------------------------------
# #  Topic Tags
# #  --------------------------------------------------------------------
#   observe ({
#     if (req (data$ready)){
#       if (nrow (req (data$article_points)) > 0){
#         label_opts <- as.character (data$topic_tags$name)
#         label_opts = c(label_opts, "None")
#         label_opts <<- label_opts [order (label_opts)]
#         updateSelectInput(session = session, inputId = "labelSelect", selected = label_opts[0], choices = label_opts)
#       }
#     }
#   })

#   observeEvent(input$newLabelBtn,{
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     isolate (graph$graph$run ("MERGE (tag:TopicTags {name: $name}) RETURN count (tag)", name = input$newLabelTxt)$to_data_frame())
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     data$topic_tags = graph$graph$run ("MATCH (tag:TopicTags) RETURN DISTINCT tag.name AS name")$to_data_frame()
#   })

#   observeEvent(input$renameLabelBtn,{
#     #if (isolate (input$labelSelect != "None")){
#     #graph = get_graph()
#     #showNotification(input$labelSelect,duration = 10, type = "message")
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     isolate (graph$graph$run ("MATCH (tag:TopicTags {name: $old_name}) 
#                               SET tag.name = $new_name
#                               RETURN count (tag)", old_name = input$labelSelect, new_name = input$newLabelTxt)$to_data_frame())
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     data$topic_tags = graph$graph$run ("MATCH (tag:TopicTags) RETURN tag.name AS name")$to_data_frame()
#       #}
#   })

#   observeEvent(input$deleteLabelBtn,{
#     #if (isolate (input$labelSelect != "None")){
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
  
#     isolate (graph$graph$run ("MATCH (tag:TopicTags {name: $name}) DETACH DELETE tag", name = input$labelSelect)$to_data_frame())
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     data$topic_tags = graph$graph$run ("MATCH (tag:TopicTags) RETURN DISTINCT tag.name AS name")$to_data_frame()
#       #}
#   })


#   #--------------------------------------------------
#   #SEARCH
#   #--------------------------------------------------
  
#   observeEvent(input$searchBtn, {
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#     index_name = paste0 (input$databaseLabel,input$databaseField)
#     graph$graph$run ("CALL db.index.fulltext.createNodeIndex($index_name,[$databaseLabel],[$databaseField])", 
#                     index_name = index_name,
#                     databaseLabel = input$databaseLabel,
#                     databaseField = input$input$databaseField)
#     data$article_points <- graph$graph$run (
#                     paste0('
#                     CALL db.index.fulltext.queryNodes("', index_name, '", $search_string) YIELD node, score
#                     MATCH (node) -[:HasUmapEmbedding]- (umap:UmapEmbedding) 
#                     WITH umap, node LIMIT 300000
#                     RETURN toString (node.id) AS tweet_id, node.', input$databaseField ,' AS text, umap.x AS x, umap.y AS y, umap.density AS density
#                     '), search_string = as.character (input$searchTxt))$to_data_frame()
#   })
  

# observeEvent(input$clearSearchBtn, {
#     graph = get_graph(host = input$databaseHost, password = input$databasePassword)  
#     data$article_points <- full_article_points
    
#     graph$graph$run ('
#                     MATCH (node) -[:HasUmapEmbedding]- (umap:UmapEmbedding) 
#                     WITH umap, node LIMIT 300000
#                     RETURN node.id AS id, node.full_text AS text, umap.x AS x, umap.y AS y, umap.density AS density
#                     ')$to_data_frame()
#   })

  
#   # 'searchTxt',label = "Search database")),
#   #   div(style="display:inline-block", actionButton ('searchBtn', "Search")), 
#   #   div(style="display:inline-block", actionButton ('clearSearchBtn', "Clear Search")),


#   #--------------------------------------------------
#   #Select, Add, Remove tags from points
#   #--------------------------------------------------
  
#   #Click listener (updates 'current_point')
#   observe({
#     if (req (data$ready)){
#       js <- req (input$map_scatterplot_click)
#       click_data <- jsonlite::fromJSON( js )
#       index = req (click_data[['index']])
      

#       if (nrow (req(data$article_points)) > 0){
#         isolate (data$current_point <- data$article_points[index + 1,])
        
#         #idx = as.character (data$article_points[index + 1,"id"])
#         #showNotification (idx)

#         #data$current_point <- index
#       }
#     }
#   })


#   # #Render the current point
#   output$currentPointOp <- renderUI({
#     if (class (req (data$current_point)) == "character"){
#       "None"
#     }
#     else{
#       HTML (paste (as.character(req (data$current_point$tweet_id)), ": <b>", as.character(req (data$current_point$text)), "</b>"))
#     }
#   })

#   # #Add label to point
#   observeEvent (input$labelPoint,{
#     if (is.null (input$labelSelect)){
#       showNotification("No label selected. Select the label you want to add to first", type = "warning")
#     } else if (input$labelSelect == "None") {
#       showNotification("Cannot add point to 'None'. Select the label you want to add to first", type = "warning")
#     } else if (req (data$current_point) == "None"){
#       showNotification("No point select. Select one on the map or table first", type = "warning")
#     } else {
#       #showNotification(input$labelSelect)
#       #id = as.character (data$current_point$tweet_id)
#       #showNotification(id)

#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       n_added = graph$graph$run (
#         paste0 ("
#         MATCH (tweet: ",  input$databaseLabel, " {id: toInteger($id)})
#         MATCH (tag: TopicTags {name: $topic_tag})
#         MERGE (tweet) -[r:HasTopicTag]-> (tag)
#         RETURN count (r)
#       "), id = data$current_point$tweet_id, topic_tag = input$labelSelect)$to_ndarray()
#       #showNotification(as.character (n_added))

#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       data$topic_points = graph$graph$run (
#                         paste0 ("
#                         MATCH (tweet:", input$databaseLabel, ") -[:HasTopicTag]- (tag: TopicTags {name: $topic_tag})
#                         RETURN DISTINCT toString (tweet.id) AS tweet_id, tweet.", input$databaseField, " AS text"), topic_tag = input$labelSelect)$to_data_frame()
#       showNotification("Added point", type = "warning")
#     } 
#   })


#   # #Remove label from point
#   observeEvent (input$removeLabelPoint,{
#     if (is.null (input$labelSelect)){
#       showNotification("No label selected. Select the label you want to remove point from first", type = "warning")
#     } else if (input$labelSelect == "None") {
#       showNotification("Cannot remove point from 'None'. Select the label you want to remove from first", type = "warning")
#     } else if (req (data$current_point) == "None"){
#       showNotification("No point select. Select one on the map or table first", type = "warning")
#     } else {
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       graph$graph$run ("
#         MATCH (tweet: Tweet {id: toInteger($id)}) -[r:HasTopicTag]-> (tag: TopicTags {name: $topic_tag})
#         DELETE r
#         RETURN count (r)
#       ", id = data$current_point$tweet_id, topic_tag = input$labelSelect)$to_ndarray()

#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       data$topic_points = graph$graph$run (
#                         paste0 ("
#                         MATCH (tweet:",  input$databaseLabel, ") -[:HasTopicTag]- (tag: TopicTags {name: $topic_tag})
#                         RETURN DISTINCT toString (tweet.id) AS tweet_id, tweet.", input$databaseField, " AS text"), topic_tag = input$labelSelect)$to_data_frame()
#       #showNotification("Added point", type = "warning")
#     } 
#   })

#   #--------------------------------------------------
#   #TOPIC SWITCHING
#   #--------------------------------------------------

#   #TODO: This should show unlabelled points, not all?
#   observeEvent (input$labelSelect, {
#     updateTextInput(session, "newLabelTxt", value = input$labelSelect)
#     if (input$labelSelect == "None"){

#       #graph = get_graph()
#       # data$topic_points = graph$graph$run ("MATCH (tweet:Tweet) -[:HasUmapEmbedding]- (umap:UmapEmbedding) 
#       #                   WHERE NOT (tweet) -[:HasTopicTag]- (:TopicTag)
#       #                   WITH umap, tweet LIMIT 10000
#       #                   RETURN tweet.id AS id, tweet.full_text AS text, umap.x AS x, umap.y AS y, umap.density AS density")$to_data_frame() 
#       data$topic_points = data$article_points
#     } else {
#       graph = get_graph(host = input$databaseHost, password = input$databasePassword)
#       data$topic_points = graph$graph$run (
#                             paste0 ("
#                             MATCH (tweet:",  input$databaseLabel, ") -[:HasTopicTag]- (tag: TopicTags {name: $topic_tag})
#                             RETURN DISTINCT toString (tweet.id) AS tweet_id, tweet.", input$databaseField, " AS text"), topic_tag = req (input$labelSelect))$to_data_frame()                      
#     }
#   })



#   #--------------------------------------------------
#   #DATA TABLE
#   #--------------------------------------------------

#   #Render table when "topic_points" is updated
#   output$topicTagTable <- renderDataTable({
#     data$topic_points
#   }, selection = "single")#, options = list(dom = 't'))


#   observe ({#input$topicTagTable_rows_selected,{
#     if (req (data$ready)){
#       if (length (req (input$topicTagTable_rows_selected)) > 0){
#         data$current_point = data$topic_points [input$topicTagTable_rows_selected,]
#       }
#     }
#     #data$current_point = as.character (input$topicTagTable_rows_selected)
#     #ap = values$article_points_working
#     #values_current$current_point_working <- ap$X [ap$group == input$labelSelect] [input$mytable_rows_selected] + 1
#   })

    










# #-----------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------
# #Notes


#   #colnames (article_points) <- c("lat", "lon", "X", "pmid", "label", "label_clean", "dist")
#   # print (head (article_points))
#   # article_points$col = plasma(20) [as.numeric (cut_number (article_points$dist, 20))]
#   # article_points$red = col2rgb(article_points$col)[1,]
#   # article_points$green = col2rgb(article_points$col)[2,]
#   # article_points$blue = col2rgb(article_points$col)[3,]
  
  
#   #sugg_df <<- article_points [1:5,]
  
  
#   # #print (article_points$red)
  
#   # if (sum (colnames (article_points) == "group") == 0){
#   #   article_points$group = "None"
#   #   article_points$group = as.character (article_points$group)
#   # } else {
#   #   label_opts <- as.character (unique (c("None", article_points$group)))
#   #   label_opts <<- label_opts [order (label_opts)]
#   #   updateSelectInput(session = session, inputId = "labelSelect", selected = "None", choices = label_opts)
#   # }
  
#   # article_points <<- article_points
  
#   # #Load embs
#   # library (data.table)
#   # art_embs = fread ("article_embs.csv",skip = 1)
#   # art_embs = art_embs [,3:ncol (art_embs)]
  
#   # observeEvent (input$labelSelect, {
#   #   updateTextInput(session = session, inputId = "newLabelTxt", value = input$labelSelect)
#   #   values$article_points_working = article_points [article_points$group == input$labelSelect,] 
#   # })
  
#   # observeEvent (input$deleteLabelBtn, {
#   #   if (input$labelSelect != "None"){
#   #     article_points = values$article_points_working
#   #     article_points$group [article_points$group == input$labelSelect] = "None"
      
#   #     values$article_points_working = article_points
      
#   #     label_opts <- label_opts [-which (label_opts == input$labelSelect)]
#   #     label_opts <<- label_opts [order (label_opts)]
#   #     updateSelectInput(session = session, inputId = "labelSelect", selected = "None", choices = label_opts)
#   #   }
#   # })
  
#   # observeEvent (input$renameLabelBtn, {
#   #   if (input$labelSelect != "None"){
#   #     article_points = values$article_points_working
#   #     article_points$group [article_points$group == input$labelSelect] = input$newLabelTxt
#   #     values$article_points_working = article_points
      
#   #     #   print (input$labelSelect)
#   #     label_opts [which (label_opts == input$labelSelect)] = input$newLabelTxt
#   #     label_opts <- label_opts
#   #     label_opts <<- label_opts [order (label_opts)]
      
#   #     #   print (label_opts)
#   #     updateSelectInput(session = session, inputId = "labelSelect", selected = "None", choices = label_opts)
#   #   }
#   # })
  
#   # observeEvent (input$newLabelBtn, {
#   #   if (sum (label_opts == input$newLabelTxt) == 0){
#   #     label_opts <- c(label_opts, input$newLabelTxt)
#   #     label_opts <<- label_opts [order (label_opts)]
#   #     updateSelectInput(session = session, inputId = "labelSelect", selected = "None", choices = label_opts)
#   #   }
#   # })
  
#   # observeEvent (input$savePointsBtn, {
#   #   showNotification(paste0("Saving...", dim (values$article_points_working)),duration = 5, type = "message")
#   #   write.csv (values$article_points_working, input$savePointsPath)
#   #   showNotification(paste0("Saved to ", input$savePointsPath),duration = 5, type = "message")
#   # })
  
#   # observeEvent (input$addLabelsMapBtn, {
#   #   article_points = values$article_points_working
#   #   a_group_u = unique (article_points$group)
#   #   if (length (unique (article_points$group)) > 1){
#   #     showNotification("Loading, this might take a while...",duration = 5, type = "message")
#   #     grouplist = list()
#   #     for (i in 1:length (a_group_u)){
#   #       if (a_group_u [i] != "None"){
#   #         lat = as.numeric (median (article_points$lat [article_points$group == a_group_u[i]]))
#   #         lon = as.numeric (median (article_points$lon [article_points$group == a_group_u[i]]))
#   #         name = a_group_u[i]
          
#   #         grouplist[[i]] <- c(lat, lon, name)
#   #       }
#   #     }
#   #     labels_df_new <- do.call(rbind, grouplist)
#   #     labels_df_new = data.frame (labels_df_new, stringsAsFactors = FALSE)
#   #     #   print (labels_df_new)
#   #     colnames (labels_df_new) = c("lat", "lon", "name")
      
#   #     labels_df_new$lat = as.numeric (labels_df_new$lat)
#   #     labels_df_new$lon = as.numeric (labels_df_new$lon)
#   #     labels_df <<- labels_df_new
#   #     #   print (labels_df)
      
#   #     showNotification("Loading, this might take a while...",duration = 5, type = "message")
#   #     deckgl_proxy("deck") %>%
#   #       add_scatterplot_layer(id = "articles", data = values_current$current_point_working [input$myDataTable_rows_all,], properties = properties_articles) %>%
#   #       add_text_layer(id = "labels", data = labels_df, properties = properties_labels) %>%
#   #       update_deckgl()
#   #   } else {
#   #     showNotification("No labels to show (except 'None')",duration = 5, type = "message")
#   #   }
#   # })
  
#   # observeEvent (input$searchBtn, {
#   #   values$article_points_working = values$article_points_working [stri_detect_fixed (values$article_points_working$label_clean, input$txtSearch),]
#   # })
  
#   # observeEvent (input$filterMapTableBtn, { 
#   #   properties_articles_big <- list(
#   #     pickable = TRUE,
#   #     getPosition = JS("data => [data.lon, data.lat]"),
#   #     getRadius = input$articleSizeSlider,
#   #     getColor = JS("data => [data.red, data.green, data.blue]"),
#   #     radiusScale = 8,
#   #     getTooltip = JS("object => `${object.label}`")
#   #   )
    
#   #   print ("filtering")
    
#   #   filter_df = values$article_points_working [values$article_points_working$group == input$labelSelect,][input$mytable_rows_all,]
    
#   #   print ("showing")
    
#   #   showNotification("Loading, this might take a while...",duration = 5, type = "message")
#   #   deckgl_proxy("deck") %>%
#   #     add_scatterplot_layer(id = "articles", data = filter_df, properties = properties_articles_big) %>%
#   #     add_text_layer(id = "labels", data = labels_df, properties = properties_labels) %>%
#   #     update_deckgl()})
  
#   # observeEvent (input$showAllBtn,{
#   #   properties_articles_big <- list(
#   #     pickable = TRUE,
#   #     getPosition = JS("data => [data.lon, data.lat]"),
#   #     getRadius = input$articleSizeSlider,
#   #     getColor = JS("data => [data.red, data.green, data.blue]"),
#   #     radiusScale = 8,
#   #     getTooltip = JS("object => `${object.label}`")
#   #   )
    
#   #   showNotification("Loading, this might take a while...",duration = 5, type = "message")
#   #   deckgl_proxy("deck") %>%
#   #     add_scatterplot_layer(id = "articles", data = values$article_points_working, properties = properties_articles_big) %>%
#   #     add_text_layer(id = "labels", data = labels_df, properties = properties_labels) %>%
#   #     update_deckgl()
#   # })
  
#   # values <- reactiveValues(article_points_working = article_points)
#   # values_current <- reactiveValues(current_point_working = current_point)
  
#   # output$currentPointOp <- renderText({
#   #   #article_points = values$article_points_working
#   #   paste0 (values_current$current_point_working, ": ", article_points$label [values_current$current_point_working])
#   # })
  
#   # observeEvent (input$deck_onclick, {
#   #   info <- input$deck_onclick
#   #   object <- info$object
    
#   #   values_current$current_point_working <- object$X + 1
#   # })
  
#   # observeEvent (input$mytable_rows_selected,{
#   #   ap = values$article_points_working
#   #   values_current$current_point_working <- ap$X [ap$group == input$labelSelect] [input$mytable_rows_selected] + 1
#   # })
  
#   # observeEvent (input$labelPoint, {
    
#   #   if (!is.null (values_current$current_point_working)){
#   #     #article_points = values$article_points_working
#   #     article_points$group = as.character (article_points$group)
#   #     article_points$group [values_current$current_point_working] = input$labelSelect
#   #     article_points$group = as.character (article_points$group)
      
#   #     values$article_points_working <- article_points [article_points$group == input$labelSelect,]  #subset(data, rating %in% input$checkGroups)
#   #   }
#   # })
  
  
  
#   # observeEvent(input$deleteRow, {
#   #   ap = values$article_points_working
#   #   ap$group = as.character (ap$group)
#   #   ap$group [values_current$current_point_working] <- "None"
    
#   #   values$article_points_working <- ap
#   # })
  
  
#   # output$mytable = DT::renderDataTable ({
#   #   values$article_points_working [,c("X", "pmid", "label", "group")]
#   #   #values$article_points_working [values$article_points_working$group == input$labelSelect,c("X", "pmid", "label", "group")]
#   # }, selection = "single", options = list(dom = 't')) #options = list(searchHighlight = TRUE))
  
#   # output$suggtable = DT::renderDataTable ({
#   #   #lab_mean = colMeans(art_embs [values$article_points_working$group == input$labelSelect,])
#   #   in_idx = as.integer (values$article_points_working$X + 1)
#   #   lab_mean = colMeans(art_embs [in_idx,])
#   #   cdists = as.numeric (pdist (art_embs [-in_idx,], lab_mean)@dist)
#   #   #cdists = as.numeric (pdist (art_embs [values$article_points_working$group == "None",], lab_mean)@dist)
    
#   #   cdists_idx = order (cdists)[1:min (1000, length (cdists))]
    
#   #   #  print (c(values$article_points_working$X + 1))
    
#   #   # sugg_df <- values$article_points_working [values$article_points_working$group == "None",c("X", "pmid", "label", "group")][cdists_idx,]
#   #   sugg_df <- article_points [-in_idx,c("X", "pmid", "label", "group")][cdists_idx,]
#   #   #print (sugg_df)
#   #   # print (dim (sugg_df))
#   #   sugg_df$distance = round (cdists, 2)[cdists_idx]
#   #   sugg_df <<- sugg_df
#   # }, selection = "single", options = list(dom = 't')) #options = list(searchHighlight = TRUE))
  
#   # observeEvent (input$suggtable_rows_selected,{
#   #   # print ("Hello!!")
#   #   #ap = values$article_points_working
    
#   #   values_current$current_point_working <- sugg_df$X [input$suggtable_rows_selected] + 1
#   #   #values_current$current_point_working <- ap$X [ap$group == "None"] [input$suggtable_rows_selected] + 1
#   # })
  
  
#   # properties_articles <- list(
#   #   pickable = TRUE,
#   #   getPosition = JS("data => [data.lon, data.lat]"),
#   #   getRadius = 100,
#   #   getColor = JS("data => [data.red, data.green, data.blue]"),
#   #   radiusScale = 8,
#   #   getTooltip = JS("object => `${object.label}`")
#   # )
  
  
#   # properties_labels <- list(
#   #   pickable = FALSE,
#   #   getPosition = JS("lab_data => [lab_data.lon, lab_data.lat]"),
#   #   getSize = 25,
#   #   getText = JS("lab_data => lab_data.name"),
#   #   getColor = JS("lab_data => [0.0, 0.0, 0.0]")
#   # )
  
#   # properties_contour <- list(
#   #   filled = FALSE,
#   #   getLineColor = JS ("cnt_data => [0, 0, 0]"),
#   #   getLineWidth = 500
#   # )
  
#   # labels_df = data.frame ("lat" = mean (article_points$lat), "lon" = mean (article_points$lon), "name" = " ")
  
  
  
#   #output$deck <- renderDeckgl({
#   #  deckgl(latitude = mean (article_points$lat), longitude = mean (article_points$lon), zoom = 5,height = "800px") %>%
#   #    add_scatterplot_layer(id = "articles", data = article_points)# %>% #, properties = properties_articles) %>%
#       #add_text_layer(id = "labels", data = labels_df)#, properties = properties_labels)
    
#   #})
# }