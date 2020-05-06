#library(deckgl)
#library (mapdeck)
#require(geojsonio)
#library("viridis") 
#library (colorspace)
#library (ggplot2)
#library(DT)
#library(semantic.dashboard)
library(shinythemes)
library(shinydashboard)
#library (rhandsontable)
library (shinyWidgets)
library(DT)
#library(shinyTree)
#library (pdist)
#library (stringi)
library (shinyjs)
library(tuicalendr)
library (timevis)
#library(vistime)
#library (plotly)
#library(shinydashboardPlus)

#library(shinycssloaders)


label_opts = "All"
view <- navbarPage("Shiny time",id ="tabs", theme = shinytheme("flatly"), 

  
  tabPanel (title = "Tasks", value = "tasks",
      shinyjs::useShinyjs(),
      
      sidebarLayout(
        sidebarPanel(
          selectInput('projectSelect', 'Project', selected = "None", label_opts, multiple=FALSE, selectize=FALSE, size = 15),
          textInput ('newProjectTxt',label = "New Project"),
          actionButton ('newProjectBtn', "Add Project"),
          actionButton ('renameProjectBtn', "Rename Project"),
          hr(),
          actionButton ('deleteProjectBtn', "Delete Project")
        ),
        #https://rdrr.io/cran/shinyWidgets/man/airDatepicker.html
        mainPanel(
          useShinyjs(),
          fluidRow (
            column (7,
              fluidRow(
                column (6, textInput ('taskName', 'Task Name')),
                column (5, airDatepickerInput('taskDeadline', 'Deadline', timepicker = TRUE))
              ),
              fluidRow(
                column (2, numericInput ('taskLikelyDuration', "Likely task hours", value = 0.25, min = 0.25, max = 99999, step = 0.25)),
                column (2, numericInput ('taskMinDuration', "Min task hours", value = 0.25, min = 0.25, max = 99999, step = 0.25)),
                column (2, numericInput ('taskMaxDuration', "Max task hours", value = 0.5, min = 0.25, max = 99999, step = 0.25))
              )
            ),
            column (6,
              selectInput('taskDependencies', 'Dependences', list ('None'), multiple=TRUE, selectize=FALSE, size = 5),
              sliderInput("taskPercentComplete",label="Percent Complete", min = 0, max = 100, post  = " %", value = 0)
            ),
            actionButton ('addTaskBtn', "Add/Update task"),
            #actionButton ('updateTaskBtn', "Update task"),
            hr(),
            actionButton ('deleteTaskBtn', "Delete task")
          ),
          fluidRow (
            column (2, textInput("taskPert50", label = "PERT 50% hours") %>% disabled()),
            column (2, textInput("taskPert80", label = "PERT 80% hours") %>% disabled()),
            column (2, textInput("taskPert90", label = "PERT 90% hours") %>% disabled())
            #textOutput("taskPert50")
          ),
          hr(),
          checkboxInput('showComplete', 'Show complete', value = FALSE),
          DTOutput ('taskTable')
        )
    )
  ),
  tabPanel (title = "Planner", value = "planner",
    timevisOutput("taskTimeline"),
   # plotlyOutput ("taskTimeline"),
    calendarOutput ('taskCalendar')
  )
)

#   tabPanel (title = "Database Connection", value = "databasePanel",
#      shinyjs::useShinyjs(),
#      #extendShinyjs(text = jscode),
#      div (style = "zoom: 0.75; padding: 0px;margin-bottom: 0px;margin-top: 0px;",
#       textInput ('databaseHost',label = "Database IP Address", value = "51.116.226.229"),
#       passwordInput ('databasePassword',label = "Database Password"),
#       actionButton ('connectToDB', "Connect to Database"),
#       hr(),
#       selectInput('databaseLabel', 'Node type to visualize', "Not connected", multiple=FALSE, selectize=FALSE, size = 7),
#       selectInput('databaseField', 'Node property to visualize', "Not connected", multiple=FALSE, selectize=FALSE, size = 7),
#       actionButton ('start', "Start")
#       #textInput ('databaseLabel',label = "Label", value = "MediaArticle"),
#       #textInput ('databaseField',label = "Field", value = "Not connected"),
#       #textInput ('databaseSearchIndex',label = "Search Index", value = "mediaTexts"),
      
#     ),
#     hr()

#   ),
#   tabPanel (title = "Map", value = "mapPanel",
#   shinyjs::useShinyjs(),
#   #extendShinyjs(text = jscode),
#   sidebarLayout(
#     sidebarPanel(
#  # dashboardBody(
#     #menuItem("Map", tabName = "map", icon = icon("dashboard"),
#     selectInput('labelSelect', 'Label', selected = "None", label_opts, multiple=TRUE, selectize=FALSE, size = 15),
#     #shinyTree ("shiny_tree", dragAndDrop = TRUE),
#     textInput ('newLabelTxt',label = "New label"),
#     actionButton ('newLabelBtn', "Add Label"),
#     actionButton ('renameLabelBtn', "Rename Label"),
#     actionButton ('deleteLabelBtn', "Delete Label"),
#     hr(),
#     #actionButton ('addLabelsMapBtn', "Add labels to map"),
#     sliderInput  ('articleSizeSlider', "Article size", min = 500, max = 5000, step = 10, value = 1000),
#     sliderInput  ('brush', "Brush size", min = 5000, max = 50000, step = 100, value = 10000),
#     hr(),

#     #actionButton ('filterMapTableBtn', "Filter map by table"),
#     #actionButton ('showAllBtn', "Show all points on map"),
#     #hr()
#     #fileInput("loadPointsPath", "Load points file"),
#     #actionButton ("loadPointsBtn", "Load points"),
#     #textInput ("savePointsPath", "Save points filename", "article_points.csv"),
#     #actionButton ("savePointsBtn", "Save points")
#     #)
#   ),
#    mainPanel (
     

#       #  tags$head(
#       #   tags$style(HTML('#labelPoint{background-color:lightgreen}')),
#       #   tags$style(HTML('#removeLabelPoint{background-color:red}'))
#       # ),
#         # tags$head(
#         #   tags$style(
#         #     HTML(".shiny-notification {
#         #          position:fixed;
#         #          top: calc(0%);
#         #          left: calc(50%);
#         #          }
#         #          "
#         #     )
#         #   )
#         # ),
        
#         tags$script('$(document).on("keyup", function(e) {
#           if(e.keyCode == 13){
#             $("#labelPoint").click();
#           }
#         });
#         '),
        
#         #deckglOutput("deck"),
#         div (style = "background-color:#222d32;", withSpinner (mapdeckOutput("map", height = "600px"))),
#         withSpinner (textOutput ("mapLoaderHolder")),
#         div (style = "position:absolute; top: 95px; left: 420px; zoom: 0.6;",
#           div(style="display:inline-block;" , textInput ('searchTxt',label = "")),
#           div(style="display:inline-block;", actionButton ('searchBtn', "Search")), 
#           div(style="display:inline-block;", actionButton ('clearSearchBtn', "Clear Search"))
#         ),
#         hr(),
#         h4 ("Current point"),
#         uiOutput("currentPointOp"),
#         hr(),
#         actionButton ("labelPoint", "Label point"),
#         p ("                            ", style="display:inline; white-space:pre"),
#         actionButton ("removeLabelPoint", "Remove label from point"),
#         h3 ("Labelled points"),
#         #textInput("txtSearch", "Search text"),
#         # actionButton("searchBtn", "Search"),
#         withSpinner (DT::dataTableOutput("topicTagTable"))
        
        
#         # h3 ("Nearest unlabelled points"),
#         # DT::dataTableOutput("suggtable")

    
#   )
#  # )
#   ))
# )