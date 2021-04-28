library(shiny)
library(corrplot)
library(ShinyQuickStarter)
ui <- fluidPage(
  # App title ----
  titlePanel("PCA Dashboard"),
  
  # Sidebar layout with input and output definitions ----
    tabPanel("PCA Dashboard",
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 h3("Tool Side Panel"),
                 
                 h6(
                   "You must press 'Read Data' at the bottom of the sidebar in order to work with your dataset"
                 ),
                 
                 tags$hr(),
                 selectInput(
                   "dataset",
                   "Select a built in dataset:",
                   choices = c('\n', "Adult Data Set", "Red Wine Quality", "Framingham Risk Score"),
                   selected = ""
                 ),
                 # Input: Select a file ----
                 fileInput(
                   'infile',
                   'Choose file to upload',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   )
                 ),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 
                 
                 tags$hr(),
                 h3("Data Settings"),
                 
                 # Input: Select separator ----
                 radioButtons(
                   "sep",
                   "Separator",
                   choices = c(
                     Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"
                   ),
                   selected = ","
                 ),
                 
                 
                 # Input: Select quotes ----
                 radioButtons(
                   "quote",
                   "Quote",
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = '"'
                 ),
                 
                 
                 
                 # Input: Select number of rows to display ----
                 
                 
                 actionButton("update", "Read Data"),
                 actionButton("reset", "Reset built in dataset selection")
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Histogram Plot",
                     plotOutput("plot", height = 700),
                     sliderInput(
                       "bins",
                       "Number of bins for the histogram",
                       min = 5,
                       max = 50,
                       value = 15
                     ),
                     selectInput(
                       "vars_PCA",
                       "Target Variable",
                       choices = c("", ""),
                       selected = ""
                     )
                   ),
                   
                   tabPanel(
                     "Correlation Analysis",
                     plotOutput("corPlot", height = 600),
                     selectInput(
                       "corMethod",
                       "Correlation Method",
                       choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
                       selected = "number"
                     )
                   ),
                   tabPanel("Statistical Summary", verbatimTextOutput("summary")),
                   tabPanel(
                     "Data Display Settings",
                     div(
                       style = "display:inline-block;vertical-align:top;",
                       radioButtons(
                         "disp",
                         "Data At a Glance Display",
                         choices = c(Head = "head", Tail = "tail",
                                     All = "all"),
                         selected = "head"
                       )
                     ),
                     div(style = "display: inline-block;vertical-align:top; width: 100px;", HTML("<br>")),
                     div(style = "display:inline-block;vertical-align:top;", tableOutput("contents"))
                     
                     
                     
                     
                   ),
                   
                   tabPanel(
                     "Perform PCA",
                     plotOutput("pcaPlot", height = 600),
                     div(
                       style = "display:inline-block;vertical-align:top;",
                       checkboxGroupInput(
                         "checkboxes_PCA",
                         label = "",
                         choices = NULL,
                         selected = FALSE
                       )
                     ),
                     div(
                       style = "display:inline-block;vertical-align:top;",
                       radioButtons(
                         "checkboxes_matrixop",
                         label = "Select Matrix",
                         choices = c("Covariance Matrix"),
                         selected = "Covariance Matrix"
                       )
                     ),
                     actionButton("doPCA", "Perform PCA"),
                     tableOutput("variance")
                   )
                   
                   
                   
                   
                 )#end of tabset
                 
                 # Output: Header + table of distribution ----
                 #h4("Data At a Glance"),
                 #tableOutput("contents"),
                 
                 # Output: Header + summary of distribution ----
                 #h4("Quick Statistics"),
                 #verbatimTextOutput("summary")
                 
                 
                 
               )
               
             )
    
  )#end of navbar
  ,
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  )
)#end of all