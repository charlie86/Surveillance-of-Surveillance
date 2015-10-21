require(markdown)
require(shiny)
require(ggplot2)
require(ggthemes)
require(googleVis)
source ("./Data.R")
# source("C:/Users/Charlie/Documents/Booz/Dashboard/Shiny/Dashboard/Data.R")

shinyUI(fluidPage(
    titlePanel("Surveillance of Surveillance"),
    
    ############################ BAR PLOTS #####################################                
    
    fluidRow(
        wellPanel(
            h3("Distribution of Systems by Category"), 
            
            column(3, 
                   br(), br(),
                   selectInput("cat.var",
                               label = "Category:",
                               c("Owner Type" = "owner_type", 
                                 "Owner Name" = "owner_name",
                                 "System Type" = "type", 
                                 "Species", 
                                 "Purpose" = "surv_category",
                                 "Geographic Coverage" = "geo_cover",
                                 "Level of Documentation" = "level_doc",
                                 "Current Status" = "status",
                                 "Update Frequency" = "updates",
                                 "Accessibility" = "accessibility"),
                               selected = "owner_type"),
                   
                   selectInput("cat.chart",
                               label = "Chart Type:",
                               c("Bar Chart", "Pie Chart", "Treemap"),
                               selected = "Bar Chart")
                   
            ),
            
            column(9, br(), br(),
                   strong(textOutput("cat.titles")),
                   plotOutput("cat.plot"),
                   br()
            )
        )
    ),
    
    ############################ KEYWORD PLOTS ################################
    
    fluidRow(
        wellPanel(
            h3("Keyword Frequency Plot"),
            column(3, br(), br(),
                   selectInput("freq.var",
                               label = "Category:",
                               c("Conditions" = "conditions", 
                                 "Data Source" = "data_source", 
                                 "Submission Mechanism" = "data_submitted_via", 
                                 "IT System" = "IT_system",
                                 "Visualization" = "visualization", 
                                 "Dissemination" = "dissem", 
                                 "Analytics" = "analytics", 
                                 "Update Frequency" = "updates", 
                                 "Accessibility" = "accessibility"),
                               selected = "conditions"),
                   selectInput("keyword.num",
                               label = "Number of keywords to display:",
                               seq(10, 50, 10)),
                   selectInput("keyword.chart",
                               label = "Chart Type:",
                               c("Bar Chart", "Treemap"),
                               selected = "Bar Chart")
                   
            ),
            column(9, br(), br(),
                   strong(textOutput("key_freq.titles")),
                   plotOutput("FreqPlot"),
                   br()
            )
        )
    ),
    
    ############################ KEYWORD TABLE ################################
    
    fluidRow(
        wellPanel(
            h3("Keyword Frequency Table"),
            column(3, br(), br(),
                   
                   helpText('Type your keyword or phrase of interest and choose your table parameters.
                                 then click "Go!" to display your table.'),
                   
                   selectInput("keynum",
                               label = "Number of words in key phrase:",
                               c(1, 2, 3),
                               selected = 1),
                   
                   conditionalPanel(condition = "input.keynum == '1'",
                                    textInput("text.string",
                                              label = "Keyword:",
                                              value = "influenza")),
                   
                   conditionalPanel(condition = "input.keynum == '2' | input.keynum == '3'",
                                    textInput("text.string1",
                                              label = "Keyword 1:",
                                              value = "infectious")),
                   
                   conditionalPanel(condition = "input.keynum == '2' | input.keynum == '3'",
                                    textInput("text.string2",
                                              label = "Keyword 2:",
                                              value = "disease")),
                   
                   conditionalPanel(condition = "input.keynum == '3'",
                                    textInput("text.string3",
                                              label = "Keyword 3:",
                                              value = "outbreak")),
                   
                   selectInput("text.var",
                               label = "Category:",
                               c("Conditions" = "conditions",
                                 "Data Source" = "data_source",
                                 "Submission Mechanism" = "data_submitted_via",
                                 "IT System" = "IT_system",
                                 "Visualization" = "visualization",
                                 "Dissemination" = "dissem",
                                 "Analytics" = "analytics",               
                                 "Updates" = "updates"),
                               selected = "conditions"),
                   
                   conditionalPanel(condition = "input.keynum == '1'", 
                                    checkboxGroupInput('show_vars.keyword', 
                                                       'Categories to show in table:',
                                                       cat.names,
                                                       selected = cat.names)),
                   
                   conditionalPanel(condition = "input.keynum == '2' | input.keynum == '3'", 
                                    checkboxGroupInput('show_vars.keyword2', 
                                                       'Categories to show in table:',
                                                       cat.names2,
                                                       selected = cat.names2)),
                   
                   actionButton("goButton", "Go!"),
                   br(), br(), br()
                   
            ),
            column(9, br(), br(),                                 
                   strong(textOutput("keyword.sysnum", 
                                     inline = TRUE)),
                   br(),
                   dataTableOutput("Keyword.Table")
            )
        )
    ),
    
    ############################### HOVER CHART ######################################    
    
    fluidRow(
        wellPanel(
            h3("Filter by Date Est."), 
            
            column(3, 
                   br(), br(),
                   selectInput("yr_cat.var",
                               label = "Category:",
                               c("Current Status" = "status",
                                 "System Type" = "type", 
                                 "Species", 
                                 "Purpose" = "surv_category",
                                 "Geographic Coverage" = "geo_cover"),
                               selected = "status"),
                   
                   uiOutput("catValues"),
                   
                   sliderInput("yr.range",
                               label = "Select Year Range",
                               min = 1900,
                               max = 2015,
                               value = c(1900, 2015),
                               step = 1,
                               sep = "")
     
            ),
            column(9,
                   br(), br(),
                   strong(textOutput("ggvisTitle")),
                   plotOutput("yrPlot")
            )
            
        )
    ),
    
    fluidRow(
        column(9,
               br(), br(),
               dataTableOutput("yr_sys.tbl")
               )
        ),
    
    ############################### UNKS TABLE ######################################    
    
    fluidRow(
        wellPanel(
            h3("Systems with Unknown Categories"),
            
            column(10,
                   br(), br(),
                   strong(textOutput("unks.text")),
                   br(),
                   dataTableOutput("unks.tbl")
                   )
            )
        )
    
    
))