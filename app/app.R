library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# load static data ##############################################
    expenditure500 <- readRDS("expenditure_over_500.RDS")
    unique_subject_descriptions <- unique(expenditure500$subject_description) 

# Define UI #####################################################
ui <- 
    dashboardPage(skin = "black",
        
        # header                    
        dashboardHeader(title = "Expenditure over £500",
                        titleWidth = 250), 
        
        # sidebar       
        dashboardSidebar(
        # tab selector menu
            sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("info")),
                menuItem("View data in table", tabName = "table", icon = icon("table")),
                menuItem("Chart", tabName = "chart", icon = icon("chart-line"))
            ),
            sliderInput(inputId = "select_filedate", 
                        label = "Select file date range", 
                        min = min(expenditure500$file_date), 
                        max = max(expenditure500$file_date),
                        value = c(min(expenditure500$file_date),
                                  max(expenditure500$file_date))),
            selectInput(inputId = "select_subject_description",
                        label = "Select subject description",
                        choices = unique_subject_descriptions,
                        selected = head(unique_subject_descriptions, 1),
                        multiple = TRUE,
                        selectize = TRUE)
        ), 
         
        # page body           
        dashboardBody(
            tabItems(
                
                # about
                tabItem(tabName = "about",
                    h2("About this dashboard"),
                    p("This dashboard displays open data from Bolton Council on expenditure over £500. ",
                    a("expenditure over £500", 
                      href = "https://www.bolton.gov.uk/downloads/download/196/expenditure_reports",
                      target = "_blank")),
                    p("Code for this app & data preparation is on my ",
                      a("github",
                        href = "https://github.com/shanwilkinson2/expenditure_over_500",
                        target = "_blank"))
                ),
                
                # table data
                tabItem(tabName = "table",
                    downloadButton("bttn_alldata", "Download all data"),
                    DT::DTOutput("alldata_table")
                ),
                
                # chart
                tabItem(tabName = "chart",
                    plotlyOutput("summary_filtered_expenditure500_plot")
                )
            )
        )
                    
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # generate data for download button
    output$bttn_alldata <- 
        downloadHandler(filename = "expenditure_over_500.csv",
                        # create file for downloading
                        content = function(file){
                            write.csv(expenditure500,
                                      file)
                        })
    
    # create table to view all data
    output$alldata_table <- DT::renderDT({
        expenditure500
    },
    filter = "top", rownames = FALSE, extensions = "Buttons",
    options = list(dom = "Bprti", # order of buttons/ filter etc
                   buttons = c("copy", "csv", "excel")
                   )
    )
    
    # filtered dataset using filter tools in sidebar
    filtered_expenditure500 <- reactive({
        expenditure500 %>%
            filter(between(file_date, input$select_filedate[1], input$select_filedate[2]),
                   subject_description %in% input$select_subject_description)
    })
    
    # summarised version of filtered_dataset using filters in sidebar
    summary_filtered_expenditure500 <- reactive({
        filtered_expenditure500() %>%
            group_by(file_date) %>%
            summarise(total_spend = sum(amount))
    })
    
    # create plotly bar chart
    output$summary_filtered_expenditure500_plot <- renderPlotly({
        summary_filtered_expenditure500() %>%
            plot_ly(x = ~file_date,
                    y = ~total_spend) %>%
            add_bars() %>%
            layout(title = ("Total expenditure on selected subject descriptions"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
