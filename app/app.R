library(shiny)
library(shinydashboard)

# load static data ##############################################
    expenditure500 <- readRDS("expenditure_over_500.RDS")

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
                menuItem("View data in table", tabName = "table", icon = icon("table"))
            )
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
