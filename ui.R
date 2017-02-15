fluidPage(
  titlePanel('Monitoraggio temperatura e umidità'),
  sidebarLayout(
    
    sidebarPanel(
    
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Jan 16",
                              "Feb 16",
                              "Mar 16")
      #,downloadButton('downloadData', 'Download')
    ),
    
    dateRangeInput("daterange1", "Date range:",
                   start = "2016-01-01",
                   end   = "2016-12-31"),
    
       actionButton("update", "Update range")
    
    
    ),
    
    mainPanel(
      tabsetPanel(
      
        tabPanel("Data", dataTableOutput('table')),
        tabPanel("Charts", plotOutput("plot")),
        tabPanel("DyChart",dygraphOutput("dygraph"))
        
        )
    )
    
  )
)