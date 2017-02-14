fluidPage(
  titlePanel('MOB: T and HR'),
  sidebarLayout(
    
    sidebarPanel(
    
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Jan 16",
                              "Feb 16")
      #,downloadButton('downloadData', 'Download')
      
    )
    ),
    
    mainPanel(
      tabsetPanel(
      
        tabPanel("Data", dataTableOutput('table')),
        tabPanel("Charts", plotOutput("plot"))
        
        )
    )
    
  )
)