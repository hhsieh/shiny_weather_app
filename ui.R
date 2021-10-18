ui <- fluidPage(
    titlePanel("KBS weather summary"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          
          column(6,
                 radioButtons("var", h3("choose a variable to dislay"),
                              choices = list("precipitation" = "precipitation", 
                                             "temperature" = "temperature"),
                              selected = "precipitation")),
          
          column(6,
                 radioButtons("mt", h3("choose a month to highlight"),
                              choices = list("Jan" = "Jan", "Feb" = "Feb", "Mar" = "Mar", 
                                             "Apr" = "Apr", "May" = "May", "Jun" = "Jun",
                                             "Jul" = "Jul", "Aug" = "Aug", "Sep" = "Sep",
                                             "Oct" = "Oct", "Nov" = "Nov", "Dec" = "Dec"),
                              selected = "Jan")),
          
          column(6, 
                 textInput("year", h3("choose a year from 1988 to 2021 to compare with the long-term trend"), 
                           value = "2020")) 
          
        )
      ),
      mainPanel(
        plotOutput(outputId = "allPlot"),
        plotOutput(outputId = "comparePlot")
        
      )
    )
  )
  