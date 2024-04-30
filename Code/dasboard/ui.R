dashboardPage(
  dashboardHeader(title = "City of Toronto BIA Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      # side bar tabs
      menuItem(text = "Visitor Levels", tabName = "visitorLevels"),
      menuItem(text = "Commercial Real Estate", tabName = "realEstate"),
      # drop down inputs
      selectInput("bia", "Select BIA", choices = c("Downtown West", "Downtown Yonge", "Financial District", "Gerrard India Baz", "Greektown", "Leslieville",
                                                   "Liberty Village", "Pape Village", "Queen Street", "Riverside", "St Lawrence Market", "West Queen West")),
      selectInput("year", "Select Year", choices = c("2023")),
      selectInput("quarter", "Select Quarter", choices = c("Q1", "Q2", "Q3", "Q4"))
    )
  ),
  dashboardBody(
    tabItems(
      # Visitor Levels Tab
      tabItem(tabName = "visitorLevels",
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 9,
                       # Monthly Visitor Levels Plot
                       box(
                         title = "Monthly Visitor Levels (%) Relative to 2019",
                         plotOutput("monthlyPlot", height = "50vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                ),
                column(width = 3,
                       # Description Panel
                       box(
                         title = "How to Read: Visitor Level Measures",
                         HTML("<p><b>Visits by Day of Week:</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and previous year based on the day of the week.</p>
                              <p><b>Visits by Time of Day:</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and previous year based on the time of the day.</p>
                              <p><b>Visitor Levels (%) Relative to 2019</b></p>
                              <p>This chart measures the number of visits to buildings within the defined study area month by month, comparing the number of visits in 2019 to the same month of
                              the current year to produce a relative percentage, with 100%, meaning the target month reached pre-pandemic levels.</p>
                              <p><b>Visitor Levels Summary</b></p>
                              <p>This table provides the percentage change of Visitor Levels, as defined in the previous chart, for the quarter to four distinct temporal baselines.
                              <ul>
                                <li>same quarter of the previous year</li>
                                <li>same quarter of the pre-construction year (2022)</li>
                                <li>same quarter of the pre-pandemic year</li>
                                <li>the last quarter of the same year</li>
                              </ul>"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(  
                column(width = 10,
                       # Day of Week Plot
                       box(
                         title = "Visits by Day of Week",
                         plotOutput("dayOfWeekPlot", height = "50vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                ),
                
                column(width = 4,
                       # Visitor Level Table
                       box(
                         title = "Visitor Levels Summary",
                         tableOutput("vistorLevelsTable"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 4,
                       # Time of Day Plot
                       box(
                         title = "Vists by Time of Day",
                         plotOutput("timeOfDayPlot", height = "50vh"),
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                ),
                column(width = 4,
                       # Visitor Type Plot
                       box(
                         title = "Visit Count by Type of Visitor",
                         plotOutput("VisitorTypePlot", height = "50vh"),
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "How to Read: Visitor Level Measures",
                         HTML("<p><b>Visits by Day of Week:</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and previous year based on the day of the week.</p>
                              <p><b>Visits by Time of Day:</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and previous year based on the time of the day.</p>
                              <p><b>Visitor Levels (%) Relative to 2019</b></p>
                              <p>This chart measures the number of visits to buildings within the defined study area month by month, comparing the number of visits in 2019 to the same month of
                              the current year to produce a relative percentage, with 100%, meaning the target month reached pre-pandemic levels.</p>
                              <p><b>Visitor Levels Summary</b></p>
                              <p>This table provides the percentage change of Visitor Levels, as defined in the previous chart, for the quarter to four distinct temporal baselines.
                              <ul>
                                <li>same quarter of the previous year</li>
                                <li>same quarter of the pre-construction year (2022)</li>
                                <li>same quarter of the pre-pandemic year</li>
                                <li>the last quarter of the same year</li>
                              </ul>"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              )
              
              
      ),
      tabItem(tabName = "realEstate"
        
      )
    )
  )
)