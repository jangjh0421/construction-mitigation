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
                column(width = 8,
                       # Monthly Visitor Levels Plot
                       box(
                         title = "Monthly Visitor Levels (%) Relative to 2019",
                         plotlyOutput("monthlyPlot", height = "60vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "Chart Description",
                         HTML("<p><b>Monthly Visitor Levels (%) Relative to 2019</b></p>
                              <p>This chart measures the number of visits to buildings within the defined study area month by month, comparing the
                              number of visits in 2019 to the same month of the current year to produce a relative percentage, with 100%, meaning
                              the target month reached pre-pandemic levels.</p>"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 8,
                       # Visitor Type Plot
                       box(
                         title = "Quarterly Visit Count by Type of Visitor",
                         plotlyOutput("visitorTypePlot", height = "75vh"),
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                  
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "Chart Description",
                         HTML("<p><b>Quarterly Visit Count by Type of Visitor</b></p>
                              <p>This chart measures the quarterly total of visits to buildings within the defined study area for the target
                              and previous year based on the type of visitor.</p>
                              <p>
                              Visitors are placed into three groups using the common daytime and evening given by the Environics Analytics Mobile G.P.S. Data.
                              <ul>
                                <li>Common Daytime Location refers to the most common location of a device between 9 am and 5 pm. Used to infer work location.</li>
                                <li>Common Evening Location refers to the most common location of a device between 5 pm and 9 am. Used to infer home location.</li>
                              </ul>
                              </p>
                              <p>
                              <ul>
                                <li><b>Resident:</b> If a visitor's Common Evening Location <b>is within</b> 1 kilometre of the study area.</li>
                                <li><b>Recurring Visitor:</b> If a visitor's Common Daytime Location <b>is within</b> 1 kilometre of the study area <b>and</b> Common Evening
                                Location <b>is not within</b> 1 kilometre of the study area.</li>
                                <li><b>Infrequent Visitor:</b> If a visitor's Common Daytime Location <b>is not within</b> 1 kilometre of the study area and
                                Common Evening Location <b>is not within</b> 1 kilometre of the study area</li>
                              </ul>
                              </p>"
                              ),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 8,
                       # Day of Week Plot
                       box(
                         title = "Quarterly Visits by Day of the Week",
                         plotlyOutput("dayOfWeekPlot", height = "55vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "Chart Description",
                         HTML("<p><b>Quarterly Visits by Day of the Week</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and
                              previous year based on the day of the week.</p>"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                
                
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 8,
                       # Time of Day Plot
                       box(
                         title = "Quarterly Vists by Time of Day",
                         plotlyOutput("timeOfDayPlot", height = "55vh"),
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "Chart Description",
                         HTML("<p><b>Quarterly Visits by Time of Day</b></p>
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and
                              previous year based on the time of day</p>.
                              <p>
                              <ul>
                                <li>Early Morning: 12 am - 6 am</li>
                                <li>Morning: 6 am - 12 pm</li>
                                <li>Afternoon: 12 pm - 6 pm</li>
                                <li>Evening: 6 pm - 12 pm</li>
                              </ul>
                              </p>"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 8,
                       # Visitor Level Summary Table
                       box(
                         title = "Visitor Level Summary Table",
                         tableOutput("vistorLevelsTable"),
                         height = "30vh",
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                  
                ),
                column(width = 4,
                       # Description Panel
                       box(
                         title = "Chart Description",
                         HTML("<p><b>Visitor Level Summary Table</b></p>
                              <p>This table provides the percentage change of Visitor Levels, for the target quarter compared to
                              four distinct temporal baseline</p>
                              <p>
                              <ul>
                                <li><b>Year Over Year:</b> Same quarter of the previous year</li>
                                <li><b>Construction Start:</b> Same quarter of the pre-construction year (2022)</li>
                                <li><b>Pre Pandemic:</b> Same quarter of the pre-pandemic year (2019)</li>
                                <li><b>Last Quarter:</b> The last quarter of the same year</li>
                              </ul>
                              </p>"),
                         width = 12,
                         style = "margin: 0px; padding: 0px;"
                       )
                )
              )
      ),
      tabItem(tabName = "realEstate"
        
      )
    )
  )
)