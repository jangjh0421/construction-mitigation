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
                column(width = 6,
                       # Day of Week Plot
                       box(
                         title = "Visits by Day of Week",
                         plotOutput("dayOfWeekPlot", height = "50vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                ),
                column(width = 6,
                       # Monthly Visitor Levels Plot
                       box(
                         title = "Visitor Levels (%) Relative to 2019",
                         plotOutput("MonthlyPlot", height = "50vh"),
                         width = 12,
                         style = "margin 0px; padding 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 6,
                       # Time of Day Plot
                       box(
                         title = "Vists by Time of Day",
                         
                       )
                )
              )
              
              
      )
    )
  )
)