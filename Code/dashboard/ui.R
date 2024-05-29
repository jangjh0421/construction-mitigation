dashboardPage(
  dashboardHeader(title = "CUI | City of Toronto BIA", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # side bar tabs
      menuItem(text = "Visitor Levels", tabName = "visitorLevels"),
      menuItem(text = "Commercial Real Estate", tabName = "realEstate"),
      # drop down inputs
      selectInput("bia", "Select BIA", choices = c("Downtown West", "Downtown Yonge", "Financial District", "Greektown", "Leslieville",
                                                   "Liberty Village", "Pape Village", "Queen Street", "Riverside", "St Lawrence Market", "West Queen West"), selected = "Downtown West", selectize = FALSE),
      selectInput("year", "Select Year", choices = c("2023"), selected = "2023", selectize = FALSE),
      selectInput("quarter", "Select Quarter", choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q4", selectize = FALSE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML(".box-body {height: auto !important; }"))
    ),
    tabItems(
      # Visitor Levels Tab
      tabItem(tabName = "visitorLevels",
              fluidRow(
                style = "margin: 0px; padding 0px;",
                column(width = 12,
                       # Monthly Visitor Levels Plot
                       box(
                         title = "Monthly Visitor Levels (%) Relative to 2019",
                         "This chart measures the number of visits to buildings within the defined study area month by month, comparing the number of visits in 2019 to the same month of the current year to produce a relative percentage, with 100%, meaning the target month reached pre-pandemic levels.",
                         solidHeader = TRUE,
                         plotlyOutput("monthlyPlot", height = "100%"),
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 12,
                       # Visitor Type Plot
                       box(
                         title = "Quarterly Visit Count by Type of Visitor",
                         HTML("
                              <p>This chart measures the quarterly total of visits to buildings within the defined study area for the target
                              and previous year based on the type of visitor.
                              Visitors are placed into three groups using the common daytime and evening locations from the Environics Analytics MobileScapes Data.
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
                         solidHeader = TRUE,
                         plotlyOutput("visitorTypePlot", height = "100%"),
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em;"
                       )
                  
                )
              ),
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 12,
                       # Day of Week Plot
                       box(
                         title = "Quarterly Visits by Day of the Week",
                         "This chart measures the quarterly total count of visits to buildings within the defined study area for the target and previous year based on the day of the week.",
                         solidHeader = TRUE,
                         plotlyOutput("dayOfWeekPlot", height = "100%"),
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 12,
                       # Time of Day Plot
                       box(
                         title = "Quarterly Vists by Time of Day",
                         HTML("
                              <p>This chart measures the quarterly total count of visits to buildings within the defined study area for the target and
                              previous year based on the time of day.</p>"
                              ),
                         solidHeader = TRUE,
                         plotlyOutput("timeOfDayPlot", height = "100%"),
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 12,
                       # Visitor Level Summary Table
                       box(
                         title = "Visitor Level Summary Table",
                         HTML("
                              <p>This table provides the percentage change of Visitor Levels, for the target quarter compared to
                              four distinct temporal baseline.</p>
                              <ul>
                                <li><b>Year Over Year:</b> Same quarter of the previous year</li>
                                <li><b>Construction Start:</b> Same quarter of the pre-construction year (2022)</li>
                                <li><b>Pre Pandemic:</b> Same quarter of the pre-pandemic year (2019)</li>
                                <li><b>Last Quarter:</b> The last quarter of the same year</li>
                              </ul>"),
                         solidHeader = TRUE,
                         gt::gt_output("visitorLevelsTable"),
                         height = "50vh",
                         width = 12,
                         style = "margin: 0px; padding: 5px;"
                       )
                  
                )
              )
      ),
      tabItem(tabName = "realEstate",
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 6,
                       # Vacancy Rate Table
                       box(
                        title = "Vacancy Rate Summary Table",
                        "The amount of new/relet/sublet space available divided by the existing rental building area for each Business Improvement Area.",
                        solidHeader = TRUE,
                        gt::gt_output("vacancyRateTable"),
                        width = 12,
                        style = "display: flex; flex-direction: column; padding: 1em;margin-bottom: 0px;"
                       )
                ),
                column(width = 6,
                       # Monthly Rent Table
                       box(
                         title = "Monthly Rent Summary Table",
                         "The average cost of rent per square meter.",
                         solidHeader = TRUE,
                         gt::gt_output("monthlyRentTable"),
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em;margin-bottom: 0px;"
                       )
                )
              ),
              fluidRow(
                style = "margin: 0px; padding: 0px;",
                column(width = 12,
                       box(
                         title = "BIA Retail Map",
                         solidHeader = TRUE,
                         width = 12,
                         style = "display: flex; flex-direction: column; padding: 1em",
                         leafletOutput("retailMap", width = "100%")
                       )
                )
              )
      )
    )
  )
)