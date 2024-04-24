

# Shiny Dashboard test environment

# load packages
library(dplyr)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(tidyverse)

# load in the construction mitigation datasets
input_directory = "C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters/ConstructionMitigation/"
ff_monthly = read_csv(paste0(input_directory, "Output/OutputData/ff_monthly_meta.csv")) %>%
  select(-...1)
ff_quarter = read_csv(paste0(input_directory, "Output/OutputData/ff_table_meta.csv")) %>%
  select(-...1)
ff_day_of_week = read_csv(paste0(input_directory, "Output/OutputData/ff_day_of_week_meta.csv")) %>%
  select(-...1)
ff_time_of_day = read_csv(paste0(input_directory, "Output/OutputData/ff_time_of_day_meta.csv")) %>%
  select(-...1)
traveltime = read_csv(paste0(input_directory, "Output/OutputData/tt_grouped_meta.csv")) %>%
  select(-...1)
marketrent = read_csv(paste0(input_directory, "Output/OutputData/marketrent_meta.csv")) %>%
  select(-...1)
vacancyrate = read_csv(paste0(input_directory, "Output/OutputData/vacancyrate_meta.csv")) %>%
  select(-...1)



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    selectInput("bia", "Select BIA", choices = c("Downtown West", "Downtown Yonge", "Financial District", "Gerrard India Baz", "Greektown", "Leslieville",
                                                 "Liberty Village", "Pape Village", "Queen Street", "Riverside", "St Lawrence Market", "West Queen West")),
    selectInput("year", "Select Year", choices = c("2023")),
    selectInput("quarter", "Select Quarter", choices = c("Q1", "Q2", "Q3", "Q4"))
  ),
  dashboardBody(
    fluidPage(
    # Add main dashboard content here
      fluidRow(
        style = "margin: 0px; padding: 0px;",
        column(width = 5,
               box(
                 title = "Foot Traffic (%) Relative to 2019",
                 plotOutput("ffMonthlyPlot", height = "400px"),
                 width = 12,
                style = "margin: 0px; padding: 0px;" 
               )
        ),
        column(width = 3, 
               style = "margin 0px; padding: 0px;",
               box(
                 title = "Visitor Levels",
                 tableOutput("ffTable"),
                 height = 200,
                 width = 12,
                 style = "margin: 0px; padding: 0px;" 
               ),
               box(
                 title = "Travel Time Change",
                 tableOutput("travelTimeTable"),
                 height = 200,
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               )
        ),
        column(width = 4,
               style = "margin: 0px; padding: 0px;",
               box(
                 title = "Sales Volume Growth",
                 plotOutput("salesPlot", height = "300px"),
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               ),
               fluidRow(
                 column(width = 6,
                        style = "margin: 0px; padding: 0px;",
                        box(
                          title = "Retail Sales (BIAs)",
                          tableOutput("retailBiaPlot"),
                          height = 100,
                          width = 12,
                          style = "margin: 0px; padding: 0px;"
                        )
                 ),
                 column(width = 6,
                        style = "margin: 0px; padding: 0px;",
                        box(
                          title = "Retail Sales (CMA)",
                          tableOutput("retailCmaPlot"),
                          height = 100,
                          width = 12,
                          style = "margin: 0px; padding: 0px;"
                        )
                 ),
               )
        )
      ),
      fluidRow(
        style = "margin 0px; padding: 0px;",
        column(width = 4,
               box(
                 title = "Visits by Day of Week",
                 plotOutput("dayOfWeekPlot", height = "200px"),
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               )
        ),
        column(width = 4,
               box(
                 title = "Visits by Time of Day",
                 plotOutput("timeOfDayPlot", height = "200px"),
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               )
        ),
        column(width = 4,
               style = "margin 0px; padding: 0px;",
               box(
                 title = "Real Time Local Business Index",
                 tableOutput("rtlbiTable"),
                 height = 100,
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               ),
               box(
                 title = "Market Rent",
                 tableOutput("marketRentTable"),
                 height = 50,
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               ),
               box(
                 title = "Vacancy Rate",
                 tableOutput("vacancyRateTable"),
                 height = 50,
                 width = 12,
                 style = "margin: 0px; padding: 0px;"
               )
        )
      )
    )
  )  
)

# Define server
server <- function(input, output) {
  # output for the monthly foot traffic chart based on the BIA name
  output$ffMonthlyPlot = renderPlot({
    ff_monthly %>%
      filter(Area == str_replace_all(input$bia, " ", "")) %>%
      ggplot(aes(x = date, y = Percentage)) +
      geom_line(size = 1.5, color = "#00AEF3") +
      ylim(0, 180) +
      labs(title = "Foot Traffic (%) Relative to 2019", x = "Month", y = "Percentage (%)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_line(color = 'gray80'),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_blank(),
        axis.title.y = element_text(size = 5),
        axis.title.x = element_blank(),
        legend.position = "none")
  })
  
  output$dayOfWeekPlot = renderPlot({
    ff_day_of_week %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
               (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) %>%
      ggplot(aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                                 y = Visits, fill = as.character(Year))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.7) +
      scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
      labs(title = "Visits by Day of Week", x = "Day of the Week", y = "Visits") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_line(color = 'gray80'),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 4),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom")
  })
  
  output$timeOfDayPlot = renderPlot({
    ff_time_of_day %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
               (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) %>%
      ggplot(aes(x = factor(Time, levels = c("12am - 6am", "6am - 9am", "9am - 12pm", "12pm - 3pm", "3pm - 6pm", "6pm - 9pm", "9pm - 12am")),
                                 y = Visits, fill = as.character(Year))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.7) +
      scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
      labs(title = "Visits by Time of Day", x = "Time of Day", y = "Visits") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_line(color = 'gray80'),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 4),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom")
  })
  
  output$ffTable = renderTable({
    ff_quarter_table = ff_quarter %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
               (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) %>%
      select(time_window, Change)
    colnames(ff_quarter_table) = c("Time Window", "% Change")
    ff_quarter_table
  })
  
}

# Run the application
shinyApp(ui, server)