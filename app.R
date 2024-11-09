library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)
library(stringr)
library(tibble)
library(bslib)

ui <- fluidPage(
  titlePanel("Nebraska Car Crash Analysis"),
  tags$head(
    tags$style(HTML("
      .custom-card {
        width: 220px;
        height: 110px;
        border: 2px solid #000000;
        border-radius: 10px;
        padding: 10px;
        box-shadow: 2px 2px 12px rgba(0, 0, 0, 0.1); 
      }
      .custom-header {
        font-size: 24px;
        font-weight: bold;
      }
      .custom-text {
        font-size: 30px;
        font-weight: bold;
      }
    "))
  ),
  card(
    class = "custom-card",
    card_header(tags$div(class = "custom-header", "Total Fatalities")),
    card_body(
      tags$div(class = "custom-text", textOutput("total_fatalities"))
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("mapType", "Map Filter:",
                  choices = c("County", "County (Per Capita)")),
      selectInput("barType", "Bar Plot Filter:",
                  choices = c("Day of the Week", "Month", 
                              "Time of Day", "Intersection Type")),
      selectInput("pieType", "Pie Chart Filter:",
                  choices = c("Light Condition" = "LGT_CONDNAME", 
                              "Weather" = "WEATHERNAME", 
                              "Relation to Junction" = "RELJCT2NAME", 
                              "Intersection Type" = "TYP_INTNAME", 
                              "Work Zone" = "WRK_ZONENAME", 
                              "Relation to Road" = "REL_ROADNAME"))
    ),
    mainPanel(
      plotOutput("mapPlot"),
      plotOutput("barPlot"),
      plotOutput("piePlot")
    )
  )
)


server <- function(input, output) {
  crash_data <- read.csv("accident_NE.csv", header = TRUE)
  
  nebraska_data <- crash_data %>%
    filter(STATENAME == "Nebraska") %>%
    mutate(COUNTYNAME = str_extract(COUNTYNAME, "^[^\\(]+"),
           COUNTYNAME = str_trim(COUNTYNAME),
           COUNTYNAME = str_to_title(COUNTYNAME))
  
  crash_summary <- nebraska_data %>%
    group_by(COUNTYNAME) %>%
    summarize(fatalities = sum(FATALS))
  
  nebraska_counties <- counties(state = "NE", cb = TRUE, class = "sf")
  
  nebraska_map <- nebraska_counties %>%
    left_join(crash_summary, by = c("NAME" = "COUNTYNAME"))
  
  crashes_by_day <- nebraska_data %>%
    group_by(DAY_WEEKNAME) %>%
    summarize(count = n())
  
  crashes_by_month <- nebraska_data %>%
    group_by(MONTHNAME) %>%
    summarize(count = n())
  
  crash_hour <- crash_data %>%
    mutate(TIME_PERIOD = case_when(
      ARR_HOUR >= 0 & ARR_HOUR <= 5 ~ "Night",
      ARR_HOUR >= 6 & ARR_HOUR <= 11 ~ "Morning",
      ARR_HOUR >= 12 & ARR_HOUR <= 17 ~ "Afternoon",
      ARR_HOUR >= 18 & ARR_HOUR <= 23 ~ "Evening",
      ARR_HOUR == 99 ~ "Unknown",
      TRUE ~ "Other"
    ))
  
  crashes_by_time_period <- crash_hour %>%
    group_by(TIME_PERIOD) %>%
    summarize(count = n())
  
  nebraska_population_2022 <- tibble::tibble(
    County = c("Douglas", "Lancaster", "Sarpy", "Hall", "Buffalo", "Dodge", "Madison", "Scotts Bluff", "Platte", "Lincoln",
               "Adams", "Cass", "Dawson", "Saunders", "Gage", "Dakota", "Washington", "Seward", "Otoe", "Saline",
               "York", "Box Butte", "Colfax", "Custer", "Red Willow", "Holt", "Wayne", "Hamilton", "Cheyenne",
               "Phelps", "Cuming", "Butler", "Knox", "Cedar", "Keith", "Dawes", "Merrick", "Richardson",
               "Pierce", "Nemaha", "Jefferson", "Kearney", "Burt", "Thurston", "Howard", "Antelope",
               "Clay", "Stanton", "Fillmore", "Cherry", "Dixon", "Boone",
               "Polk", "Johnson",
               "Sheridan",
               "Thayer",
               "Furnas",
               "Morrill",
               "Nuckolls",
               "Valley",
               "Chase",
               "Webster",
               "Kimball",
               "Nance",
               "Harlan",
               "Sherman",
               "Brown",
               "Franklin",
               "Perkins","Frontier","Pawnee","Hitchcock","Greeley","Gosper","Deuel","Garden","Garfield","Boyd",
               "Dundy","Rock","Sioux","Hayes","Keya Paha","Wheeler","Banner","Thomas","Hooker","Logan",
               "Loup","Grant","Blaine","Arthur","McPherson"),
    Population = c(592974, 328998, 203345, 62298, 50862, 37329, 35840, 35629, 34863, 33116,
                   30851, 27740, 24265, 23749, 21704, 21420, 21137, 17717, 16484, 14562,
                   14400, 10701, 10679, 10658, 10386, 10173, 9849, 9635, 9556,
                   9126, 8883, 8475, 8289, 8158, 8047, 8014, 7795, 7648,
                   7281,7116,6968,6834,6684,6559,6545,6299,
                   6183,5947,5555,5499,5466,5254,
                   5216,5151,
                   4850,
                   4804,
                   4542,
                   4486,
                   4121,
                   3966,
                   3694,
                   3363,
                   3242,
                   3231,
                   3058,
                   2960,
                   2823,
                   2800,
                   2763,
                   2559,
                   2493,
                   2490,
                   2203,
                   1859,
                   1841,
                   1753,
                   1739,
                   1720,
                   1527,
                   1301,
                   1181,
                   828,
                   805,
                   768,
                   691,
                   689,
                   676,
                   625,
                   583,
                   546,
                   420,
                   399,
                   387)
  )
  
  crash_summary_per_capita <- crash_summary %>%
    left_join(nebraska_population_2022, by = c("COUNTYNAME" = "County")) %>%
    mutate(fatalities_per_capita = fatalities / Population * 100000)
  
  nebraska_map_per_capita <- nebraska_counties %>%
    left_join(crash_summary_per_capita, by = c("NAME" = "COUNTYNAME"))
  
  column_titles <- c(
    LGT_CONDNAME = "Light Condition",
    WEATHERNAME = "Weather",
    RELJCT2NAME = "Relation to Junction",
    TYP_INTNAME = "Intersection Type",
    WRK_ZONENAME = "Work Zone",
    REL_ROADNAME = "Relation to Road"
  )
  
  output$total_fatalities <- renderText({
    sum(nebraska_data$FATALS)
  })
  
  output$mapPlot <- renderPlot({
    if (input$mapType == "County") {
      ggplot(nebraska_map) +
        geom_sf(aes(fill = fatalities)) +
        scale_fill_gradient(low = "yellow", high = "red", na.value = "yellow") +
        labs(title = "Nebraska Car Crash Fatalities by County (2022)",
             fill = "Fatalities") +
        theme_minimal()
    } else if (input$mapType == "County (Per Capita)") {
      ggplot(nebraska_map_per_capita) +
        geom_sf(aes(fill = fatalities_per_capita)) +
        scale_fill_gradient(low = "yellow", high = "red", na.value = "yellow") +
        labs(title = "Nebraska Car Crash Fatalities per Capita (2022)",
             fill = "Fatalities per 100,000") +
        theme_minimal()
    }
  })
  
  output$barPlot <- renderPlot({
    if (input$barType == "Day of the Week") {
      ggplot(crashes_by_day, aes(x = DAY_WEEKNAME, y = count)) +
        geom_bar(stat = "identity", fill = "orchid") +
        labs(title = "Car Crashes in Nebraska by Day of the Week (2022)",
             x = "Day of the Week",
             y = "Number of Crashes") +
        theme_minimal()
    } else if (input$barType == "Month") {
      ggplot(crashes_by_month, aes(x = MONTHNAME, y = count)) +
        geom_bar(stat = "identity", fill = "orchid") +
        labs(title = "Car Crashes in Nebraska by Month (2022)",
             x = "Month",
             y = "Number of Crashes") +
        theme_minimal() +
        scale_x_discrete(limits = month.name)
    } else if (input$barType == "Time of Day") {
      ggplot(crashes_by_time_period, aes(x = TIME_PERIOD, y = count)) +
        geom_bar(stat = "identity", fill = "orchid") +
        labs(title = "Car Crashes in Nebraska by Time of Day (2022)",
             x = "Time Period",
             y = "Number of Crashes") +
        theme_minimal()
    } else if (input$barType == "Intersection Type") {
      ggplot(nebraska_data, aes(x = TYP_INTNAME)) +
        geom_bar(fill = "orchid") +
        labs(title = "Distribution of Crashes by Intersection Type in Nebraska (2022)",
             x = "Intersection Type",
             y = "Number of Crashes") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$piePlot <- renderPlot({
    pie_data <- nebraska_data %>%
      group_by(!!sym(input$pieType)) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    ggplot(pie_data, aes(x = "", y = percentage, fill = !!sym(input$pieType))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = paste("Distribution of Crashes by", column_titles[[input$pieType]]),
           fill = column_titles[[input$pieType]]) +
      theme_minimal()
  })
}
shinyApp(ui=ui,server=server)
