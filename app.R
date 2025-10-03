library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(TrafikVerketR)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),

  titlePanel("Train Station Information"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("station_code",
                "Station Code:",
                value = "Cst",
                placeholder = "e.g., Cst, Lp, Söd"),

      numericInput("hours",
                   "Hours to look back for delays:",
                   value = 6,
                   min = 1,
                   max = 24,
                   step = 1),

      actionButton("get_data", "Get Station Data",
                   class = "btn-primary"),

      br(), br(),

      wellPanel(
        h5("Common Station Codes:"),
        tags$ul(
          tags$li("Cst - Stockholm Central"),
          tags$li("Söd - Stockholm Södra"),
          tags$li("Lp - Linköping"),
          tags$li("Gb - Göteborg Central"),
          tags$li("Mc - Malmö"),
          tags$li("U - Uppsala")
        )
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Departures",
                 h3("Upcoming Departures"),
                 DT::dataTableOutput("departures_table")),

        tabPanel("Delays",
                 h3("Recent Delays"),
                 DT::dataTableOutput("delays_table")),

        tabPanel("Summary",
                 h3("Delay Summary"),

                 # Summary metrics as simple text outputs
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("Average Delay"),
                            textOutput("avg_delay_text")
                          )
                   ),
                   column(4,
                          wellPanel(
                            h4("Total Trains"),
                            textOutput("total_trains_text")
                          )
                   ),
                   column(4,
                          wellPanel(
                            h4("Canceled Trains"),
                            textOutput("canceled_trains_text")
                          )
                   )
                 ),

                 plotOutput("delay_histogram"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive value to store all data
  station_data <- reactiveValues(
    departures = NULL,
    delays = NULL
  )

  # Get data when button is clicked
  observeEvent(input$get_data, {
    req(input$station_code)

    # Show loading
    showNotification("Fetching station data...", type = "message", duration = 3)

    tryCatch({
      # Get departures (This uses the second tibble structure you provided)
      station_data$departures <- get_d(input$station_code)

      # Get delays (This uses the first tibble structure you provided)
      station_data$delays <- get_delay(input$station_code, hours = input$hours)

      showNotification("Data loaded successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })

  # 1. Departures Table Output (Using the column names: Owner, Time, Track, From, To)
  output$departures_table <- DT::renderDataTable({
    req(station_data$departures)

    # The data frame returned by get_d is likely using the second structure you provided:
    # Owner, Time, Track, From, To
    station_data$departures %>%
      select(
        Owner, # The column name is 'Owner'
        Time,  # The column name is 'Time'
        Track,
        From,
        To
      ) %>%
      rename(
        `Operator` = Owner,
        `Scheduled Time` = Time
        # Track, From, and To are fine as is
      ) %>%
      DT::datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
  })

  # 2. Delays Table Output (Using the column names: train_id, actual_time, canceled, delay_minutes, from, to)
  output$delays_table <- DT::renderDataTable({
    req(station_data$delays)

    # The data frame returned by get_delay is using the first structure you provided:
    # train_id, actual_time, canceled, delay_minutes, from, to
    station_data$delays %>%
      mutate(
        # Format the time
        ActualTime_Display = strftime(actual_time, format = "%H:%M:%S"), # <--- Using actual_time
        # Create a status column
        Status = case_when(
          canceled ~ "Canceled",
          delay_minutes > 0 ~ paste0("Delayed ", delay_minutes, " min"),
          TRUE ~ "On Time"
        )
      ) %>%
      select(
        train_id,
        from,
        to,
        ActualTime_Display, # The newly formatted time
        Status
      ) %>%
      rename(
        `Train ID` = train_id,
        From = from,
        To = to,
        `Actual Time` = ActualTime_Display
      ) %>%
      DT::datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
  })

  # 3. Summary Metrics as text outputs (These were mostly correct, but updated for 'delay_minutes' and 'canceled')
  output$avg_delay_text <- renderText({
    req(station_data$delays)

    if (nrow(station_data$delays) == 0) {
      return("0 min")
    }

    valid_delays <- station_data$delays %>%
      filter(!canceled, !is.na(delay_minutes)) # <--- Using 'canceled' and 'delay_minutes'

    if (nrow(valid_delays) == 0) {
      "0 min"
    } else {
      avg_delay <- round(mean(valid_delays$delay_minutes, na.rm = TRUE), 1)
      paste(avg_delay, "minutes")
    }
  })

  output$total_trains_text <- renderText({
    req(station_data$delays)
    as.character(nrow(station_data$delays))
  })

  output$canceled_trains_text <- renderText({
    req(station_data$delays)

    if (nrow(station_data$delays) == 0) {
      "0"
    } else {
      as.character(sum(station_data$delays$canceled, na.rm = TRUE)) # <--- Using 'canceled'
    }
  })

  # 4. Delay Histogram Plot Output (Updated for 'delay_minutes' and 'canceled')
  output$delay_histogram <- renderPlot({
    req(station_data$delays)

    # Filter out canceled trains and NA delays, and only keep positive delays
    plot_data <- station_data$delays %>%
      filter(!canceled, !is.na(delay_minutes), delay_minutes > 0) # <--- Using 'canceled' and 'delay_minutes'

    if (nrow(plot_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No recent delays to plot.", size = 6))
    }

    # Create the histogram
    ggplot(plot_data, aes(x = delay_minutes)) +
      geom_histogram(binwidth = 5, fill = "#3498db", color = "white") + # flatly theme blue
      labs(
        title = paste("Delay Distribution for", input$station_code),
        x = "Delay in Minutes",
        y = "Count of Trains"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

