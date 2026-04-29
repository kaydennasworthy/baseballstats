# 0. Libraries
library(shiny)
library(tidyverse)
library(plotly)

# 1. Data
sc_bos_daily <- readRDS("../data/statcast_2026_boston_daily.rds")
DDAY <- as.Date("2026-04-25")

# 2. Config
all_metrics <- c(
  "Bat Speed"     = "avg_bat_speed",
  "Launch Angle"  = "avg_launch_angle",
  "Launch Speed"  = "avg_launch_speed",
  "Swing Length"  = "avg_swing_length",
  "Hard Hit %"    = "hard_hit_pct",
  "AVG"           = "AVG",
  "OBP"           = "OBP",
  "SLG"           = "SLG",
  "OPS"           = "OPS"
)

# 3. Precompute pre/post summary (team aggregate)
# Done outside server so it doesn't recompute on every interaction
pre_post_summary <- sc_bos_daily %>%
  pivot_longer(
    cols      = any_of(unname(all_metrics)),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(period = if_else(game_date <= DDAY, "pre", "post")) %>%
  group_by(metric, period) %>%
  summarise(
    mean  = mean(value, na.rm = TRUE),
    se    = sd(value, na.rm = TRUE) / sqrt(n()),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    xmin  = if_else(period[1] == "pre", min(sc_bos_daily$game_date), DDAY),
    xmax  = if_else(period[1] == "pre", DDAY, max(sc_bos_daily$game_date)),
    .groups = "drop"
  )

# 4. UI
ui <- fluidPage(
  titlePanel("BOS Batter Statcast Metrics"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "metrics",
        "Select up to 6 metrics:",
        choices  = all_metrics,
        selected = c("avg_bat_speed", "avg_launch_speed", "hard_hit_pct")
      ),
      hr(),
      checkboxGroupInput(
        "players",
        "Filter players:",
        choices  = sort(unique(sc_bos_daily$player_name)),
        selected = unique(sc_bos_daily$player_name)
      ),
      hr(),
      actionButton("select_all",   "Select All Players"),
      actionButton("deselect_all", "Deselect All Players"),
      hr(),
      checkboxInput("show_dday",    "Show Cora Firing Date",  value = TRUE),
      checkboxInput("show_pre_ci",  "Show Pre-firing Mean/CI",  value = FALSE),
      checkboxInput("show_post_ci", "Show Post-firing Mean/CI", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("metric_plot", height = "700px")
    )
  )
)

# 5. Server
server <- function(input, output, session) {
  
  observe({
    if (length(input$metrics) > 6) {
      updateCheckboxGroupInput(session, "metrics",
                               selected = head(input$metrics, 6)
      )
    }
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "players",
                             selected = sort(unique(sc_bos_daily$player_name))
    )
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "players", selected = character(0))
  })
  
  output$metric_plot <- renderPlotly({
    
    req(input$metrics, input$players)
    
    metric_labels <- setNames(names(all_metrics), all_metrics)
    
    # Filter summary to selected metrics only, with pretty labels
    summary_filtered <- pre_post_summary %>%
      filter(metric %in% input$metrics) %>%
      mutate(metric = metric_labels[metric])
    
    plot_data <- sc_bos_daily %>%
      filter(player_name %in% input$players) %>%
      pivot_longer(
        cols      = any_of(input$metrics),
        names_to  = "metric",
        values_to = "value"
      ) %>%
      mutate(metric = metric_labels[metric])
    
    p <- ggplot() +
      # CI bands first so they sit behind lines
      { if (input$show_pre_ci)
        geom_rect(
          data = filter(summary_filtered, period == "pre"),
          aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper),
          fill = "steelblue", alpha = 0.15, inherit.aes = FALSE
        )
      } +
      { if (input$show_pre_ci)
        geom_segment(
          data = filter(summary_filtered, period == "pre"),
          aes(x = xmin, xend = xmax, y = mean, yend = mean),
          color = "steelblue", linewidth = 0.8, linetype = "dashed",
          inherit.aes = FALSE
        )
      } +
      { if (input$show_post_ci)
        geom_rect(
          data = filter(summary_filtered, period == "post"),
          aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper),
          fill = "darkorange", alpha = 0.15, inherit.aes = FALSE
        )
      } +
      { if (input$show_post_ci)
        geom_segment(
          data = filter(summary_filtered, period == "post"),
          aes(x = xmin, xend = xmax, y = mean, yend = mean),
          color = "darkorange", linewidth = 0.8, linetype = "dashed",
          inherit.aes = FALSE
        )
      } +
      # Player lines on top
      geom_path(
        data = plot_data,
        aes(x = game_date, y = value, color = player_name, group = player_name,
            text = paste0(player_name, "\n",
                          format(game_date, "%b %d"), "\n",
                          round(value, 2))),
        linewidth = 0.8
      ) +
      geom_point(
        data = plot_data,
        aes(x = game_date, y = value, color = player_name, group = player_name,
            text = paste0(player_name, "\n",
                          format(game_date, "%b %d"), "\n",
                          round(value, 2))),
        size = 1.5
      ) +
      { if (input$show_dday)
        geom_vline(xintercept = DDAY, linetype = "dashed",
                   color = "red", linewidth = 0.8)
      } +
      { if (input$show_dday)
        annotate("text", x = DDAY, y = Inf,
                 label = "Cora fired", hjust = -0.1, vjust = 1.5,
                 color = "red", size = 3)
      } +
      facet_wrap(~ metric, scales = "free_y", ncol = 2) +
      labs(x = NULL, color = "Player") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        legend     = list(orientation = "h")
      )
  })
}

# 6. Run
shinyApp(ui, server)