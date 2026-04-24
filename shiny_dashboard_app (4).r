library(shiny)
library(bslib)
library(readr)
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(plotly)
library(DT)
library(janitor)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Interactive Tibble Dashboard"),
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      fileInput(
        "file",
        "Upload a CSV or Excel file",
        accept = c(".csv", ".xlsx", ".xls")
      ),
      uiOutput("sheet_ui"),
      hr(),
      uiOutput("filter_var_ui"),
      uiOutput("filter_control_ui")
    ),
    navset_card_tab(
      nav_panel(
        "Overview",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Column Type Controls"),
            p("Convert columns in the uploaded tibble before visualization."),
            uiOutput("type_controls_ui")
          ),
          card(
            card_header("Tibble Preview"),
            DTOutput("data_table")
          )
        ),
        card(
          card_header("Tibble Structure"),
          verbatimTextOutput("structure_text")
        )
      ),
      nav_panel(
        "Visualization",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Plot Settings"),
            selectInput(
              "plot_type",
              "Plot type",
              choices = c(
                "Scatter" = "scatter",
                "Line" = "line",
                "Scatter + Smooth" = "scatter_smooth",
                "Line + Smooth" = "line_smooth",
                "Bar" = "bar",
                "Boxplot" = "boxplot",
                "Histogram" = "histogram",
                "Density" = "density"
              )
            ),
            uiOutput("x_var_ui"),
            uiOutput("y_var_ui"),
            uiOutput("color_var_ui"),
            uiOutput("shape_var_ui"),
            uiOutput("label_var_ui")
          ),
          card(
            card_header("Interactive ggplot"),
            plotlyOutput("ggplot_output", height = "560px")
          )
        )
      ),
      nav_panel(
        "Summary",
        card(
          card_header("Quick Summary"),
          verbatimTextOutput("summary_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  raw_tibble <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)

    df <- if (ext == "csv") {
      read_csv(input$file$datapath, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      sheet <- if (!is.null(input$sheet) && nzchar(input$sheet)) input$sheet else 1
      read_excel(input$file$datapath, sheet = sheet)
    } else {
      validate("Unsupported file type. Please upload a CSV or Excel file.")
    }

    df |>
      clean_names() |>
      as_tibble()
  })

  output$sheet_ui <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)

    if (ext %in% c("xlsx", "xls")) {
      sheets <- excel_sheets(input$file$datapath)
      selectInput("sheet", "Choose Excel sheet", choices = sheets)
    }
  })

  output$type_controls_ui <- renderUI({
    df <- raw_tibble()

    tagList(
      lapply(names(df), function(col) {
        current_type <- class(df[[col]])[1]

        selectInput(
          inputId = paste0("type_", col),
          label = paste0(col, " — current: ", current_type),
          choices = c(
            "Keep current" = "keep",
            "Number" = "number",
            "Factor" = "factor",
            "Date" = "date"
          ),
          selected = "keep"
        )
      })
    )
  })

  converted_tibble <- reactive({
    df <- raw_tibble()

    for (col in names(df)) {
      type_choice <- input[[paste0("type_", col)]]

      if (is.null(type_choice) || type_choice == "keep") next

      df[[col]] <- switch(
        type_choice,
        number = suppressWarnings(as.numeric(df[[col]])),
        factor = as.factor(df[[col]]),
        date = suppressWarnings(as.Date(df[[col]])),
        df[[col]]
      )
    }

    as_tibble(df)
  })

  output$filter_var_ui <- renderUI({
    df <- converted_tibble()
    selectInput(
      "filter_var",
      "Filter column",
      choices = c("None" = "", names(df)),
      selected = ""
    )
  })

  output$filter_control_ui <- renderUI({
    req(input$filter_var)
    if (input$filter_var == "") return(NULL)

    df <- converted_tibble()
    x <- df[[input$filter_var]]

    if (inherits(x, "Date")) {
      rng <- range(x, na.rm = TRUE)
      dateRangeInput(
        "filter_date_range",
        "Date range",
        start = rng[1],
        end = rng[2],
        min = rng[1],
        max = rng[2]
      )
    } else if (is.numeric(x)) {
      rng <- range(x, na.rm = TRUE)
      sliderInput(
        "filter_numeric_range",
        "Numeric range",
        min = floor(rng[1]),
        max = ceiling(rng[2]),
        value = c(floor(rng[1]), ceiling(rng[2]))
      )
    } else {
      vals <- unique(x)
      vals <- vals[!is.na(vals)]

      selectInput(
        "filter_values",
        "Filter values",
        choices = sort(as.character(vals)),
        selected = sort(as.character(vals)),
        multiple = TRUE
      )
    }
  })

  filtered_tibble <- reactive({
    df <- converted_tibble()

    if (!is.null(input$filter_var) && input$filter_var != "") {
      x <- df[[input$filter_var]]

      if (inherits(x, "Date") && !is.null(input$filter_date_range)) {
        df <- df |>
          filter(.data[[input$filter_var]] >= input$filter_date_range[1],
                 .data[[input$filter_var]] <= input$filter_date_range[2])
      } else if (is.numeric(x) && !is.null(input$filter_numeric_range)) {
        df <- df |>
          filter(.data[[input$filter_var]] >= input$filter_numeric_range[1],
                 .data[[input$filter_var]] <= input$filter_numeric_range[2])
      } else if (!is.numeric(x) && !inherits(x, "Date") && !is.null(input$filter_values)) {
        df <- df |>
          filter(as.character(.data[[input$filter_var]]) %in% input$filter_values)
      }
    }

    as_tibble(df)
  })

  output$data_table <- renderDT({
    datatable(
      filtered_tibble(),
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top"
    )
  })

  output$structure_text <- renderPrint({
    df <- filtered_tibble()
    glimpse(df)
  })

  output$x_var_ui <- renderUI({
    df <- filtered_tibble()
    selectInput("x_var", "X", choices = c("None" = "", names(df)), selected = names(df)[1])
  })

  output$y_var_ui <- renderUI({
    df <- filtered_tibble()
    selectInput("y_var", "Y", choices = c("None" = "", names(df)), selected = "")
  })

  output$color_var_ui <- renderUI({
    df <- filtered_tibble()
    selectInput("color_var", "Color", choices = c("None" = "", names(df)), selected = "")
  })

  output$shape_var_ui <- renderUI({
    df <- filtered_tibble()
    selectInput("shape_var", "Shape", choices = c("None" = "", names(df)), selected = "")
  })

  output$label_var_ui <- renderUI({
    df <- filtered_tibble()
    selectInput("label_var", "Label", choices = c("None" = "", names(df)), selected = "")
  })

  output$ggplot_output <- renderPlotly({
    df <- filtered_tibble()

    validate(
      need(nrow(df) > 0, "No rows available after filtering."),
      need(!is.null(input$plot_type), "Choose a plot type."),
      need(!is.null(input$x_var) && input$x_var != "", "Choose an X variable.")
    )

    # Build a clean plotting tibble with fixed column names. This avoids
    # ggplotly errors from duplicated aesthetic names such as colour/color.
    plot_df <- df |>
      mutate(
        .x_plot = .data[[input$x_var]],
        .tooltip_x = as.character(.data[[input$x_var]])
      )

    mapping <- aes(x = .x_plot)
    tooltip_fields <- c("x")

    if (!is.null(input$y_var) && input$y_var != "") {
      plot_df <- plot_df |>
        mutate(
          .y_plot = .data[[input$y_var]],
          .tooltip_y = as.character(.data[[input$y_var]])
        )
      mapping$y <- rlang::expr(.y_plot)
      tooltip_fields <- c(tooltip_fields, "y")
    }

    if (!is.null(input$color_var) && input$color_var != "") {
      plot_df <- plot_df |>
        mutate(.color_plot = .data[[input$color_var]])
      mapping$colour <- rlang::expr(.color_plot)
      tooltip_fields <- c(tooltip_fields, "colour")
    }

    if (!is.null(input$shape_var) && input$shape_var != "") {
      plot_df <- plot_df |>
        mutate(.shape_plot = as.factor(.data[[input$shape_var]]))
      mapping$shape <- rlang::expr(.shape_plot)
      tooltip_fields <- c(tooltip_fields, "shape")
    }

    if (!is.null(input$label_var) && input$label_var != "") {
      plot_df <- plot_df |>
        mutate(.label_plot = as.character(.data[[input$label_var]]))
      mapping$text <- rlang::expr(.label_plot)
      tooltip_fields <- c(tooltip_fields, "text")
    }

    p <- ggplot(plot_df, mapping)

    p <- switch(
      input$plot_type,
      scatter = {
        validate(need(input$y_var != "", "Scatter plots require a Y variable."))
        p + geom_point(size = 2.5, alpha = 0.8)
      },
      line = {
        validate(need(input$y_var != "", "Line plots require a Y variable."))
        p + geom_line() + geom_point(size = 1.8)
      },
      scatter_smooth = {
        validate(need(input$y_var != "", "Scatter + Smooth plots require a Y variable."))
        p + geom_point(size = 2.5, alpha = 0.8) + geom_smooth()
      },
      line_smooth = {
        validate(need(input$y_var != "", "Line + Smooth plots require a Y variable."))
        p + geom_line(alpha = 0.6) + geom_point(size = 1.8, alpha = 0.8) + geom_smooth()
      },
      bar = {
        if (!is.null(input$y_var) && input$y_var != "") {
          p + geom_col()
        } else {
          p + geom_bar()
        }
      },
      boxplot = {
        validate(need(input$y_var != "", "Boxplots require a Y variable."))
        p + geom_boxplot()
      },
      histogram = {
        p + geom_histogram(bins = 30)
      },
      density = {
        p + geom_density()
      }
    )

    p <- p +
      theme_minimal(base_size = 13) +
      labs(
        x = input$x_var,
        y = if (!is.null(input$y_var) && input$y_var != "") input$y_var else NULL,
        colour = if (!is.null(input$color_var) && input$color_var != "") input$color_var else NULL,
        shape = if (!is.null(input$shape_var) && input$shape_var != "") input$shape_var else NULL
      )

    ggplotly(p, tooltip = unique(tooltip_fields))
  })

  output$summary_text <- renderPrint({
    df <- filtered_tibble()
    summary(df)
  })
}

shinyApp(ui, server)
