library(tidyverse)
library(glue)
library(ggiraph)
library(DT)
library(shiny)
library(shinydashboard)


theme_set(theme_minimal() +
            theme(text = element_text(size = 8)))

narrow_plot_px <- 110
narrow_plot_in <- 1.3

# Generate random datasets

# Generate random values for satisfaction and completion

get_satisfaction_completion <- function() {
  tramites_values <- c((95:100), rep(100, 15))
  sat_values <- rnorm(100, mean = 0.5, sd = 1)
  sat_values <- sat_values[sat_values < 2 & sat_values > -2]
  
  list(
    "Laboral" = c(
      "tramites" = glue("{sample(tramites_values, 1)}%"),
      "satisfaccion" = round(sample(sat_values, 1), 2)
    ),
    "Fiscal" = c(
      "tramites" = glue("{sample(tramites_values, 1)}%"),
      "satisfaccion" = round(sample(sat_values, 1), 2)
    )
  )
}

get_random_date <- function() {
  seq(as.Date('2024/01/01'), as.Date('2025/01/01'), by = "month") |>
    sample(1) |>
    as.POSIXct()
}

# Simulate productivity time series
get_productivity <- function(a_date = this_date) {
  tibble(# one datapoint by month
    date = date(seq(a_date - dmonths(12), a_date, by = "month")),
    # The value is the value of the day before plus a random number
    productivity =  100 + cumsum(rnorm(12, mean = 0,  sd = 5))) |>
    mutate(
      month = month(date, label = TRUE, abbr = TRUE),
      month = fct_reorder(month, date),
      tooltip = glue("{month}: {round(productivity, 2)}")
    ) |>
    arrange(month)
}

get_clients <- function(a_date = this_date) {
  tibble(date = date(seq(a_date - dmonths(12), a_date, by = "month")),
         clients = round(100 + cumsum(rnorm(12, 0, 10)), 0)) |>
    mutate(
      month = month(date, label = TRUE, abbr = TRUE),
      month = fct_reorder(month, date),
      tooltip = glue("{month}: {clients}")
    )
}

# Simulate a distribution of income per sector
get_sector_income <- function() {
  sectores_económicos <- c(
    "Agricultura",
    "Salud",
    "Cosméticos",
    "Tecnología",
    "Transporte",
    "Comercio",
    "Finanzas",
    "Educación",
    "Servicios"
  )
  
  tibble(
    sector = sample(
      sectores_económicos,
      100,
      replace = TRUE,
      prob = seq(1, 0.01, length.out = length(sectores_económicos))
    ),
    billings = rnorm(100, 1000, 800)
  )
}

# Generate company billing and hours worked data
get_profitability <- function() {
  tibble(
    Empresa = companies,
    Horas = round(sample(1:104, 100, replace = TRUE), 2),
    Facturación = round(Horas * rnorm(100, 100, 10), ),
    Rentabilidad = round(Facturación / Horas, 2)
  ) |>
    filter(Rentabilidad > 0) |>
    arrange(Rentabilidad) |>
    mutate(
      `Empresa cliente` = `Empresa`,
      Facturación = glue("{as.character(Facturación)} €"),
      `* Rentabilidad` = Rentabilidad, 
      ) |> 
    select(`Empresa cliente`, Facturación, `* Rentabilidad`)
}

# Generate data for small clients
get_small_clients <- function(this_month = this_month) {
  n_small_clients <- sample(round(rnorm(20, 20, 5)), 1)
  
  small_clients <- sample(companies, n_small_clients)
  
  tibble(
    company = small_clients,
    income_this_month = round(rnorm(n_small_clients, 1000, 500), 2),
    increment = round(rnorm(n_small_clients, 0, 500), 2),
    income_last_month = income_this_month - increment
  ) |>
    filter(income_last_month > 0 & income_this_month > 0) |>
    mutate(
      increment = round(income_this_month - income_last_month, 2),
      grew = ifelse(increment > 0, TRUE, FALSE),
      company = fct_reorder(company, income_this_month),
      tooltip = glue(
        "{company}
                     {levels(this_month)[as.numeric(this_month) - 1]}:\t {income_last_month} €
                     {this_month}:\t   {income_this_month} €
                     Inc: {increment} €
                     "
      )
    )
}

# Stategic clients
get_strategic_clients <- function() {
  n_strategic_clients <- sample(round(rnorm(20, 5, 2.5)), 1)
  
  strategic_clients <- sample(companies, n_strategic_clients)
  
  tibble(
    company = strategic_clients,
    new = sample(
      c("NUEVO", ""),
      n_strategic_clients,
      replace = TRUE,
      prob = c(0.2, 0.8)
    ),
    satisfaction = round(rnorm(n_strategic_clients, 1, 1), 0.5)
  ) |>
    filter(satisfaction < 2 & satisfaction > -2)
}

# Define UI for the application

body <- dashboardBody(
  width = 12,
  
  fluidRow(
    column(
      width = 2, 
      h4(textOutput("date"))
    ),
    column(
      width = 10, 
      h4("Los datos mostrados en este dashboard han sido generados aleatoriamente. Recarga la página para regenerarlo")
    )
  ),

fluidRow(
  column(
    width = 2,
    h2("Calidad"),
    box(
      width = NULL,
      title = "Laboral",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      # A value box
      valueBoxOutput("lab_tr", width = NULL),
      valueBoxOutput("lab_sat", width = NULL),
      textOutput("lab_footer")
    ),
    box(
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      title = "Fiscal-Contable",
      collapsible = TRUE,
      valueBoxOutput("fis_tr", width = NULL),
      valueBoxOutput("fis_sat", width = NULL),
      textOutput("fis_footer")
    )
  ),
  column(
    width = 5,
    h2("Viabilidad"),
    box(
      width = NULL,
      title = "Balance de clientes en el útimo año",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      girafeOutput("balance", height = narrow_plot_px)
    ),
    box(
      width = NULL,
      title = "Facturación por sector (este mes)",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      girafeOutput("sector",  height = narrow_plot_px)
    ),
    box(
      width = NULL,
      title = "Pequeños clientes",
      footer = "Incremento de sus impuestos respecto al mes anterior (€)",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      girafeOutput("small",  height = narrow_plot_px)
    ),
    box(
      width = NULL,
      title = "Satisfacción de clientes estratégicos",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      p("I. e, en zonas sin competidores"),
      dataTableOutput("strategic")
    )
  ),
  column(
    width = 5,
    h2("Productividad"),
    box(
      width = NULL,
      title = "Ingresos totales / Horas totales trabajadas",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      girafeOutput("productivity_plot", height = narrow_plot_px * 2)
    ),
    box(
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      title = "Clientes, por rentabilidad",
      p("Top clientes ordenados por rentabilidad ascendente este mes"),
      dataTableOutput("profitability"), 
      footer = "* Rentabilidad = Facturación del cliente / Horas de trabajo totales"
    )
  )
))

companies <- readLines("chatgpt_company-names.txt")

ui <-
  dashboardPage(dashboardHeader(title = glue("Aslafis: Dashboard")),
                dashboardSidebar(disable = TRUE),
                body)

# Define the dashboard server

server <- function(input, output) {
  
  this_date <- get_random_date()
  this_month <- month(this_date - ddays(1), label = TRUE)
  servicios_list <- get_satisfaction_completion()
  productivity_df <- get_productivity(this_date)
  clients_df <- get_clients(this_date)
  sector_df <- get_sector_income()
  total_billings <- sum(sector_df$billings)
  companies <- readLines("chatgpt_company-names.txt")
  profitability_df <- get_profitability()
  small_clients_df <- get_small_clients(this_month)
  strategic_clients_df <- get_strategic_clients()
  
  output$date <- renderText({
    as.character(date(this_date))
  })
  
  output$lab_tr <- renderValueBox({
    valueBox(
      value = servicios_list[["Laboral"]]["tramites"],
      subtitle = "Trámites completados dentro de plazo",
      color = if (servicios_list[["Laboral"]]["tramites"] == "100%")
        "green"
      else
        "red"
    )
  })
  
  output$fis_tr <- renderValueBox({
    valueBox(
      value = servicios_list[["Fiscal"]]["tramites"],
      subtitle = "Trámites completados dentro de plazo",
      color = if (servicios_list[["Fiscal"]]["tramites"] == "100%")
        "green"
      else
        "red"
    )
  })
  
  output$lab_sat <- renderValueBox({
    valueBox(
      value = servicios_list[["Laboral"]]["satisfaccion"],
      subtitle = "Satisfacción con las consultas",
      color = if (servicios_list[["Laboral"]]["satisfaccion"] > 0)
        "green"
      else
        "red"
    )
  })
  
  output$fis_sat <- renderValueBox({
    valueBox(
      value = servicios_list[["Fiscal"]]["satisfaccion"],
      subtitle = "Satisfacción con las consultas",
      color = if (servicios_list[["Fiscal"]]["satisfaccion"] > 0)
        "green"
      else
        "red"
    )
  })
  
  output$lab_footer <- renderText({
    glue("Rendimiento en {this_month}.")
  })
  
  output$fis_footer <- renderText({
    glue("Rendimiento en {this_month}.")
  })
  
  output$productivity_plot <- renderGirafe({
    # Replace this with the actual code to generate the productivity plot
    productivity_plot <- ggplot(productivity_df,
                                aes(x = month,
                                    y = productivity)) +
      geom_line(group = "") +
      geom_point_interactive(aes(tooltip = tooltip), size = 10, alpha = 0) +
      theme(axis.title = element_blank())
    
    girafe(ggobj = productivity_plot,
           width_svg = 5,
           height_svg = narrow_plot_in  * 2)
    
  })
  
  output$balance <- renderGirafe({
    clients_plot <- ggplot(clients_df, aes(x = month, y = clients)) +
      geom_line(group = "") +
      geom_point_interactive(aes(tooltip = tooltip),
                             size = 10,
                             alpha = 0) +
      # scale_x_continuous(breaks = 1:12, labels = month.abb) +
      theme(axis.title = element_blank())
    
    girafe(ggobj = clients_plot,
           width_svg = 5,
           height_svg = narrow_plot_in)
  })
  
  output$sector <- renderGirafe({
    sector_bar <-  sector_df |>
      group_by(sector) |>
      summarise(per_bill = sum(billings) /  total_billings * 100) |>
      mutate(sector = fct_reorder(sector, per_bill)) |>
      mutate(tooltip = glue("{sector}:\n {round(per_bill, 2)} %")) |>
      ggplot(aes(
        x = per_bill,
        y = "",
        fill = sector,
        tooltip = tooltip
      )) +
      geom_bar_interactive(stat = "identity",
                           alpha = 0,
                           color = "black") +
      geom_vline(xintercept = 20,
                 linetype = "dashed",
                 color = "red") +
      theme(legend.position = "none") +
      theme(axis.title.y = element_blank()) +
      xlab("% de la facturación total")
    
    girafe(ggobj = sector_bar,
           width_svg = 5,
           height_svg = narrow_plot_in)
  })
  
  
  output$profitability <- renderDataTable({
    datatable(profitability_df,
              options = list(pageLength = 5))
  })
  
  output$small <- renderGirafe({
    max_row <-
      summarise(small_clients_df, across(starts_with("income"), max))
    max_value <- max(as.numeric(as.vector(max_row[1, ])))
    
    small_clients_plot <- small_clients_df |>
      mutate(client_rank = as.numeric(company)) |>
      ggplot(aes(x = client_rank)) +
      geom_col_interactive(aes(y = max_value, tooltip = tooltip),
                           alpha = 0) +
      geom_segment(
        aes(
          xend = client_rank,
          y = income_last_month,
          yend = income_this_month,
          color = grew
        ),
        arrow = arrow(length = unit(1, "mm"), type = "closed"),
        linewidth = 1
      ) +
      scale_color_manual_interactive(
        values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
        labels = c("TRUE" = "Creció", "FALSE" = "Decreció")
      ) +
      scale_x_continuous(breaks = 1:nrow(small_clients_df)) +
      theme(axis.title = element_blank(),
            legend.position = "none")
    
    girafe(ggobj = small_clients_plot,
           width_svg = 5,
           height_svg = narrow_plot_in)
  })
  
  output$strategic <- renderDataTable({
    datatable(
      strategic_clients_df,
      colnames = NULL,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        dom = "t"
      )
    ) |>
      # If the new cell is NUEVO, color green
      formatStyle("new",
                  backgroundColor = styleEqual(c("NUEVO", ""),
                                               c("lightgreen", "background_color"))) |>
      # If the satisfaction cell is > 0, color green, else red
      formatStyle("satisfaction",
                  backgroundColor = styleInterval(c(0),
                                                  c("red", "lightgreen")))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
