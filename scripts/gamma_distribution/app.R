library(shiny)
library(tidyverse)
library(krulRutils)
library(latex2exp)


ui <- fluidPage(
  withMathJax(),
  fluidRow(
    column(
      width = 3,
      h2("Instrucciones"),
      p("Esta aplicación nos muestra la gráfica de la función de densidad de probabilidad (PDF) de una distribución Gamma con parámetros \\(\\alpha\\) y \\(\\beta\\)."),
      p("Además, calcula y muestra las características principales de la distribución Gamma: media, mediana, varianza, asimetría y curtosis excesiva."),
      p("Para usar esta aplicación, ingresa los valores deseados para los parámetros \\(\\alpha\\), \\(\\beta\\) y el valor máximo de \\(X\\) que desees visualizar, y luego haz clic en el botón 'Calcular' para actualizar la gráfica y las características calculadas."),
      hr(),
      h2("Datos de Entrada"),
      numericInput("alpha", "Parámetro \\(\\alpha\\):", 2),
      numericInput("beta", "Parámetro \\(\\beta\\):", 1),
      numericInput("xmax", "Valor máximo de \\(X\\):", 8),
      actionButton("calc", "Calcular"),
      hr(),
      h2("Características principales"),
      uiOutput("summary_text")
    ),
    column(
      width = 7,
      h2("Función de densidad de probabilidad de la distribución Gamma"),
      plotOutput("plot", height = "600px")
    )
  )
)


server <- function(input, output, session) {
  params <- eventReactive(input$calc,
    {
      validate(
        need(0 < input$xmax, "El valor máximo de x debe ser mayor que 0."),
        need(0 < input$alpha, "El valor de alpha debe ser mayor que 0."),
        need(0 < input$beta, "El valor de beta debe ser mayor que 0.")
      )
      list(
        xmax = input$xmax,
        alpha = input$alpha,
        beta = input$beta,
        gamma_mean = input$alpha / input$beta,
        gamma_median = qgamma(0.5, shape = input$alpha, rate = input$beta),
        gamma_var = input$alpha / (input$beta^2),
        gamma_skewness = 2 / sqrt(input$alpha),
        gamma_kurtosis = 6 / input$alpha
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  output$plot <- renderPlot({
    # Extraemos los parámetros
    alpha <- params()$alpha
    beta <- params()$beta
    xmax <- params()$xmax
    gamma_mean <- params()$gamma_mean
    gamma_median <- params()$gamma_median

    # Creamos una secuencia de valores para X
    x_data <- seq(0, xmax, length.out = 500)

    # Calculamos la PDF de la distribución Gamma
    gamma_distribution_tbl <- tibble(
      x_data = x_data,
      density_data = dgamma(x_data, shape = alpha, rate = beta)
    )
    # Generamos la gráfica de la distribución Gamma
    gamma_distribution_plot <- gamma_distribution_tbl |>
      ggplot(aes(x = x_data, y = density_data)) +
      geom_line(
        color = c_pal("C blue"),
        linewidth = 1
      ) +
      geom_vline(
        xintercept = gamma_mean,
        color = c_pal("C red"),
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = gamma_median,
        color = c_pal("C green"),
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = gamma_mean,
        y = 0.5,
        label = "Media",
        color = c_pal("C red"),
        hjust = -0.1
      ) +
      annotate(
        "text",
        x = gamma_median,
        y = 0.5,
        label = "Mediana",
        color = c_pal("C green"),
        hjust = 1.1
      ) +
      labs(
        x = TeX("$X$"),
        y = "Densidad"
      ) +
      theme_krul()

    # Devolvemos la gráfica
    gamma_distribution_plot
  })

  output$summary_text <- renderUI({
    # Extraemos los parámetros
    alpha <- params()$alpha
    beta <- params()$beta
    gamma_mean <- params()$gamma_mean
    gamma_median <- params()$gamma_median
    gamma_var <- params()$gamma_var
    gamma_skewness <- params()$gamma_skewness
    gamma_kurtosis <- params()$gamma_kurtosis

    # Calculamos las características principales
    HTML(paste0(
      "<strong>Media:</strong> ", round(gamma_mean, 8), "<br>",
      "<strong>Mediana:</strong> ", round(gamma_median, 8), "<br>",
      "<strong>Varianza:</strong> ", round(gamma_var, 8), "<br>",
      "<strong>Asimetría:</strong> ", round(gamma_skewness, 8), "<br>",
      "<strong>Curtosis excesiva:</strong> ", round(gamma_kurtosis, 8)
    ))
  })
}

shinyApp(ui, server)
