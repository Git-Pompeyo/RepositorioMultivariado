library(shiny)
library(tidyverse)
library(krulRutils)
library(latex2exp)
library(greybox)

ui <- fluidPage(
  withMathJax(),
  fluidRow(
    column(
      width = 3,
      h2("Instrucciones"),
      p("Esta aplicación nos muestra la gráfica de la función de densidad de probabilidad (PDF) de una distribución normal generalizada con parámetros \\(\\mu\\), \\(\\alpha\\) y \\(\\beta\\)."),
      p("Además, calcula y muestra las características principales de la distribución normal generalizada: media, varianza, asimetría y curtosis excesiva."),
      p("Para usar esta aplicación, ingresa los valores deseados para los parámetros \\(\\mu\\), \\(\\alpha\\), \\(\\beta\\) junto con el valor mínimo y máximo de \\(X\\) que desees visualizar, y luego haz clic en el botón 'Calcular' para actualizar la gráfica y las características calculadas."),
      hr(),
      h2("Datos de Entrada"),
      numericInput("mu", "Parámetro \\(\\mu\\):", 0),
      numericInput("alpha", "Parámetro \\(\\alpha\\):", 1),
      numericInput("beta", "Parámetro \\(\\beta\\):", 2),
      numericInput("xmin", "Valor mínimo de \\(X\\):", -4),
      numericInput("xmax", "Valor máximo de \\(X\\):", 4),
      actionButton("calc", "Calcular"),
      hr(),
      h2("Características principales"),
      uiOutput("summary_text")
    ),
    column(
      width = 7,
      h2("Función de densidad de probabilidad de la distribución normal generalizada"),
      plotOutput("plot", height = "600px")
    )
  )
)


server <- function(input, output, session) {
  params <- eventReactive(input$calc,
    {
      validate(
        need(
          input$xmin < input$xmax,
          "El valor mínimo de x debe ser menor que el valor máximo de x."
        ),
        need(
          0 < input$alpha,
          "El valor de alpha debe ser mayor que 0."
        ),
        need(
          0 < input$beta,
          "El valor de beta debe ser mayor que 0."
        )
      )
      list(
        xmin = input$xmin,
        xmax = input$xmax,
        mu = input$mu,
        alpha = input$alpha,
        beta = input$beta,
        gnorm_mean = input$mu,
        gnorm_var =
          (input$alpha^2 * gamma(3 / input$beta)) / (gamma(1 / input$beta)),
        gnorm_skewness = 0,
        gnorm_kurtosis =
          (gamma(5 / input$beta) * gamma(1 / input$beta)) /
            (gamma(3 / input$beta)^2) - 3
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  output$plot <- renderPlot({
    # Extraemos los parámetros
    xmin <- params()$xmin
    xmax <- params()$xmax
    mu <- params()$mu
    alpha <- params()$alpha
    beta <- params()$beta

    # Creamos una secuencia de valores para X
    x_data <- seq(xmin, xmax, length.out = 1000)

    # Calculamos la PDF de la distribución Gamma
    gnorm_distribution_tbl <- tibble(
      x_data = x_data,
      density_data = dgnorm(x_data, mu = mu, scale = alpha, shape = beta)
    )
    # Generamos la gráfica de la distribución Gamma
    gnorm_distribution_plot <- gnorm_distribution_tbl |>
      ggplot(aes(x = x_data, y = density_data)) +
      geom_line(
        color = c_pal("C blue"),
        linewidth = 1
      ) +
      labs(
        x = TeX("$X$"),
        y = "Densidad"
      ) +
      theme_krul()

    # Devolvemos la gráfica
    gnorm_distribution_plot
  })

  output$summary_text <- renderUI({
    # Extraemos los parámetros
    alpha <- params()$alpha
    beta <- params()$beta
    gnorm_mean <- params()$gnorm_mean
    gnorm_var <- params()$gnorm_var
    gnorm_skewness <- params()$gnorm_skewness
    gnorm_kurtosis <- params()$gnorm_kurtosis

    # Calculamos las características principales
    HTML(paste0(
      "<strong>Media:</strong> ", round(gnorm_mean, 8), "<br>",
      "<strong>Varianza:</strong> ", round(gnorm_var, 8), "<br>",
      "<strong>Asimetría:</strong> ", round(gnorm_skewness, 8), "<br>",
      "<strong>Curtosis excesiva:</strong> ", round(gnorm_kurtosis, 8)
    ))
  })
}

shinyApp(ui, server)
