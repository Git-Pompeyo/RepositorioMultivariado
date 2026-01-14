library(shiny)
library(tidyverse)
library(krulRutils)
library(latex2exp)


ui <- fluidPage(
  fluidRow(
    column(
      3,
      h2("Instrucciones"),
      HTML("<p>Esta aplicación nos muestra la gráfica de dos funciones...</p>"),
      hr(),
      h2("Datos de Entrada"),
      numericInput("xmin", "Valor mínimo de x:", -15),
      numericInput("xmax", "Valor máximo de x:", 15),
      numericInput("ymin", "Valor mínimo de y:", -2),
      numericInput("ymax", "Valor máximo de y:", 2),
      numericInput("ncoef", "Número de coeficientes:", 0, min = 0),
      actionButton("calc", "Calcular")
    ),
    column(
      9,
      plotOutput("plot", height = "600px")
    )
  )
)


server <- function(input, output, session) {
  # In Qt you only compute after button click; mimic that with eventReactive
  params <- eventReactive(input$calc,
    {
      validate(
        need(input$xmin < input$xmax, "x mínimo debe ser menor que x máximo."),
        need(input$ymin < input$ymax, "y mínimo debe ser menor que y máximo."),
        need(input$ncoef >= 0, "Número de coeficientes debe ser ≥ 0.")
      )
      list(
        xmin = input$xmin, xmax = input$xmax,
        ymin = input$ymin, ymax = input$ymax,
        ncoef = input$ncoef
      )
    },
    ignoreInit = TRUE
  )

  output$plot <- renderPlot({
    req(params())

    p <- params()
    x <- seq(p$xmin, p$xmax, length.out = 800)

    # Placeholder example: "analytic function" + "Taylor approx"
    # Replace these with your actual function and Taylor polynomial.
    f <- sin(x)
    approx <- x # crude placeholder

    plot(x, f, type = "l", xlab = "x", ylab = "y", ylim = c(p$ymin, p$ymax))
    lines(x, approx, lty = 2)
    legend("topright", legend = c("Función", "Aprox. Taylor"), lty = c(1, 2), bty = "n")
  })
}

shinyApp(ui, server)
