library(shiny)
library(tidyverse)
library(krulRutils)
library(latex2exp)

ui <- fluidPage(
  tags$head(
    tags$title("Aproximaciones con series de Taylor"),

    # Some light CSS to mimic separators + spacing
    tags$style(HTML("
      .panel-top { margin-top: 10px; }
      .h-sep { border-top: 3px solid #ddd; margin: 12px 0; }
      .v-sep { border-left: 3px solid #ddd; height: 100%; }
      .left-box { padding-right: 16px; }
      .right-box { padding-left: 16px; }
      .form-row { margin-bottom: 10px; }
      /* Optional: keep content aligned to top like AlignTop */
      .row.equal-height { display: flex; align-items: flex-start; }
    "))
  ),

  # Main Layout (QHBoxLayout)
  fluidRow(
    class = "equal-height",

    # Left panel (stretch ~2)
    column(
      width = 4,
      class = "left-box panel-top",

      # Instructions QLabel (HTML)
      HTML("
        <h2>Instrucciones</h2>
        <p>Esta aplicación nos muestra la gráfica de dos funciones. La primera es la función analítica que deben de aproximar y la segunda es su aproximación utilizando series de Taylor.</p>
        <p>Para generar la gráfica, necesitamos el rango de los valores de <i>x</i>, el rango de los valores de <i>y</i>, además del número de coeficientes que se utilizarán en la aproximación por series de Taylor.</p>
      "),
      div(class = "h-sep"),

      # "Datos de Entrada" header
      HTML("<h2>Datos de Entrada</h2>"),

      # Inputs (QFormLayout equivalent)
      fluidRow(
        column(
          12,
          div(
            class = "form-row",
            numericInput("xmin", "Valor mínimo de x:", value = -15, step = 0.1)
          ),
          div(
            class = "form-row",
            numericInput("xmax", "Valor máximo de x:", value = 15, step = 0.1)
          ),
          div(
            class = "form-row",
            numericInput("ymin", "Valor mínimo de y:", value = -2, step = 0.1)
          ),
          div(
            class = "form-row",
            numericInput("ymax", "Valor máximo de y:", value = 2, step = 0.1)
          ),
          div(
            class = "form-row",
            numericInput("ncoef", "Número de coeficientes:", value = 0, min = 0, step = 1)
          ),

          # Submit button
          actionButton("calc", "Calcular")
        )
      )
    ),

    # Vertical separator (QFrame VLine)
    column(
      width = 1,
      div(
        class = "v-sep",
        style = "
          margin: 10px auto;
          height: 600px;
        "
      )
    ),

    # Right panel (stretch ~4)
    column(
      width = 7,
      class = "right-box panel-top",
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
