library(shiny)
library(deSolve)
library(highcharter)
library(magrittr)
library(shinyanimate)
library(shinyjs)      # autoplay helper
library(rclipboard)   # copy‑to‑clipboard button

# -------------------- Beginner‑friendly deSolve example --------------------
code_snippet <- "## First steps with deSolve: a constant inflow–outflow model\n\n# 1) Load the ODE solver package\nlibrary(deSolve)\n\n# 2) Set the parameters you can play with\nN0         <- 50   # initial population size\nbirth_rate <- 1    # births per time‑unit (constant)\ndeath_rate <- 0    # deaths per time‑unit (constant)\n\n# 3) Write the differential equation dN/dt = births − deaths\nstock_model <- function(t, state, parms){\n  with(as.list(c(state, parms)), {\n    dN <- birth_rate - death_rate  # derivative\n    list(dN)                       # deSolve expects a list\n  })\n}\n\n# 4) Choose the time points where you want the solution\ntimes <- 0:50   # shorthand for seq(0, 50, by = 1)\n\n# 5) Pack the initial state and parameters into named vectors\nstate <- c(N = N0)\nparms <- c(birth_rate = birth_rate, death_rate = death_rate)\n\n# 6) Integrate the ODE\nout <- ode(y = state, times = times, func = stock_model, parms = parms) |>\n       as.data.frame()\n\n# 7) Plot the trajectory\nplot(out$time, out$N, type = 'l',\n     xlab = 'Time', ylab = 'Population (N)')"  
# ---------------------------------------------------------------------------

ui <- fluidPage(
  rclipboardSetup(),
  useShinyjs(),
  withAnim(),
  titlePanel("Stock–Flow Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0", "Initial Population (N₀)", 0, 500, 50),
      sliderInput("birth_const", "Births per unit time", 0, 10, 1),
      sliderInput("death_const", "Deaths per unit time", 0, 10, 0),
      sliderInput("tmax", "Simulation time", 0, 200, 50),
      tags$div(style = "text-align:center; margin:20px 0;",
               actionButton("run", "Run Simulation")),
      sliderInput("time_slider", "Time", min = 0, max = 0, value = 0, step = 1,
                  animate = animationOptions(interval = 20, loop = FALSE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation", highchartOutput("popChart", height = "500px")),
        tabPanel("R Code",
                 rclipButton("copy_code", "Copy code", clipText = code_snippet,
                             icon = icon("clipboard")),
                 tags$pre(style = "white-space:pre-wrap; word-wrap:break-word;", code_snippet)
        )
      )
    )
  ),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

# --------------------------------- model -----------------------------------
stock_model <- function(time, state, parms){
  with(as.list(c(state, parms)), list(birth_const - death_const))
}

# -------------------------------- server -----------------------------------
server <- function(input, output, session){
  
  # compute trajectory when button pressed
  traj_df <- eventReactive(input$run, {
    parms <- c(birth_const = input$birth_const,
               death_const  = input$death_const)
    state <- c(N = input$N0)
    times <- seq(0, input$tmax, 1)
    df <- as.data.frame(ode(state, times, stock_model, parms))
    
    # reset slider
    updateSliderInput(session, "time_slider", value = 0, min = 0, max = input$tmax)
    
    # autoplay: click the (hidden) play button after DOM update
    runjs("setTimeout(function(){ var b=document.querySelector('.slider-animate-button'); if(b) b.click(); }, 400);")
    
    df
  })
  
  # initial empty chart
  output$popChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line", animation = FALSE) %>%
      hc_plotOptions(series = list(animation = list(duration = 0))) %>%
      hc_title(text = "Population vs Time") %>%
      hc_xAxis(title = list(text = "Time"), min = 0, max = input$tmax) %>%
      hc_yAxis(title = list(text = "Population (N)")) %>%
      hc_add_series(id = "line_series", data = list(), name = "N", marker = list(enabled = FALSE))
  })
  
  # update y‑axis after each run
  observeEvent(traj_df(), {
    highchartProxy("popChart") %>%
      hcpxy_update(yAxis = list(max = max(traj_df()$N)))
  })
  
  # animate line as slider moves
  observeEvent(input$time_slider, {
    df <- traj_df(); req(df)
    pts <- Map(function(x, y) list(x = x, y = y),
               df$time[df$time <= input$time_slider],
               df$N[df$time <= input$time_slider])
    highchartProxy("popChart") %>%
      hcpxy_update_series(id = "line_series", data = pts, animation = FALSE)
  })
}

shinyApp(ui, server)
