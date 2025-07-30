library(shiny)
library(deSolve)
library(highcharter)
library(magrittr)
library(shinyanimate)
library(shinyjs)      # autoplay helper
library(rclipboard)   # copy‑to‑clipboard button

# ---------- pedagogic snippet shown in the R Code tab -----------------------
code_snippet <- "
## Simulating a constant inflow–outflow model in base R
## (Simply copy-paste this code into a new R script)
##
## Why an ODE?
## An ordinary differential equation (ODE) describes how a state
## variable changes continuously over time. Here the population size N
## changes according to two constant rates:
##   dN/dt = births - deaths
## The package *deSolve* integrates this ODE numerically so that we can
## plot N(t) without doing the calculus by hand.

# 1) Load the ODE solver package
library(deSolve)

# 2) Define the parameters for the simulation
N0         <- 50   # initial population size
birth_rate <- 1    # births per time‑unit (constant inflow)
death_rate <- 0    # deaths per time‑unit (constant outflow)

# 3) Write the ODE: dN/dt = births − deaths
stock_model <- function(t, state, parms){
  with(as.list(c(state, parms)), {
    dN <- birth_rate - death_rate   # derivative
    list(dN)                        # deSolve expects a list
  })
}

# 4) Choose the time points where the numerical solution is needed
times <- 0:50    # equivalent to seq(0, 50, by = 1)

# 5) Pack initial state and parameters into named vectors
state <- c(N = N0)
parms <- c(birth_rate = birth_rate,
           death_rate = death_rate)

# 6) Integrate the ODE
out <- ode(y = state, times = times,
           func = stock_model, parms = parms) |>
       as.data.frame()

# 7) Plot the trajectory
plot(out$time, out$N, type = 'l',
     xlab = 'Time', ylab = 'Population (N)')
"
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
        tabPanel("Simulation",
                 highchartOutput("popChart", height = "500px")),
        
        # ----------- Help tab -------------------------------------------------
        tabPanel("Help",
                 tags$h4("Paramètres"),
                 tags$style(".param {margin-bottom: 6px;}"),
                 tags$dl(
                   tags$div(class = "param",
                            tags$dt(tags$code("N₀")),
                            tags$dd("Population initiale au temps 0.")
                   ),
                   tags$div(class = "param",
                            tags$dt(tags$code("Births per unit time")),
                            tags$dd("Flux constant d’entrées (naissances ou immigration).")
                   ),
                   tags$div(class = "param",
                            tags$dt(tags$code("Deaths per unit time")),
                            tags$dd("Flux constant de sorties (mortalité ou émigration).")
                   ),
                   tags$div(class = "param",
                            tags$dt(tags$code("Simulation time")),
                            tags$dd("Durée totale de la simulation (unités entières).")
                   )
                 ),
                 tags$p(
                   tags$strong("Run Simulation :"),
                   "calcule toute la trajectoire avec les valeurs sélectionnées, ",
                   "met à jour la durée maximale du curseur « Time », puis lance automatiquement ",
                   "l’animation du graphique."
                 )
        ),
        # ---------------------------------------------------------------------
        
        tabPanel("R Code",
                 rclipButton("copy_code", "Copy code", clipText = code_snippet,
                             icon = icon("clipboard")),
                 tags$pre(style = "white-space:pre-wrap; word-wrap:break-word;",
                          code_snippet)
        )
      )
    )
  ),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

# --------------------------------- ODE model --------------------------------
stock_model <- function(time, state, parms){
  with(as.list(c(state, parms)), list(birth_const - death_const))
}

# --------------------------------- server -----------------------------------
server <- function(input, output, session){
  
  traj_df <- eventReactive(input$run, {
    parms <- c(birth_const = input$birth_const,
               death_const  = input$death_const)
    state <- c(N = input$N0)
    times <- seq(0, input$tmax, 1)
    df <- as.data.frame(ode(state, times, stock_model, parms))
    
    updateSliderInput(session, "time_slider",
                      value = 0, min = 0, max = input$tmax)
    
    # autoplay
    runjs("setTimeout(function(){
             var b=document.querySelector('.slider-animate-button');
             if(b) b.click();
           }, 400);")
    df
  })
  
  output$popChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line", animation = FALSE) %>%
      hc_plotOptions(series = list(animation = list(duration = 0))) %>%
      hc_title(text = "Population vs Time") %>%
      hc_xAxis(title = list(text = "Time"),
               min = 0, max = input$tmax) %>%
      hc_yAxis(title = list(text = "Population (N)")) %>%
      hc_add_series(id = "line_series", data = list(),
                    name = "N", marker = list(enabled = FALSE))
  })
  
  observeEvent(traj_df(), {
    highchartProxy("popChart") %>%
      hcpxy_update(yAxis = list(max = max(traj_df()$N)))
  })
  
  observeEvent(input$time_slider, {
    df <- traj_df(); req(df)
    pts <- Map(function(x, y) list(x = x, y = y),
               df$time[df$time <= input$time_slider],
               df$N[df$time <= input$time_slider])
    highchartProxy("popChart") %>%
      hcpxy_update_series(id = "line_series",
                          data = pts, animation = FALSE)
  })
}

shinyApp(ui, server)
