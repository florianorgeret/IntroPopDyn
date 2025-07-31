# --------------------------------------------------------------------------
# Minimal dependency-check: install missing packages then load them
# --------------------------------------------------------------------------
pkgs_needed <- c("shiny", "highcharter", "magrittr",
                 "shinyanimate", "shinyjs", "rclipboard")

to_install <- setdiff(pkgs_needed, rownames(installed.packages()))
if (length(to_install))
  install.packages(to_install, repos = "https://cloud.r-project.org")

lapply(pkgs_needed, require, character.only = TRUE)

# --------------------------------------------------------------------------
#  BIDE • flux constants • Shiny app
# --------------------------------------------------------------------------

code_snippet <- "
## BIDE ultra-simple (flux constants, temps discret)
N0 <- 50  # initial population
B  <- 5   # births per step
I  <- 0   # immigration
D  <- 2   # deaths
E  <- 0   # emigration

tmax <- 20
N <- numeric(tmax + 1)
N[1] <- N0

for (t in 1:tmax) {
  N[t + 1] <- N[t] + B + I - D - E
}

plot(0:tmax, N, type = 'l',
     xlab = 'Time step', ylab = 'Population (N)')
"

ui <- fluidPage(
  rclipboardSetup(),
  useShinyjs(),
  withAnim(),
  titlePanel("BIDE Flux-Constant Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0", "Initial Population (N₀)", 0, 500, 50),
      sliderInput("B",  "Births per step (B)", 0, 20, 5),
      sliderInput("I",  "Immigration per step (I)", 0, 20, 0),
      sliderInput("D",  "Deaths per step (D)", 0, 20, 2),
      sliderInput("E",  "Emigration per step (E)", 0, 20, 0),
      sliderInput("tmax", "Number of steps", 1, 200, 20),
      tags$div(style = "text-align:center; margin:20px 0;",
               actionButton("run", "Run Simulation")),
      sliderInput("time_slider", "Time", min = 0, max = 0, value = 0, step = 1,
                  animate = animationOptions(interval = 40, loop = FALSE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation",
                 highchartOutput("popChart", height = "500px")),
        
        tabPanel("Help",
                 tags$h4("Parameters"),
                 tags$style(".param {margin-bottom:6px;}"),
                 tags$dl(
                   tags$div(class="param",
                            tags$dt(tags$code("N₀")), tags$dd("Population initiale."))
                   ,
                   tags$div(class="param",
                            tags$dt(tags$code("B")), tags$dd("Naissances ajoutées à chaque pas."))
                   ,
                   tags$div(class="param",
                            tags$dt(tags$code("I")), tags$dd("Immigration à chaque pas."))
                   ,
                   tags$div(class="param",
                            tags$dt(tags$code("D")), tags$dd("Décès soustraits à chaque pas."))
                   ,
                   tags$div(class="param",
                            tags$dt(tags$code("E")), tags$dd("Émigration à chaque pas."))
                   ,
                   tags$div(class="param",
                            tags$dt(tags$code("Number of steps")),
                            tags$dd("Durée de la simulation en pas discrets."))
                 ),
                 tags$p(
                   tags$strong("Run Simulation :"),
                   "calcule N(t) avec les flux constants, ajuste la plage du curseur « Time », ",
                   "puis lance automatiquement l’animation."
                 )
        ),
        
        tabPanel("R Code",
                 rclipButton("copy_code", "Copy code", clipText = code_snippet,
                             icon = icon("clipboard")),
                 tags$pre(style="white-space:pre-wrap; word-wrap:break-word;",
                          code_snippet)
        )
      )
    )
  ),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

server <- function(input, output, session){
  
  traj_df <- eventReactive(input$run, {
    N <- numeric(input$tmax + 1)
    N[1] <- input$N0
    for (t in 1:input$tmax)
      N[t + 1] <- N[t] + input$B + input$I - input$D - input$E
    df <- data.frame(time = 0:input$tmax, N = N)
    
    updateSliderInput(session, "time_slider",
                      value = 0, min = 0, max = input$tmax)
    runjs("setTimeout(function(){
             var b=document.querySelector('.slider-animate-button');
             if(b) b.click();
           }, 400);")
    df
  })
  
  output$popChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line", animation = FALSE) %>%
      hc_title(text = "Population vs Time step") %>%
      hc_xAxis(title = list(text = "Time step"), min = 0, max = input$tmax) %>%
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
      hcpxy_update_series(id = "line_series", data = pts, animation = FALSE)
  })
}

shinyApp(ui, server)
