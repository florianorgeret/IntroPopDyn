##  Self-contained Shiny app
#  → checks / installs required packages before launching
pkgs_needed <- c("shiny", "highcharter", "magrittr",
                 "shinyanimate", "shinyjs", "rclipboard", "deSolve")
to_install  <- setdiff(pkgs_needed, rownames(installed.packages()))
if (length(to_install))
  install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(pkgs_needed, require, character.only = TRUE)

#  R code shown in the R Code tab 
code_snippet <- "
## BIDE (per-capita rates) – DISCRETE version
N0 <- 50
b  <- 0.05; d <- 0.01
i  <- 0.00; e <- 0.00
tmax <- 50
N <- numeric(tmax + 1); N[1] <- N0
for (t in 1:tmax) {
  N[t + 1] <- N[t] + (b - d + i - e) * N[t]
}
plot(0:tmax, N, type = 'l',
     xlab = 'Time step', ylab = 'Population (N)')

## -----------------------------------------------------------------
## Same model – CONTINUOUS version (ODE) with deSolve
library(deSolve)
bide_cont <- function(t, state, parms){
  with(as.list(c(state, parms)), {
    dN <- (b - d + i - e) * N
    list(dN)
  })
}
state <- c(N = N0); parms <- c(b = b, d = d, i = i, e = e)
times <- 0:tmax
out <- ode(state, times, bide_cont, parms) |> as.data.frame()
lines(out$time, out$N, col = 2, lty = 2)
legend('topleft', bty='n', col=c(1,2), lty=c(1,2),
       legend=c('Discrete','Continuous (ODE)'))
"

ui <- fluidPage(
  rclipboardSetup(), useShinyjs(), withAnim(),
  titlePanel("BIDE (per-capita rates) Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0", "Initial population N₀", 0, 500, 50),
      sliderInput("b",  "Birth rate  (b)", 0, .2, 0.05, step=.005),
      sliderInput("d",  "Death rate  (d)", 0, .2, 0.01, step=.005),
      sliderInput("i",  "Immigration rate (i)", 0, .1, 0, step=.005),
      sliderInput("e",  "Emigration  rate (e)", 0, .1, 0, step=.005),
      sliderInput("tmax", "Number of steps", 1, 200, 50),
      tags$div(style="text-align:center; margin:20px 0;",
               actionButton("run", "Run Simulation")),
      sliderInput("time_slider", "Time", 0, 0, 0, step=1,
                  animate=animationOptions(interval=40, loop=FALSE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation",
                 highchartOutput("popChart", height="500px")),
        tabPanel("Help",
                 tags$h4("Per-capita rates"),
                 tags$style(".param{margin-bottom:6px;}"),
                 tags$dl(
                   tags$div(class="param", tags$dt(code("b")), tags$dd("births per individual per step")),
                   tags$div(class="param", tags$dt(code("d")), tags$dd("deaths per individual per step")),
                   tags$div(class="param", tags$dt(code("i")), tags$dd("immigrants per individual per step")),
                   tags$div(class="param", tags$dt(code("e")), tags$dd("emigrants per individual per step")),
                   tags$div(class="param", tags$dt(code("N₀ / steps")), tags$dd("initial size and duration"))
                 ),
                 p(strong("Run Simulation:"),"computes the discrete trajectory, updates the slider range, ",
                   "and launches the chart animation. The ODE version gives the same curve with identical parameters.")
        ),
        tabPanel("R Code",
                 rclipButton("copy_code","Copy code",clipText=code_snippet,icon=icon("clipboard")),
                 tags$pre(style="white-space:pre-wrap; word-wrap:break-word;", code_snippet)
        )
      )
    )
  ),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

server <- function(input, output, session){
  
  traj_df <- eventReactive(input$run,{
    N <- numeric(input$tmax+1); N[1] <- input$N0
    for(t in 1:input$tmax)
      N[t+1] <- N[t] + (input$b - input$d + input$i - input$e)*N[t]
    df <- data.frame(time=0:input$tmax, N=N)
    updateSliderInput(session,"time_slider",value=0,min=0,max=input$tmax)
    runjs("setTimeout(function(){
             var b=document.querySelector('.slider-animate-button');
             if(b) b.click(); },400);")
    df
  })
  
  output$popChart <- renderHighchart({
    highchart() %>% hc_chart(type="line", animation=FALSE) %>%
      hc_title(text="Population vs Time step") %>%
      hc_xAxis(title=list(text="Time step"), min=0, max=input$tmax) %>%
      hc_yAxis(title=list(text="Population (N)")) %>%
      hc_add_series(id="line_series", data=list(),
                    name="N", marker=list(enabled=FALSE))
  })
  
  observeEvent(traj_df(),{
    highchartProxy("popChart") %>%
      hcpxy_update(yAxis=list(max=max(traj_df()$N)))
  })
  
  observeEvent(input$time_slider,{
    df <- traj_df(); req(df)
    keep <- df$time <= input$time_slider
    pts  <- Map(function(x,y) list(x=x,y=y), df$time[keep], df$N[keep])
    highchartProxy("popChart") %>%
      hcpxy_update_series(id="line_series", data=pts, animation=FALSE)
  })
}

shinyApp(ui, server)
