
pkgs_needed <- c("shiny", "highcharter", "magrittr",
                 "shinyanimate", "shinyjs", "rclipboard", "deSolve")
to_install  <- setdiff(pkgs_needed, rownames(installed.packages()))
if (length(to_install))
  install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(pkgs_needed, require, character.only = TRUE)

code_snippet <- "
## BIDE (per-capita rates) – discrete vs continuous (ODE) comparison
## r = b + i - d - e is the net per-capita growth rate.
## Discrete model: time step Δt = 1, N[t+1] = N[t] + r * N[t] (jumps).
## Continuous model: ordinary differential equation (ODE) dN/dt = r * N, smooth limit.
N0 <- 50
b  <- 0.05; i <- 0.00
d  <- 0.01; e <- 0.00
r  <- b + i - d - e
tmax <- 50

## discrete
N <- numeric(tmax + 1); N[1] <- N0
for (t in 1:tmax) {
  N[t + 1] <- N[t] + r * N[t]
}
plot(0:tmax, N, type = 'l',
     xlab = 'Time step', ylab = 'Population (N)', main=sprintf('r = %.2f', r), lty=3, col='#1f77b4')

## continuous via ODE
library(deSolve)
bide_cont <- function(t, state, parms){
  with(as.list(c(state, parms)), {
    dN <- r * N
    list(dN)
  })
}
state <- c(N = N0); parms <- c(r = r)
times <- 0:tmax
out <- ode(state, times, bide_cont, parms) |> as.data.frame()
lines(out$time, out$N, col = '#1f77b4', lty = 1)
legend('topleft', bty='n', col=c('#1f77b4','#1f77b4'), lty=c(3,1),
       legend=c('Discrete (time step)','Continuous (ODE)')) 
"

ui <- fluidPage(
  rclipboardSetup(), useShinyjs(), withAnim(),
  titlePanel("BIDE (per-capita rates) Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0", "Initial population N₀", 0, 500, 50),
      sliderInput("b",  "Birth rate (b)", 0, .2, 0.05, step = .005),
      sliderInput("i",  "Immigration rate (i)", 0, .1, 0, step = .005),
      sliderInput("d",  "Death rate (d)", 0, .2, 0.01, step = .005),
      sliderInput("e",  "Emigration rate (e)", 0, .1, 0, step = .005),
      sliderInput("tmax", "Number of time steps", 1, 200, 50),
      checkboxInput("animate_continuous", "Continuous version (ODE)", value = FALSE),
      tags$div(style = "text-align:center; margin:20px 0;",
               actionButton("run", "Run Simulation")),
      sliderInput("time_slider", "Time", 0, 0, 0, step = 1,
                  animate = animationOptions(interval = 20, loop = FALSE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation",
                 highchartOutput("popChart", height = "500px")),
        tabPanel("Help",
                 tags$h4("Taux per-capita et modèles"),
                 tags$style(".param{margin-bottom:6px;}"),
                 tags$dl(
                   tags$div(class = "param", tags$dt(code("b")), tags$dd("naissances par individu et par pas de temps")),
                   tags$div(class = "param", tags$dt(code("i")), tags$dd("immigrants par individu et par pas de temps")),
                   tags$div(class = "param", tags$dt(code("d")), tags$dd("décès par individu et par pas de temps")),
                   tags$div(class = "param", tags$dt(code("e")), tags$dd("émigrants par individu et par pas de temps")),
                   tags$div(class = "param", tags$dt(code("N₀ / pas de temps")), tags$dd("taille initiale et durée"))
                 ),
                 tags$p(strong("Taux net :"), "r = b + i - d - e est le taux de croissance par individu résultant de la combinaison des naissances, immigration, décès et émigration."),
                 tags$p(strong("Modèle discret :"), "la population évolue par sauts à chaque pas de temps (Δt = 1) selon ", code("N[t+1] = N[t] + r * N[t]"), ". C'est une discrétisation explicite de l'évolution continue ; on applique à chaque étape le taux net sur la population actuelle."),
                 tags$p(strong("Modèle continu (ODE) :"), "ODE signifie équation différentielle ordinaire. Ici on écrit ", code("dN/dt = r * N"), ", ce qui correspond à la limite lorsque le pas de temps devient infinitésimal. La trajectoire est lisse et suit une croissance ou décroissance exponentielle. La case à cocher contrôle si la courbe continue se dévoile progressivement (si cochée) ou reste figée à son point initial jusqu'à ce que le curseur avance (si décochée)."),
                 tags$p(strong("Fonctionnement :"), "Clique sur « Run Simulation » pour recalculer les trajectoires, réinitialiser le curseur temporel et démarrer l'animation. La courbe pointillée est le modèle discret (time step), la courbe continue est la solution de l'ODE.")
        ),
        tabPanel("R Code",
                 rclipButton("copy_code", "Copy code", clipText = code_snippet, icon = icon("clipboard")),
                 tags$pre(style = "white-space:pre-wrap; word-wrap:break-word;", code_snippet)
        )
      )
    )
  ),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

server <- function(input, output, session){
  
  traj_list <- eventReactive(input$run, {
    r <- input$b + input$i - input$d - input$e
    N <- numeric(input$tmax + 1); N[1] <- input$N0
    for (t in 1:input$tmax)
      N[t + 1] <- N[t] + r * N[t]
    df_discrete <- data.frame(time = 0:input$tmax, N = N)
    bide_cont <- function(t, state, parms){
      with(as.list(c(state, parms)), {
        dN <- r * N
        list(dN)
      })
    }
    state <- c(N = input$N0); parms <- c(r = r)
    times <- 0:input$tmax
    out <- ode(state, times, bide_cont, parms) |> as.data.frame()
    updateSliderInput(session, "time_slider", value = 0, min = 0, max = input$tmax)
    runjs("setTimeout(function(){
             var b=document.querySelector('.slider-animate-button');
             if(b) b.click(); },400);")
    list(discrete = df_discrete, continuous = out, r = r)
  })
  
  output$popChart <- renderHighchart({
    r_val <- input$b + input$i - input$d - input$e
    blue <- "#1f77b4"
    highchart() %>%
      hc_chart(type = "line", animation = FALSE) %>%
      hc_subtitle(
        text = paste0("<div style='font-size:22px; font-weight:700;'>r = b + i - d - e = ", sprintf("%.2f", r_val), "</div>"),
        useHTML = TRUE) %>%
      hc_xAxis(title = list(text = "Time step", style = list(fontSize = "16px")), min = 0, max = input$tmax) %>%
      hc_yAxis(title = list(text = "Population (N)", style = list(fontSize = "16px"))) %>%
      hc_add_series(id = "discrete", data = list(), name = "Discrete (time step)", dashStyle = "Dot", color = blue, lineWidth = 4, marker = list(enabled = FALSE)) %>%
      hc_add_series(id = "continuous", data = list(), name = "Continuous (ODE)", color = blue, marker = list(enabled = FALSE))
  })
  
  observeEvent(traj_list(), {
    lst <- traj_list()
    df_d <- lst$discrete
    df_c <- lst$continuous
    maxN <- max(c(df_d$N, df_c$N))
    pts_d0 <- list(list(x = 0, y = df_d$N[1]))
    pts_c0 <- list(list(x = 0, y = df_c$N[1]))
    highchartProxy("popChart") %>%
      hcpxy_update(
        yAxis = list(max = maxN),
        subtitle = list(
          text = paste0("<div style='font-size:22px; font-weight:700;'>r = b + i - d - e = ", sprintf("%.2f", lst$r), "</div>")
        )
      ) %>%
      hcpxy_update_series(id = "discrete", data = pts_d0, animation = FALSE) %>%
      hcpxy_update_series(id = "continuous", data = pts_c0, animation = FALSE)
  })
  
  observeEvent(input$time_slider, {
    lst <- traj_list(); req(lst)
    df_d <- lst$discrete
    keep_d <- df_d$time <= input$time_slider
    pts_d <- Map(function(x,y) list(x = x, y = y), df_d$time[keep_d], df_d$N[keep_d])
    proxy <- highchartProxy("popChart") %>% hcpxy_update_series(id = "discrete", data = pts_d, animation = FALSE)
    if (isTRUE(input$animate_continuous)) {
      df_c <- lst$continuous
      keep_c <- df_c$time <= input$time_slider
      pts_c <- Map(function(x,y) list(x = x, y = y), df_c$time[keep_c], df_c$N[keep_c])
      proxy <- proxy %>% hcpxy_update_series(id = "continuous", data = pts_c, animation = FALSE)
    }
    proxy
  })
}

shinyApp(ui, server)
