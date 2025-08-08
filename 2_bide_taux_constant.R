pkgs <- c("shiny","highcharter","magrittr","shinyjs",
          "shinyanimate","rclipboard","deSolve")
need <- setdiff(pkgs, rownames(installed.packages()))
if(length(need)) install.packages(need, repos="https://cloud.r-project.org")
lapply(pkgs, require, character.only = TRUE)

## R Code shown in the “R Code” tab (kept simple, with rounding + log10 step)
code_snippet <- "
## BIDE per-capita rates – discrete vs continuous (ODE) with log10 transform
N0 <- 50
b <- 0.05; d <- 0.01
i <- 0.00; e <- 0.00
r <- b + i - d - e
tmax <- 50

## discrete model: N[t+1] = N[t] + r * N[t]
N <- numeric(tmax + 1); N[1] <- N0
for (t in 1:tmax) N[t + 1] <- N[t] + r * N[t]
df_discrete <- data.frame(time = 0:tmax, N = N)
df_discrete$N_round <- round(df_discrete$N)  # population must be whole numbers for display

## continuous model (ODE): dN/dt = r * N
library(deSolve)
ode_fun <- function(t, state, parms){
  with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
}
state <- c(N = N0)
df_continuous <- ode(state, 0:tmax, ode_fun, parms = c(r = r)) 
df_continuous <- as.data.frame(df_continuous)
df_continuous$N_round <- round(df_continuous$N)  # display as whole numbers

## Plot linear (rounded values for display)
plot(df_discrete$time, df_discrete$N_round, type='l', lty=3, xlab='Time step', ylab='Population (N)',
     main=sprintf('r=%.3f', r))
lines(df_continuous$time, df_continuous$N_round, lty=1)
legend('topleft', bty='n', lty=c(3,1), legend=c('Discrete','Continuous (ODE)'))


## extra step: log10 transform (on real-valued N, otherwise we'll get an error in case of N=0)
df_discrete$log10N   <- log10(df_discrete$N)
df_continuous$log10N <- log10(df_continuous$N)

## Plot log10 (use real-valued N)
plot(df_discrete$time, df_discrete$log10N, type='l', lty=3, xlab='Time step', ylab='log10(N)',
     main='log10 transform of trajectories')
lines(df_continuous$time, df_continuous$log10N, lty=1)
legend('topleft', bty='n', lty=c(3,1), legend=c('Discrete','Continuous (ODE)'))
"

ui <- fluidPage(
  titlePanel("BIDE – Per-Capita Rates simulator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0","Initial population N0", 0, 500, 50),
      sliderInput("b","Birth rate (b)", 0, 0.5, 0.05, step=.01),
      sliderInput("d","Death rate (d)", 0, 0.5, 0.01, step=.01),
      sliderInput("i","Immigration rate (i)", 0, 0.3, 0,    step=.01),
      sliderInput("e","Emigration  rate (e)", 0, 0.3, 0,    step=.01),
      sliderInput("tmax","Time steps", 1, 200, 50),
      checkboxInput("animate_cont","Animate ODE curve", FALSE),
      checkboxInput("log_scale","Log10 Y-axis (semi-log)", FALSE),
      div(style="text-align:center;margin:18px;",
          actionButton("run","Run simulation")),
      sliderInput("time_slider","Time",0,0,0,step=1,
                  animate=animationOptions(interval=20,loop=FALSE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation",
                 highchartOutput("popChart",height="500px")),
        tabPanel("Help",
                 tags$h4("Taux per-capita et modèles"),
                 tags$style(".param{margin-bottom:6px;}"),
                 tags$dl(
                   tags$div(class="param", tags$dt(code("b")), tags$dd("naissances par individu et par pas de temps")),
                   tags$div(class="param", tags$dt(code("i")), tags$dd("immigrants par individu et par pas de temps")),
                   tags$div(class="param", tags$dt(code("d")), tags$dd("décès par individu et par pas de temps")),
                   tags$div(class="param", tags$dt(code("e")), tags$dd("émigrants par individu et par pas de temps")),
                   tags$div(class="param", tags$dt(code("N₀ / pas de temps")), tags$dd("taille initiale et durée"))
                 ),
                 tags$p(strong("Taux net :"), code("r = b + i - d - e"), "est le taux de croissance par individu résultant de la combinaison des naissances, immigration, décès et émigration."),
                 tags$p(strong("Modèle discret :"), "la population évolue par sauts à chaque pas de temps (Δt = 1) selon ", code("N[t+1] = N[t] + r * N[t]"), ". C'est une discrétisation explicite de l'évolution continue ; on applique à chaque étape le taux net sur la population actuelle."),
                 tags$p(strong("Modèle continu (ODE) :"), "ODE signifie équation différentielle ordinaire. Ici on écrit ", code("dN/dt = r * N"), ", ce qui correspond à la limite lorsque le pas de temps devient infinitésimal. La trajectoire est lisse et suit une croissance ou décroissance exponentielle. La case à cocher contrôle si la courbe continue se dévoile progressivement (si cochée) ou reste figée à son point initial jusqu'à ce que le curseur avance (si décochée)."),
                 tags$p(strong("Transformée log10 :"), "Étape supplémentaire : on calcule ", code("log10(N)"), " (dans l'onglet R Code) pour visualiser une croissance exponentielle comme une droite ; cocher 'Log10 Y-axis' applique l'équivalent directement sur l'axe du graphique."),
                 tags$p(strong("Fonctionnement :"), "Cliquer sur « Run simulation » pour recalculer les trajectoires discrète et continue, réinitialiser le curseur temporel et déclencher l'animation. Le sous-titre affiche r.")
        ),
        tabPanel("R Code",
                 rclipButton("copy_code","Copy code", clipText=code_snippet, icon=icon("clipboard")),
                 tags$pre(style="white-space:pre-wrap; word-wrap:break-word;", code_snippet)
        )
      )
    )
  ),
  useShinyjs(),
  rclipboardSetup(),
  tags$style(".slider-animate-button{visibility:hidden;}")
)

server <- function(input, output, session){
  
  traj <- eventReactive(input$run, {
    r <- input$b + input$i - input$d - input$e
    
    ## Discrete trajectory (store real + rounded for display)
    N <- numeric(input$tmax+1); N[1] <- input$N0
    for(t in 1:input$tmax) N[t+1] <- N[t] + r * N[t]
    df_d <- data.frame(time = 0:input$tmax, N = N)
    df_d$N_round <- round(df_d$N)
    
    ## Continuous (ODE) — keep real values, plus a rounded column for linear display
    ode_fun <- function(t, state, parms){
      with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
    }
    state <- c(N = input$N0); parms <- c(r = r)
    df_c <- deSolve::ode(state, 0:input$tmax, ode_fun, parms) %>% as.data.frame()
    df_c$N_round <- round(df_c$N)
    
    list(discrete = df_d, continuous = df_c, r = r)
  })
  
  output$popChart <- renderHighchart({
    req(traj())
    axis_type <- if (isTRUE(input$log_scale)) "logarithmic" else "linear"
    y_title   <- if (isTRUE(input$log_scale)) "log₁₀(N)" else "Population (N)"
    blue <- "#1f77b4"
    r_val <- traj()$r
    
    ## Choose values for scaling: rounded when linear, real when log
    all_vals <- if (!isTRUE(input$log_scale)) {
      c(traj()$discrete$N_round, traj()$continuous$N_round)
    } else {
      c(traj()$discrete$N,       traj()$continuous$N)
    }
    ymax <- max(all_vals, na.rm=TRUE); pad_top <- ymax * 0.05; ymax_plot <- ymax + pad_top
    
    if (axis_type == "logarithmic") {
      pos_vals <- all_vals[all_vals > 0]
      if (!length(pos_vals)) {
        showNotification("Log scale requires positive values; increase N0 or r.", type="warning")
        pos_vals <- 1
      }
      yaxis_min <- max(min(pos_vals) * 0.9, 1e-8)
      yaxis_max <- ymax_plot
    } else {
      yaxis_min <- 0
      yaxis_max <- ymax_plot
    }
    
    y0_d <- if (!isTRUE(input$log_scale)) traj()$discrete$N_round[1] else traj()$discrete$N[1]
    y0_c <- if (!isTRUE(input$log_scale)) traj()$continuous$N_round[1] else traj()$continuous$N[1]
    
    highchart() %>%
      hc_chart(type="line", animation=FALSE) %>%
      hc_xAxis(title=list(text="Time step", style=list(fontSize="15px")),
               min=0, max=input$tmax) %>%
      hc_yAxis(title=list(text=y_title, style=list(fontSize="15px")),
               type=axis_type, min=yaxis_min, max=yaxis_max,
               labels=list(format = if (isTRUE(input$log_scale)) "{value}" else "{value:.0f}")) %>%
      hc_tooltip(shared=TRUE, pointFormat="{series.name}: <b>{point.y:.0f}</b><br/>") %>%
      hc_subtitle(text = sprintf("r = b + i - d - e = %.3f", r_val),
                  style=list(fontSize="22px", fontWeight="700")) %>%
      hc_add_series(id="discrete", name="Discrete",
                    data=list(list(x=0,y=y0_d)),
                    dashStyle="Dot", color=blue, marker=list(enabled=FALSE)) %>%
      hc_add_series(id="continuous", name="Continuous (ODE)",
                    data=list(list(x=0,y=y0_c)),
                    color=blue, marker=list(enabled=FALSE))
  })
  
  observeEvent(input$time_slider, {
    req(traj())
    df_d <- traj()$discrete
    keep_d <- df_d$time <= input$time_slider
    y_d <- if (!isTRUE(input$log_scale)) df_d$N_round else df_d$N
    pts_d <- Map(function(x,y) list(x=x,y=y), df_d$time[keep_d], y_d[keep_d])
    
    proxy <- highchartProxy("popChart") %>%
      hcpxy_update_series(id="discrete", data=pts_d)
    
    if (isTRUE(input$animate_cont)) {
      df_c <- traj()$continuous
      keep_c <- df_c$time <= input$time_slider
      y_c <- if (!isTRUE(input$log_scale)) df_c$N_round else df_c$N
      pts_c <- Map(function(x,y) list(x=x,y=y), df_c$time[keep_c], y_c[keep_c])
      proxy <- proxy %>% hcpxy_update_series(id="continuous", data=pts_c)
    }
    proxy
  })
  
  observeEvent(input$run, {
    updateSliderInput(session,"time_slider", value=0, min=0, max=input$tmax)
    runjs("setTimeout(function(){
             var btn=document.querySelector('.slider-animate-button');
             if(btn) btn.click();
           },400);")
  })
}

shinyApp(ui, server)
