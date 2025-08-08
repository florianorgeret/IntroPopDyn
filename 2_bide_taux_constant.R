pkgs <- c("shiny","highcharter","magrittr","shinyjs","shinyanimate","rclipboard","deSolve")
need <- setdiff(pkgs, rownames(installed.packages()))
if(length(need)) install.packages(need, repos="https://cloud.r-project.org")
lapply(pkgs, require, character.only = TRUE)

## R Code shown in the “R Code” tab (uses natural log)
code_snippet <- "
## BIDE per-capita rates – discrete vs continuous (ODE) with natural log
N0 <- 50
b <- 0.05; d <- 0.01
i <- 0.00; e <- 0.00
r <- b + i - d - e
tmax <- 50

## discrete model: N[t+1] = N[t] + r * N[t]
N <- numeric(tmax + 1); N[1] <- N0
for (t in 1:tmax) N[t + 1] <- N[t] + r * N[t]
df_discrete <- data.frame(time = 0:tmax, N = N)
df_discrete$N_round <- round(df_discrete$N)  # whole numbers for display

## continuous model (ODE): dN/dt = r * N
library(deSolve)
ode_fun <- function(t, state, parms){
  with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
}
state <- c(N = N0)
df_continuous <- ode(state, 0:tmax, ode_fun, parms = c(r = r)) |> as.data.frame()
df_continuous$N_round <- round(df_continuous$N)

## extra step: natural log (on real-valued N; NA if N<=0)
df_discrete$lnN   <- ifelse(df_discrete$N > 0, log(df_discrete$N), NA_real_)
df_continuous$lnN <- ifelse(df_continuous$N > 0, log(df_continuous$N), NA_real_)

## Plot linear (rounded values)
plot(df_discrete$time, df_discrete$N_round, type='l', lty=3, xlab='Time step', ylab='Population (N)',
     main=sprintf('r=%.3f', r))
lines(df_continuous$time, df_continuous$N_round, lty=1)
legend('topleft', bty='n', lty=c(3,1), legend=c('Discrete','Continuous (ODE)'))

## Plot ln(N) on a linear axis
plot(df_discrete$time, df_discrete$lnN, type='l', lty=3, xlab='Time step', ylab='ln(N)',
     main='Natural log transform of trajectories')
lines(df_continuous$time, df_continuous$lnN, lty=1)
legend('topleft', bty='n', lty=c(3,1), legend=c('Discrete','Continuous (ODE)'))

## Simple estimation of r: slope of ln(N) ~ time
coef(lm(log(df_continuous$N) ~ df_continuous$time))
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
      checkboxInput("animate_cont","Continuous curve (ODE)", FALSE),
      checkboxInput("ln_scale","Transform Y-axis (semi-log)", FALSE),
      div(style="text-align:center;margin:18px;",
          actionButton("run","Run simulation")),
      sliderInput("time_slider","Time",0,0,0,step=1,
                  animate=animationOptions(interval=20,loop=FALSE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation", highchartOutput("popChart",height="520px")),
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
                 tags$p(strong("Taux net :"), code("r = b + i - d - e"),
                        "est le taux de croissance par individu résultant de la combinaison des naissances, immigration, décès et émigration."),
                 tags$p(strong("Modèle discret :"),
                        "la population évolue par sauts à chaque pas de temps (Δt = 1) selon ",
                        code("N[t+1] = N[t] + r * N[t]"), "."),
                 tags$p("C'est une discrétisation explicite de l'évolution continue ; on applique à chaque étape le taux net sur la population actuelle."),
                 tags$p(strong("Modèle continu (ODE) :"),
                        "ODE signifie équation différentielle ordinaire. Ici on écrit ",
                        code("dN/dt = r * N"), ", ce qui correspond à la limite lorsque le pas de temps devient infinitésimal. La trajectoire est lisse et suit une croissance ou décroissance exponentielle."),
                 tags$p("La case à cocher contrôle si la courbe continue se dévoile progressivement (si cochée) ou reste figée à son point initial jusqu'à ce que le curseur avance (si décochée)."),
                 tags$p(strong("Transformée ln :"),
                        "Cocher « ln Y-axis » trace ", code("ln(N)"),
                        " sur un axe linéaire. Dans ce repère, la pente de la courbe continue est égale à ",
                        code("r"),
                        ". Un triangle annoté « r » illustre cette pente et n'apparaît qu'après avoir relancé la simulation avec l'échelle ln activée.")
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
  
  ## Auto-toggle ODE animation with ln toggle
  observeEvent(input$ln_scale, {
    updateCheckboxInput(session, "animate_cont", value = isTRUE(input$ln_scale))
    # Remove any existing triangle; it will reappear only after the next Run
    highchartProxy("popChart") %>%
      hcpxy_remove_series(id="slope_triangle") %>%
      hcpxy_remove_series(id="slope_label") %>% invisible()
  }, ignoreInit = TRUE)
  
  ## Triangle allowed only right after RUN (and only if ln was ON at that time)
  triangle_ok <- reactiveVal(FALSE)
  
  traj <- eventReactive(input$run, {
    triangle_ok(isTRUE(input$ln_scale))
    
    r <- input$b + input$i - input$d - input$e
    
    ## Discrete trajectory (real + rounded + ln)
    N <- numeric(input$tmax+1); N[1] <- input$N0
    for(t in 1:input$tmax) N[t+1] <- N[t] + r * N[t]
    df_d <- data.frame(time = 0:input$tmax, N = N)
    df_d$N_round <- round(df_d$N)
    df_d$lnN     <- ifelse(df_d$N > 0, log(df_d$N), NA_real_)
    
    ## Continuous (ODE): real + rounded + ln
    ode_fun <- function(t, state, parms){
      with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
    }
    state <- c(N = input$N0); parms <- c(r = r)
    df_c <- deSolve::ode(state, 0:input$tmax, ode_fun, parms) %>% as.data.frame()
    df_c$N_round <- round(df_c$N)
    df_c$lnN     <- ifelse(df_c$N > 0, log(df_c$N), NA_real_)
    
    list(discrete = df_d, continuous = df_c, r = r)
  })
  
  output$popChart <- renderHighchart({
    req(traj())
    ln_mode  <- isTRUE(input$ln_scale)
    y_title  <- if (ln_mode) "ln(N)" else "Population (N)"
    blue     <- "#1f77b4"
    r_val    <- traj()$r
    
    ## Scaling values
    all_vals <- if (ln_mode) {
      c(traj()$discrete$lnN, traj()$continuous$lnN)
    } else {
      c(traj()$discrete$N_round, traj()$continuous$N_round)
    }
    finite_vals <- all_vals[is.finite(all_vals)]
    if (!length(finite_vals)) {
      showNotification("ln scale requires positive values; increase N0 or r.", type="warning")
      finite_vals <- c(0,1)
    }
    ymin <- if (ln_mode) min(finite_vals, na.rm=TRUE) else 0
    ymax <- max(finite_vals, na.rm=TRUE)
    pad  <- if (ln_mode) 0.05 * (ymax - ymin) else 0.05 * ymax
    y_min_plot <- if (ln_mode) ymin - pad else 0
    y_max_plot <- ymax + pad
    
    ## Initial points
    y0_d <- if (ln_mode) traj()$discrete$lnN[1]     else traj()$discrete$N_round[1]
    y0_c <- if (ln_mode) traj()$continuous$lnN[1]   else traj()$continuous$N_round[1]
    
    ## Formats
    lbl_fmt <- if (ln_mode) "{value:.2f}" else "{value:.0f}"
    tip_fmt <- if (ln_mode) "{point.y:.2f}" else "{point.y:.0f}"
    
    highchart() %>%
      hc_chart(type="line", animation=FALSE) %>%
      hc_xAxis(title=list(text="Time step", style=list(fontSize="15px")),
               min=0, max=input$tmax) %>%
      hc_yAxis(title=list(text=y_title, style=list(fontSize="15px")),
               type="linear", min=y_min_plot, max=y_max_plot,
               labels=list(format = lbl_fmt)) %>%
      hc_tooltip(shared=TRUE, pointFormat=paste0("{series.name}: <b>", tip_fmt, "</b><br/>")) %>%
      hc_subtitle(text = sprintf("r = b + i - d - e = %.2f", r_val),
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
    ln_mode <- isTRUE(input$ln_scale)
    
    df_d <- traj()$discrete
    keep_d <- df_d$time <= input$time_slider
    y_d <- if (ln_mode) df_d$lnN else df_d$N_round
    pts_d <- Map(function(x,y) list(x=x,y=y), df_d$time[keep_d], y_d[keep_d])
    proxy <- highchartProxy("popChart") %>% hcpxy_update_series(id="discrete", data=pts_d)
    
    if (isTRUE(input$animate_cont)) {
      df_c <- traj()$continuous
      keep_c <- df_c$time <= input$time_slider
      y_c <- if (ln_mode) df_c$lnN else df_c$N_round
      pts_c <- Map(function(x,y) list(x=x,y=y), df_c$time[keep_c], y_c[keep_c])
      proxy <- proxy %>% hcpxy_update_series(id="continuous", data=pts_c)
    }
    proxy
  })
  
  ## Triangle on ODE (slope = r). Label 'r' INSIDE triangle near the lower-left corner (data-space placement).
  draw_triangle <- function(tr, tmax){
    r_val <- tr$r
    if(!is.finite(r_val) || abs(r_val) < 1e-8) return(invisible(NULL))
    
    all_vals <- c(tr$discrete$lnN, tr$continuous$lnN)
    finite_vals <- all_vals[is.finite(all_vals)]
    if (!length(finite_vals)) return(invisible(NULL))
    ymin <- min(finite_vals, na.rm=TRUE); ymax <- max(finite_vals, na.rm=TRUE)
    pad  <- 0.05 * (ymax - ymin)
    y_min_plot <- ymin - pad; y_max_plot <- ymax + pad
    span <- y_max_plot - y_min_plot
    margin <- 0.06 * span
    
    ## Anchor on the ODE straight line: y = ln(N0) + r * t
    dx <- max(1, round(tmax * 0.22))
    x0 <- max(0, round(tmax * 0.15))
    if (x0 + dx > tmax) x0 <- max(0, tmax - dx)
    y_line_x0 <- approx(tr$continuous$time, tr$continuous$lnN, xout = x0, rule = 2, ties = mean)$y
    dy <- r_val * dx
    
    ## Keep triangle inside the plot
    y_low  <- min(y_line_x0, y_line_x0 + dy)
    y_high <- max(y_line_x0, y_line_x0 + dy)
    shift_up   <- (y_min_plot + margin) - y_low
    shift_down <- y_high - (y_max_plot - margin)
    y0 <- y_line_x0
    if (shift_up > 0)   y0 <- y0 + shift_up
    if (shift_down > 0) y0 <- y0 - shift_down
    
    tri_pts <- list(
      list(x = x0,      y = y0),           # lower-left corner
      list(x = x0 + dx, y = y0),
      list(x = x0 + dx, y = y0 + dy)
    )
    
    ## Place the 'r' label INSIDE the triangle along the hypotenuse, close to the lower-left corner
    label_x <- x0 + dx * 0.12
    label_y <- y0 + dy * 0.12
    
    highchartProxy("popChart") %>%
      hcpxy_remove_series(id="slope_triangle") %>%
      hcpxy_remove_series(id="slope_label") %>%
      hcpxy_add_series(type="polygon", id="slope_triangle", name="slope r",
                       data=tri_pts, color="rgba(31,119,180,0.18)",
                       enableMouseTracking=FALSE, showInLegend=FALSE, zIndex=3) %>%
      hcpxy_add_series(
        type="scatter", id="slope_label",
        data=list(list(x = label_x, y = label_y, name="r")),
        marker=list(enabled=FALSE),
        dataLabels=list(
          enabled=TRUE, format="{point.name}",
          crop=FALSE, overflow="justify",
          style=list(fontSize="16px", fontWeight="700", color="#1f77b4")
        ),
        enableMouseTracking=FALSE, showInLegend=FALSE, zIndex=4
      ) %>% invisible()
  }
  
  ## On RUN: reset slider; if ln ON at click, draw triangle after flush; start animation
  observeEvent(input$run, {
    updateSliderInput(session,"time_slider", value=0, min=0, max=input$tmax)
    highchartProxy("popChart") %>%
      hcpxy_remove_series(id="slope_triangle") %>%
      hcpxy_remove_series(id="slope_label") %>% invisible()
    
    if (isTRUE(triangle_ok())) {
      tr   <- isolate(traj()); if (!is.null(tr)) {
        tmax <- isolate(input$tmax)
        session$onFlushed(function(){ draw_triangle(tr, tmax) }, once = TRUE)
      }
    }
    
    runjs("setTimeout(function(){
             var btn=document.querySelector('.slider-animate-button');
             if(btn) btn.click();
           },400);")
  })
}

shinyApp(ui, server)
