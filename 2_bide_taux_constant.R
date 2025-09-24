## Charger les packages requis
pkgs <- c("shiny", "highcharter", "magrittr", "shinyjs", "rclipboard", "deSolve")
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak", repos = "https://cloud.r-project.org")
pak::pkg_install(pkgs)
invisible(lapply(pkgs, require, character.only = TRUE))

## Code R montré dans l'onglet "R Code" (pédagogique et corrigé)
code_snippet <- "
## Modèles de croissance exponentiel continu vs. géométrique discret
library(deSolve)

## Paramètres
N0 <- 50
b <- 0.05   # Taux de natalité per capita
d <- 0.01   # Taux de mortalité per capita
i <- 0.00   # Taux d'immigration per capita
e <- 0.00   # Taux d'émigration per capita

tmax <- 50

## 1. Calculer le taux de croissance intrinsèque (continu), r
r <- b + i - d - e

## 2. En déduire le taux de croissance fini (discret), lambda
lambda <- exp(r)

## Modèle discret (géométrique) : N[t+1] = lambda * N[t]
N_d <- numeric(tmax + 1); N_d[1] <- N0
for (t in 1:tmax) N_d[t + 1] <- lambda * N_d[t]
df_discrete <- data.frame(time = 0:tmax, N = N_d)

## Modèle continu (exponentiel) : dN/dt = r * N
ode_fun <- function(t, state, parms){
  with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
}
state <- c(N = N0)
# On définit des pas de temps fins pour obtenir une courbe bien lisse
times_for_ode <- seq(0, tmax, by = 0.1)
df_continuous <- ode(state, times_for_ode, ode_fun, parms = c(r = r))
df_continuous <- as.data.frame(df_continuous)

## Affichage des trajectoires
plot(df_discrete$time, df_discrete$N, type='p', pch=16, col='blue',
     xlab='Temps', ylab='Population (N)', main=sprintf('r=%.3f | λ=%.3f', r, lambda),
     ylim=range(c(df_discrete$N, df_continuous$N))) # Assurer que tout est visible
lines(df_continuous$time, df_continuous$N, col='red', lwd=2)
legend('topleft', bty='n', pch=c(16, NA), lty=c(NA, 1), col=c('blue','red'),
       legend=c('Discret (points)', 'Continu (courbe)'))
"

ui <- fluidPage(
  titlePanel("Simulateur de croissance : Modèles discrets et continus"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0","Population initiale N₀", 0, 500, 50),
      sliderInput("b","Taux de natalité (b)", 0, 0.5, 0.05, step=.01),
      sliderInput("d","Taux de mortalité (d)", 0, 0.5, 0.01, step=.01),
      sliderInput("i","Taux d'immigration (i)", 0, 0.3, 0,   step=.01),
      sliderInput("e","Taux d'émigration (e)", 0, 0.3, 0,   step=.01),
      sliderInput("tmax","Pas de temps", 1, 200, 50),
      checkboxInput("animate_cont","Dévoiler la courbe continue", FALSE),
      checkboxInput("ln_scale","Échelle log (Axe Y)", FALSE),
      div(style="text-align:center;margin:18px;",
          actionButton("run","Lancer la simulation")),
      sliderInput("time_slider","Temps",0,0,0,step=1,
                  animate=animationOptions(interval=30,loop=FALSE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation", highchartOutput("popChart",height="520px")),
        tabPanel("Help",
                 tags$h4("Paramètres et modèles de croissance"),
                 tags$p("Cette application compare deux modèles fondamentaux de la dynamique des populations."),
                 tags$h5("Taux de croissance"),
                 tags$p(strong("Taux de croissance intrinsèque (continu), r :"), "Calculé comme ", code("r = b + i - d - e"),
                        ", il représente le taux de changement instantané par individu."),
                 tags$p(strong("Taux de croissance fini (discret), λ :"), "Il représente le facteur multiplicatif de la population sur un pas de temps. Il est lié à r par la relation ", code("λ = exp(r)"), "."),
                 tags$h5("Modèles"),
                 tags$p(strong("Modèle continu (Exponentiel) :"),
                        "Décrit par ", code("dN/dt = r * N"), ". Il suppose une croissance permanente, générant une courbe lisse."),
                 tags$p(strong("Modèle discret (Géométrique) :"),
                        "La population évolue par à-coups (ex: reproduction saisonnière) selon ",
                        code("N[t+1] = λ * N[t]"), "."),
                 tags$p("Les deux modèles coïncident aux pas de temps entiers (t = 0, 1, 2, ...)."),
                 tags$h5("Transformation logarithmique"),
                 tags$p("Cocher « Échelle log » affiche ", code("ln(N)"),
                        ". Sur ce graphique, la croissance continue devient une ligne droite de pente ", code("r"), ".")
        ),
        tabPanel("Code R",
                 rclipButton("copy_code","Copier le code", clipText=code_snippet, icon=icon("clipboard")),
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
  
  observeEvent(input$ln_scale, {
    updateCheckboxInput(session, "animate_cont", value = isTRUE(input$ln_scale))
    highchartProxy("popChart") %>%
      hcpxy_remove_series(id="slope_triangle") %>%
      hcpxy_remove_series(id="slope_label") %>% invisible()
  }, ignoreInit = TRUE)
  
  triangle_ok <- reactiveVal(FALSE)
  
  traj <- eventReactive(input$run, {
    triangle_ok(isTRUE(input$ln_scale))
    
    r <- input$b + input$i - input$d - input$e
    lambda <- exp(r)
    
    # Modèle Discret : on garde N_round pour l'affichage des points
    N_d <- numeric(input$tmax+1); N_d[1] <- input$N0
    for(t in 1:input$tmax) N_d[t+1] <- lambda * N_d[t]
    df_d <- data.frame(time = 0:input$tmax, N = N_d)
    df_d$N_round <- round(df_d$N)
    df_d$lnN     <- ifelse(df_d$N > 0, log(df_d$N), NA_real_)
    
    # Modèle Continu : on n'utilise plus de valeurs arrondies pour le tracé
    ode_fun <- function(t, state, parms){
      with(as.list(c(state, parms)), { dN <- r * N; list(dN) })
    }
    state <- c(N = input$N0); parms <- c(r = r)
    times_continus <- seq(0, input$tmax, by = 0.1)
    df_c <- deSolve::ode(state, times_continus, ode_fun, parms) %>% as.data.frame()
    df_c$lnN     <- ifelse(df_c$N > 0, log(df_c$N), NA_real_)
    
    list(discrete = df_d, continuous = df_c, r = r, lambda = lambda)
  })
  
  output$popChart <- renderHighchart({
    req(traj())
    ln_mode   <- isTRUE(input$ln_scale)
    show_cont <- isTRUE(input$animate_cont)
    y_title   <- if (ln_mode) "ln(N)" else "Population (N)"
    
    # Calcul des limites de l'axe Y avec les valeurs réelles pour la courbe continue
    all_vals <- if (ln_mode) {
      c(traj()$discrete$lnN, if (show_cont) traj()$continuous$lnN else numeric(0))
    } else {
      c(traj()$discrete$N_round, if (show_cont) traj()$continuous$N else numeric(0))
    }
    finite_vals <- all_vals[is.finite(all_vals)]
    if (!length(finite_vals)) {
      showNotification("L'échelle logarithmique requiert N > 0.", type="warning")
      finite_vals <- c(0,1)
    }
    ymin <- if (ln_mode) min(finite_vals, na.rm=TRUE) else 0
    ymax <- max(finite_vals, na.rm=TRUE)
    pad  <- 0.05 * (ymax - ymin)
    y_min_plot <- if (ln_mode) ymin - pad else 0
    y_max_plot <- ymax + pad
    
    y0_d <- if (ln_mode) traj()$discrete$lnN[1]   else traj()$discrete$N_round[1]
    y0_c <- if (ln_mode) traj()$continuous$lnN[1] else traj()$continuous$N[1]
    
    highchart() %>%
      hc_chart(type="line", animation=FALSE) %>%
      hc_xAxis(title=list(text="Temps", style=list(fontSize="15px")), min=0, max=input$tmax) %>%
      hc_yAxis(title=list(text=y_title, style=list(fontSize="15px")),
               type="linear", min=y_min_plot, max=y_max_plot) %>%
      hc_tooltip(shared=TRUE, pointFormat="{series.name}: <b>{point.y:,.2f}</b><br/>") %>%
      hc_subtitle(text = sprintf("r = %.3f | λ = %.3f", traj()$r, traj()$lambda),
                  style=list(fontSize="22px", fontWeight="700")) %>%
      hc_add_series(id="discrete", name="Discret (Géométrique)",
                    type="scatter", data=list(list(x=0,y=y0_d)),
                    color="#1f77b4", marker=list(symbol="circle")) %>%
      hc_add_series(id="continuous", name="Continu (Exponentiel)",
                    data=list(list(x=0,y=y0_c)),
                    color="#ff7f0e", marker=list(enabled=FALSE))
  })
  
  observeEvent(input$time_slider, {
    req(traj())
    ln_mode <- isTRUE(input$ln_scale)
    
    # Affichage des points discrets (valeurs arrondies)
    df_d <- traj()$discrete
    keep_d <- df_d$time <= input$time_slider
    y_d <- if (ln_mode) df_d$lnN else df_d$N_round
    pts_d <- Map(function(x,y) list(x=x,y=y), df_d$time[keep_d], y_d[keep_d])
    proxy <- highchartProxy("popChart") %>% hcpxy_update_series(id="discrete", data=pts_d, type="scatter")
    
    # Animation de la courbe continue (valeurs réelles)
    if (isTRUE(input$animate_cont)) {
      df_c <- traj()$continuous
      keep_c <- df_c$time <= input$time_slider
      y_c <- if (ln_mode) df_c$lnN else df_c$N # On utilise bien N, pas N_round
      pts_c <- Map(function(x,y) list(x=x,y=y), df_c$time[keep_c], y_c[keep_c])
      proxy <- proxy %>% hcpxy_update_series(id="continuous", data=pts_c, type="line")
    }
    proxy
  })
  
  # Le reste de la fonction server (draw_triangle, observeEvent(input$run)) est inchangé.
  # ...
  draw_triangle <- function(tr, tmax){
    r_val <- tr$r; if(!is.finite(r_val) || abs(r_val) < 1e-8) return(invisible(NULL))
    all_vals <- c(tr$discrete$lnN, tr$continuous$lnN); finite_vals <- all_vals[is.finite(all_vals)]
    if (!length(finite_vals)) return(invisible(NULL))
    ymin <- min(finite_vals, na.rm=TRUE); ymax <- max(finite_vals, na.rm=TRUE)
    pad <- 0.05 * (ymax - ymin); y_min_plot <- ymin - pad; y_max_plot <- ymax + pad
    span <- y_max_plot - y_min_plot; margin <- 0.06 * span
    dx <- max(1, round(tmax * 0.22)); x0 <- max(0, round(tmax * 0.15))
    if (x0 + dx > tmax) x0 <- max(0, tmax - dx)
    y_line_x0 <- approx(tr$continuous$time, tr$continuous$lnN, xout = x0, rule = 2, ties = mean)$y
    dy <- r_val * dx; y_low  <- min(y_line_x0, y_line_x0 + dy); y_high <- max(y_line_x0, y_line_x0 + dy)
    shift_up   <- (y_min_plot + margin) - y_low; shift_down <- y_high - (y_max_plot - margin)
    y0 <- y_line_x0
    if (shift_up > 0)   y0 <- y0 + shift_up
    if (shift_down > 0) y0 <- y0 - shift_down
    tri_pts <- list(list(x=x0, y=y0), list(x=x0+dx, y=y0), list(x=x0+dx, y=y0+dy))
    label_x <- x0 + dx*0.12; label_y <- y0 + dy*0.12
    highchartProxy("popChart") %>%
      hcpxy_remove_series(id="slope_triangle") %>% hcpxy_remove_series(id="slope_label") %>%
      hcpxy_add_series(type="polygon", id="slope_triangle", name="pente r", data=tri_pts,
                       color="rgba(255,127,14,0.2)", enableMouseTracking=FALSE,
                       showInLegend=FALSE, zIndex=3) %>%
      hcpxy_add_series(type="scatter", id="slope_label", data=list(list(x=label_x, y=label_y, name="r")),
                       marker=list(enabled=FALSE), enableMouseTracking=FALSE, showInLegend=FALSE, zIndex=4,
                       dataLabels=list(enabled=TRUE, format="{point.name}", crop=FALSE, overflow="justify",
                                       style=list(fontSize="16px", fontWeight="700", color="#ff7f0e"))) %>%
      invisible()
  }
  
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
          }, 400);")
  })
}


shinyApp(ui, server)