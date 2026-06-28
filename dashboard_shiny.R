
# Chargement des librairies

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(tidymodels)
library(vip)

# ── Chargement des données et modèles ─────────────────────
fit_final        <- readRDS("model/xgboost_energie.rds")
resultats_tuning <- readRDS("model/resultats_tuning.rds")
df_fusionne      <- read_csv("data/processed/donnee_fusionne.csv")
df_train         <- read_csv("data/processed/train.csv")
df_test          <- read_csv("data/processed/test.csv")
df               <- read_csv("data/processed/energetique.csv")
df_meteo         <- read_csv("data/processed/meteo.csv")

# ── Calcul des métriques ───────────────────────────────────
mae_baseline  <- 1100.69
mape_baseline <- 4.77
r2_baseline   <- 0.899

mae_xgb  <- mean(abs(predictions$demande_energetique -
                       predictions$pred_xgb), na.rm = TRUE)
mape_xgb <- mean(abs((predictions$demande_energetique -
                        predictions$pred_xgb) /
                       predictions$demande_energetique)) * 100
ss_res   <- sum((predictions$demande_energetique -
                   predictions$pred_xgb)^2, na.rm = TRUE)
ss_tot   <- sum((predictions$demande_energetique -
                   mean(predictions$demande_energetique))^2, na.rm = TRUE)
r2_xgb   <- 1 - ss_res / ss_tot

# ── Thème ──────────────────────────────────────────────────
theme_dash <- bs_theme(
  bootswatch  = "flatly",
  primary     = "#085041",
  success     = "#1D9E75",
  base_font   = font_google("Inter"),
  heading_font = font_google("Inter"),
  navbar_bg    = "#085041",    # ← couleur navbar ici
  navbar_fg    = "white"       # ← couleur texte navbar ici
)

# ============================================================
# UI
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(tidymodels)
library(vip)

ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px;",
    bsicons::bs_icon("lightning-charge-fill", size = "1.5em"),
    span("Prévision de la demande énergétique",
         style = "font-size: 1.3em; font-weight: 600; letter-spacing: 0.3px;")
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#085041",
    success      = "#1D9E75",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  # ── Navigation pour l'Onglet 1 : Vue d'ensemble ──────────────────────────
  nav_panel(
    "Vue d'ensemble",
    layout_column_wrap(
      width = 1/3,
      card(
        card_header("Apercu des données d'Hydro Québec (df)"),
        tableOutput("apercu_df")
      ),
      card(
        card_header("Apercu des données météorologiques (df_meteo)"),
        tableOutput("apercu_meteo")
      ),
      card(
        card_header("Apercu des données fusionnées (df_fusionne)"),
        tableOutput("apercu_fusionne")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      bslib::value_box(
        title = "Nombre d'observations",
        value = format(nrow(df_fusionne), big.mark = " "),
        showcase = bsicons::bs_icon("table"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Nombre de variables",
        value = ncol(df_fusionne),
        showcase = bsicons::bs_icon("layout-three-columns"),
        theme = "primary"
      )
    ),
    
      card(
        card_header("Répartition train / test"),
        plotlyOutput("graphe_split", height = "250px")
      )
    ),
  
  # Navigation pour l'onglet: Prédictions
  nav_panel(
    "Prédictions",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        h5("Paramètres"),
        dateRangeInput(
          "plage_dates",
          "Période affichée",
          start    = min(predictions$date),
          end      = min(predictions$date) + days(30),
          min      = min(predictions$date),
          max      = max(predictions$date),
          language = "fr"
        ),
        hr(),
        checkboxInput("show_residus", "Afficher les résidus", FALSE),
        hr(),
        h6("Métriques sur la période sélectionnée"),
        verbatimTextOutput("metriques_periode")
      ),
      card(
        card_header("Prédictions XGBoost vs demande réelle"),
        plotlyOutput("graphe_predictions", height = "400px")
      ),
      conditionalPanel(
        condition = "input.show_residus == true",
        card(
          card_header("Résidus"),
          plotlyOutput("graphe_residus", height = "250px")
        )
      )
    )
  ),
  # Navigation pour l'onglet: Analyse exploratoire
  nav_panel("Analyse exploratoire",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Profil saisonnier mensuel"),
                plotlyOutput("graphe_saisonnier", height = "350px")
              ),
              card(
                card_header("Relation demande ~ température"),
                plotlyOutput("graphe_temp", height = "350px")
              )
            ),
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Profil horaire: semaine vs weekend"),
                plotlyOutput("graphe_horaire", height ="350px")
              ),
              card(
                card_header("Évolution 2019-2024"),
                plotlyOutput("graphe_evolution", height = "350px")
              )
            )
            
            ),
  nav_panel("Modélisation",
            layout_column_wrap(
              width = 1/4,
              heights_equal ="row",
              fixed_width = FALSE,
              bslib::value_box(
                title    = "MAE — XGBoost",
                value    = paste0(round(mae_xgb, 0), " MW"),
                showcase = bs_icon("lightning-charge"),
                theme    = "success",
                height = "160px",
                p("vs baseline : ", strong(paste0(round(mae_baseline, 0), " MW")))
              ),
              bslib::value_box(
                title    = "MAPE — XGBoost",
                value    = paste0(round(mape_xgb, 2), "%"),
                showcase = bs_icon("percent"),
                theme    = "success",
                height = "160px",
                p("Taux de prédiction : ", strong(paste0(round(100 - mape_xgb, 2), "%")))
              ),
              bslib::value_box(
                title    = "R² — XGBoost",
                value    = round(r2_xgb, 4),
                showcase = bs_icon("graph-up"),
                theme    = "success",
                height = "160px",
                p("Variance expliquée : ", strong(paste0(round(r2_xgb * 100, 2), "%")))
              ),
              bslib::value_box(
                title    = "Période couverte",
                value    = "2019–2024",
                showcase = bs_icon("calendar3"),
                theme    = "primary",
                height = "160px",
                p("43 000+ observations horaires")
              )
            ),
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Importance des variables (VIP)"),
                plotOutput("vip_plot", height = "400px")
              ),
              card(
                card_header("Courbe d'apprentissage selon MAE par fold"),
                plotlyOutput("courbe_apprentissage", height = "400px")
              )
            ),
            card(
              card_header("Tableau comparatif des modèles"),
              tableOutput("table_comparaison")
            )
            
            ),
  nav_panel("À propos",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Description du projet"),
                p("Ce projet prédit la demande électrique horaire au Québec en utilisant
        des données historiques d'Hydro-Québec (2019–2024) combinées avec des
        données météorologiques."),
                hr(),
                h6("Sources de données"),
                tags$ul(
                  tags$li(strong("Hydro-Québec:")," Historique de données  de la demande énergétique par heure (https://donnees.hydroquebec.com/explore/dataset/historique-demande-electricite-quebec/information/"),
                  tags$li(strong("Open-Meteo: "), " Données météo par heure (récupérées sur l'API sous les coordonnées géographiques (latitude: 45.52°N et longitude: 73.61°W correspondant à Montréal)")
                ),
                hr(),
                h6("Pipeline"),
                tags$ol(
                  tags$li("Collecte et fusion des données"),
                  tags$li("Analyse exploratoire (EDA)"),
                  tags$li("Feature engineering (lags, HDD/CDD, encodage cyclique)"),
                  tags$li("Modélisation XGBoost avec validation croisée temporelle"),
                  tags$li("Évaluation et comparaison des modèles")
                )
              ),
              card(
                card_header("Stack technique"),
                tags$ul(
                  tags$li(strong("Langage : "), "R"),
                  tags$li(strong("Modélisation : "), "tidymodels, xgboost, glmnet"),
                  tags$li(strong("Visualisation : "), "ggplot2, plotly"),
                  tags$li(strong("Dashboard : "), "Shiny, bslib"),
                  tags$li(strong("Données : "), "dplyr, lubridate, slider")
                ),
                hr(),
                h6("Hyperparamètres optimaux (XGBoost)"),
                tags$ul(
                  tags$li(strong("min_n : "), "14"),
                  tags$li(strong("tree_depth : "), "13"),
                  tags$li(strong("learn_rate : "), "0.0449")
                ),
                hr(),
                h6("Performance finale: Modèle XGBoost"),
                tags$ul(
                  tags$li(strong("MAE : "), paste0(round(mae_xgb, 0), " MW")),
                  tags$li(strong("MAPE : "), paste0(round(mape_xgb, 2), "%")),
                  tags$li(strong("R² : "), round(r2_xgb, 4)),
                  tags$li(strong("Taux de prédiction : "), paste0(round(100 - mape_xgb, 2), "%")) 
                  
                )
              )
            )
)
)

server <- function(input, output, session) {
  
  
  # Données réactives 
  df_filtre <- reactive({
    predictions %>%
      filter(date >= input$plage_dates[1],
             date <= input$plage_dates[2] + days(1))
  })
  
  # --------------Onglet Vue d'ensemble--------------
 
  output$apercu_df <- renderTable({
    head(df, 5)
  }, striped = TRUE, hover = TRUE, bordered =TRUE)
  
  output$apercu_meteo <- renderTable({
    head(df_meteo, 5)
  },striped = TRUE, hover = TRUE, bordered =TRUE
  )
  
  output$apercu_fusionne <- renderTable({
    head(df_fusionne, 5)
  },striped = TRUE, hover = TRUE, bordered =TRUE
  )
  output$shape_fusionne <- renderPrint({
    cat("Nombre d'observations:", nrow(df_fusionne), "\n")
    cat("Nombre de variables:", ncol(df_fusionne), "\n")
  })
  
  output$graphe_split <- renderPlotly({
    df_fusionne %>%
      mutate(ensemble = ifelse(date < as.POSIXct("2023-01-02"),
                               "Entraînement", "Test")) %>%
      group_by(mois_annee = floor_date(date, "month"), ensemble) %>%
      summarise(demande_moy = mean(demande_energetique, na.rm = TRUE),
                .groups = "drop") %>%
      plot_ly(x = ~mois_annee, y = ~demande_moy, color = ~ensemble,
              type = "scatter", mode = "lines",
              colors = c("Entraînement" = "#085041", "Test" = "#EF9F27")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Demande (MW)"),
             legend = list(orientation = "h", y = 1.1))
    
    })
    
  #-------------- Onglet "Prédictions"----------
    output$metriques_periode <- renderText({
      df <- df_filtre() %>% filter(!is.na(demande_energetique))
      mae_p  <- mean(abs(df$demande_energetique - df$pred_xgb), na.rm = TRUE)
      mape_p <- mean(abs((df$demande_energetique - df$pred_xgb) /
                           df$demande_energetique)) * 100
      paste0("MAE  : ", round(mae_p, 0), " MW\n",
             "MAPE : ", round(mape_p, 2), "%\n",
             "Taux : ", round(100 - mape_p, 2), "%")
    })
    
    output$graphe_predictions <- renderPlotly({
      df <- df_filtre()
      plot_ly() %>%
        add_lines(data = df, x = ~date, y = ~demande_energetique,
                  name = "Réel",
                  line = list(color = "#085041", width = 1.5)) %>%
        add_lines(data = df, x = ~date, y = ~pred_xgb,
                  name = "Prédiction XGBoost",
                  line = list(color = "#EF9F27", width = 1.5, dash = "dot")) %>%
        layout(
          xaxis     = list(title = ""),
          yaxis     = list(title = "Demande (MW)"),
          legend    = list(orientation = "h", y = 1.1),
          hovermode = "x unified"
        )
    })
    
    output$graphe_residus <- renderPlotly({
      df <- df_filtre() %>%
        mutate(residu = demande_energetique - pred_xgb)
      plot_ly(df, x = ~date, y = ~residu,
              type = "scatter", mode = "lines",
              line = list(color = "#993C1D", width = 0.8)) %>%
        add_lines(x = range(df$date), y = c(0, 0),
                  line = list(color = "#888", dash = "dash"),
                  showlegend = FALSE) %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "Résidu (MW)"))
    })
    
    # ----- Onglet "Analyse exploratoire"-----
    # Output: Profil saisonnier
    output$graphe_saisonnier <- renderPlotly({
      df_fusionne %>% 
        mutate(mois_label = month(date, label = TRUE, abbr = TRUE)) %>% 
        group_by(mois_label) %>% 
        summarise(moy = mean(demande_energetique, na.rm =TRUE),
                  .groups = "drop") %>% 
        plot_ly(x = ~mois_label, y = ~moy, type = "scatter", mode ="lines+markers",
                line = list(color="#085041", width = 2),
                marker = list(color = "#085041", size = 8)) %>% 
        layout(xaxis = list(title = ""),
               yaxis = list(title = "Demande moyenne (MW)"))
    })
    
    # Output relation demande ~ température
    output$graphe_temp <- renderPlotly({
      df_plot <- df_fusionne %>%
        mutate(temp_bin = cut(temperature, breaks = seq(-30, 40, by = 2))) %>%
        group_by(temp_bin) %>%
        summarise(demande_moy = mean(demande_energetique, na.rm = TRUE),
                  temp_mid    = mean(temperature, na.rm = TRUE),
                  n           = as.numeric(n()), .groups = "drop") %>%
        filter(n > 20) 
      
      p <- ggplot(df_plot, aes(x = temp_mid, y = demande_moy, size = n)) + 
        geom_point(color = "#A0522D", alpha = 0.7) +
        labs(x="Température (°C)", y = "Demande moyenne (MW)") + 
        theme_minimal(base_size = 12)
      
      ggplotly(p)
    })
    
    # Profil horaire semaine VS weekend
    output$graphe_horaire <- renderPlotly({
      df_fusionne %>%
        mutate(
          heure     = hour(date),
          type_jour = ifelse(wday(date) %in% c(1, 7), "Weekend", "Semaine")
        ) %>%
        group_by(heure, type_jour) %>%
        summarise(demande_moy = mean(demande_energetique, na.rm = TRUE),
                  .groups = "drop") %>%
        plot_ly(x = ~heure, y = ~demande_moy, color = ~type_jour,
                type = "scatter", mode = "lines",
                colors = c("Semaine" = "#085041", "Weekend" = "#EF9F27")) %>%
        layout(xaxis = list(title = "Heure"),
               yaxis = list(title = "Demande moyenne (MW)"),
               legend = list(orientation = "h", y = 1.1))
    })
    
    # Output: Évolution temporelle
    output$graphe_evolution <- renderPlotly({
      df_fusionne %>%
        group_by(semaine = floor_date(date, "week")) %>%
        summarise(demande_moy = mean(demande_energetique, na.rm = TRUE),
                  .groups = "drop") %>%
        plot_ly(x = ~semaine, y = ~demande_moy, type = "scatter",
                mode = "lines",
                line = list(color = "#085041", width = 1)) %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "Demande moyenne (MW)"))
    })
    
    
    # ----------- Onglet Modélisation --------------
    
    # Tableau de comparaisons des modèles
    output$table_comparaison <- renderTable({
      data.frame(
        Modèle           = c("Baseline (lag 24h)", "Régression linéaire", "XGBoost"),
        `MAE (MW)`       = c(1100.69, 707.50, round(mae_xgb, 2)),
        `MAPE (%)`       = c(4.77, 3.25, round(mape_xgb, 2)),
        `R²`             = c(0.8990, 0.9648, round(r2_xgb, 4)),
        `Taux préd. (%)` = c(95.23, 96.75, round(100 - mape_xgb, 2)),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 2)
    
    # Output: Variables importantes
    output$vip_plot <- renderPlot({
      vip(fit_final, num_features = 20) +
        labs(title = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(size = 10))
    })
    
    # Output: Courbe d'apprentissage
    output$courbe_apprentissage <- renderPlotly({
      meilleurs <- select_best(resultats_tuning, metric = "mae")
      
      courbe_val <- collect_metrics(resultats_tuning, summarize = FALSE) %>%
        filter(.metric == "mae", .config == meilleurs$.config) %>%
        arrange(id) %>%
        mutate(fold_num = row_number())
      
      plot_ly() %>%
        add_lines(data = courbe_val, x = ~fold_num, y = ~.estimate,
                  name = "Validation",
                  line = list(color = "#085041", width = 1.5)) %>%
        add_lines(x = range(courbe_val$fold_num),
                  y = c(mae_xgb, mae_xgb),
                  name = "MAE Test final",
                  line = list(color = "#EF9F27", dash = "dash", width = 1.5)) %>%
        layout(xaxis = list(title = "Fold"),
               yaxis = list(title = "MAE (MW)"),
               legend = list(orientation = "h", y = 1.1))
    })
    


}


shinyApp(ui, server)