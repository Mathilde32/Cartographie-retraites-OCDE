# app.R

library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmltools)

# ----------------- Données -----------------

df <- read_excel("/Users/mathilde/Documents/TSE/M2/S1/Alternance/Réforme retraites/2. BDD/Data_clustering_retraites.xlsx", sheet = "dataclean_cluster") %>%
  rename(VARIABLE = 1) %>%
  pivot_longer(-VARIABLE, names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = VARIABLE, values_from = value)

df$country[df$country == "United States"] <- "United States of America"

df <- df %>%
  mutate(across(-country, ~ as.numeric(gsub(",", ".", .))))

world <- ne_countries(scale = "medium", returnclass = "sf")

world_ocde <- world %>%
  left_join(df, by = c("name" = "country")) %>%
  filter(!is.na(cluster)) %>%
  mutate(cluster = as.character(cluster))

name_translation <- c(
  "Australia" = "Australie", "Austria" = "Autriche", "Belgium" = "Belgique",
  "Canada" = "Canada", "Denmark" = "Danemark", "Finland" = "Finlande",
  "France" = "France", "Germany" = "Allemagne", "Greece" = "Grèce",
  "Ireland" = "Irlande", "Italy" = "Italie", "Netherlands" = "Pays-Bas",
  "New Zealand" = "Nouvelle-Zélande", "Norway" = "Norvège", "Portugal" = "Portugal",
  "Spain" = "Espagne", "Sweden" = "Suède", "Switzerland" = "Suisse",
  "United Kingdom" = "Royaume-Uni", "United States of America" = "États-Unis"
)

world_ocde$name_fr <- name_translation[world_ocde$name]

# ----------------- Paramètres -----------------

variables <- c(
  "Taux de remplacement net (%)" = "net_pens_replacement_rate",
  "Âge effectif de départ à la retraite (ans)" = "effective_retirement_age",
  "Taux d'emploi des 55-64 ans (%)" = "empl_rate_55_64y",
  "Revenu relatif des +65 ans" = "incomes_old",
  "Nombre de régimes de retraite" = "pension_schemes_number",
  "Actifs des fonds de pension (% PIB)" = "pension_assets",
  "Typologie (cluster)" = "cluster"
)

cluster_colors <- c(
  "1" = "#E63946",  # Continentaux
  "2" = "#A773D3",  # Nordiques
  "3" = "#1D3557",  # Anglo-Saxons
  "4" = "#F5C32A"   # Méditerranéens
)

cluster_labels <- c(
  "1" = "Pays Continentaux",
  "2" = "Pays Nordiques",
  "3" = "Pays Anglo-Saxons",
  "4" = "Pays Méditerranéens"
)

# ----------------- UI -----------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      #map {
        height: 100vh !important;
      }
      .leaflet-control {
        z-index: 1000;
      }
      .absolute-panel {
        position: absolute;
        top: 20px;
        left: 20px;
        z-index: 999;
        background-color: white;
        padding: 10px;
        border-radius: 8px;
        box-shadow: 0 0 15px rgba(0,0,0,0.3);
      }
    "))
  ),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    class = "absolute-panel",
    draggable = TRUE,
    selectInput("selected_var", "Choisissez une variable :", choices = variables, selected = "cluster")
  )
)

# ----------------- SERVER -----------------

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    selected_var <- input$selected_var
    
    if (selected_var == "cluster") {
      pal <- colorFactor(
        palette = unname(cluster_colors),
        domain = names(cluster_colors)
      )
      fill_color <- ~pal(cluster)
      legend_pal <- pal
      legend_values <- ~cluster
      legend_title <- "Typologie OCDE (cluster)"
      legend_labels <- cluster_labels
    } else {
      vals <- world_ocde[[selected_var]]
      pal <- colorNumeric(palette = "Blues", domain = vals, na.color = "transparent")
      fill_color <- ~pal(get(selected_var))
      legend_pal <- pal
      legend_values <- ~get(selected_var)
      legend_title <- names(variables)[variables == selected_var]
      legend_labels <- NULL
    }
    
    leaflet(world_ocde) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10, lat = 50, zoom = 2.5) %>%
      addPolygons(
        fillColor = fill_color,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", name_fr, "</strong><br/>",
          legend_title, " : ", if (selected_var == "cluster") cluster_labels[cluster] else format(round(get(selected_var), 2), nsmall = 2)
        ), HTML),
        highlightOptions = highlightOptions(
          color = "blue",
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "topright",
        pal = legend_pal,
        values = legend_values,
        title = legend_title,
        labFormat = if (!is.null(legend_labels)) {
          labelFormat(transform = function(x) legend_labels[as.character(x)])
        } else {
          labelFormat()
        }
      )
  })
}

# ----------------- Lancement -----------------

shinyApp(ui, server)
