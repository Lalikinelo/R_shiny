library(shiny)
library(dplyr)
library(sf)
library(terra)
library(leaflet)
library(tidyverse)
library(rjson)
library(jsonlite)


# get geojson data from url

#download.file(url = "https://data.education.gouv.fr/explore/dataset/fr-esr-parcoursup/download/?format=geojson&timezone=Europe/Berlin&lang=fr",destfile = "data/parcoursup_test.geojson")
#parcoursup <- st_read("data/parcoursup_test.geojson")


# Creation des inputs que l'utilisateur va choisir
choices_diplom=parcoursup %>%
  select(form_lib_voe_acc)%>%
  group_by(form_lib_voe_acc)%>%
  summarise()

choices_typ_bac=list("Techno","Generale","Pro")

choices_dep = parcoursup %>% 
  st_drop_geometry() %>% 
  group_by(dep_lib) %>% 
  summarise()

#################### Interface Shiny ######################

ui <- fluidPage(
  titlePanel("Mes chances d'admission"),
  sidebarLayout(
    sidebarPanel(  
      selectInput(inputId="wished_diplom", 
                  label= "Choisir la formation visée",
                  choices_diplom$form_lib_voe_acc,
                  selected = "BTS - Production"),
      selectInput(inputId="type_bac_origin", 
                  label= "Choisir le bac obtenu",
                  choices_typ_bac,
                  selected = "Pro"),
      selectInput(inputId="dep",
                  label= "Choisir un departement", 
                  choices_dep,
                  selected = "Rhône"),
      fluid = TRUE,
    ),#sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", leafletOutput("etab_avec_taux_adm")),
        textOutput("test")),
    )#mainPanel
  )#sidebarLayout
)#fluidPage




server <- function(input, output) {
  
  output$etab_avec_taux_adm <- renderLeaflet({
    
    
    
    
    
    # choix des stat d'admission à afficher en fonction du bac d'origine choisi, de la filiere choisie et du departement choisi
    etab_stat=parcoursup %>% 
      filter(dep_lib==input$dep) %>% 
      filter(form_lib_voe_acc==input$wished_diplom) %>% 
      select(g_ea_lib_vx,prop_tot_bt, prop_tot_bg,prop_tot_bp,prop_tot)
    
    
    # Choisi la bonne statistique en fonction du bac d'origine
    if (input$type_bac_origin =="Techno" ) {
      etab_stat <- etab_stat %>% 
        mutate(tx_prop=(prop_tot_bt/prop_tot)*100)
    }
    if (input$type_bac_origin=="Generale" ) {
      etab_stat <- etab_stat %>% 
        mutate(tx_prop=(prop_tot_bg/prop_tot)*100)
    }
    if (input$type_bac_origin=="Pro" ) {
      etab_stat <- etab_stat %>% 
        mutate(tx_prop=(prop_tot_bp/prop_tot)*100)
    }
    

  
    etab_stat <- etab_stat %>%
    mutate(long = unlist(map(etab_stat$geometry,1)),
           lat = unlist(map(etab_stat$geometry,2)))
    
    
    if(nrow(etab_stat) == 0)
    {
      leaflet() %>%
        addTiles() %>% 
        addControl(html ="<p style='display: inline-block; '> Aucun etablissement ne correspond à ces critères </p>", position= "topleft")
    }
    else
    {
    
      print(nrow(etab_stat))
      
      
      getColor <- function(etab_stat) {
        sapply(etab_stat$tx_prop, function(tx_prop) {
          if(tx_prop >= 50 & !is.nan(tx_prop)) {
            "green"
          } else if(tx_prop >= 25 & !is.nan(tx_prop)) {
            "orange"
          } else {
            "red"
          } })
      }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(etab_stat)
    )
    
    IconSet <- awesomeIconList(
      "plus de 50%"   = makeAwesomeIcon(icon= 'ios-close', markerColor = 'green', iconColor = 'black', library = "ion"),
      "de 25 a 50%" = makeAwesomeIcon(icon= 'ios-close', markerColor = 'orange', iconColor = 'black', library = "ion"),
      "moins de 25%" = makeAwesomeIcon(icon= 'ios-close', markerColor = 'red', iconColor = 'black', library = "ion")
    )
    
    
    # legend html generator:
    markerLegendHTML <- function(IconSet) {
      
      # container div:
      legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Taux d'admission avec ce bac</h4>"
      
      n <- 1
      # add each icon for font-awesome icons icons:
      for (Icon in IconSet) {
          legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                              "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                              "<i style='margin-left: 5px; margin-top: 11px; 'class= 'ion ion-",Icon[["icon"]]," ion-inverse'></i>",
                              "</div>",
                              "<p style='margin-left: 17px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                              "</div>")    
        
        n<- n + 1
      }
      paste0(legendHtml, "</div>")
    }
    
    
    
    
      leaflet(etab_stat) %>% 
        addTiles() %>% 
        addAwesomeMarkers(~long, ~lat, icon=icons, label = paste(etab_stat$g_ea_lib_vx,"\n", as.character(as.integer(etab_stat$tx_prop)), "%"))%>% 
        addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")
    }
  })                               
}

shinyApp(ui = ui, server = server)