library(shiny)
library(dplyr)
library(sf)
library(terra)
library(leaflet)
library(tidyverse)




parcoursup <- st_read("data/parcoursup.shp")


# Creation des inputs que l'utilisateur va choisir
choices_diplom=parcoursup %>%
  select(form_lib_vo)%>%
  group_by(form_lib_vo)%>%
  summarise()

choices_typ_bac=list("Techno","Generale","Pro")

choices_dep = parcoursup %>% 
  st_drop_geometry() %>% 
  group_by(dep_lib) %>% 
  summarise()


ui <- fluidPage(
  selectInput(inputId="wished_diplom", label="Choisir la formation visÃ©e", choices_diplom$form_lib_vo),
  selectInput(inputId="type_bac_origin", label="Choisir le bac obtenu", choices_typ_bac),
  selectInput(inputId="dep", label="Choisir un departement", choices_dep),
  
  mainPanel(
    leafletOutput("etab_avec_taux_adm"),
    textOutput("test")
  )
)





server <- function(input, output) {
  
  output$etab_avec_taux_adm <- renderLeaflet({
    
    
    
    
    
    # choix des stat d'admission à afficher en fonction du bac d'origine choisi, de la filiere choisie et du departement choisi
    etab_stat=parcoursup %>% 
      filter(dep_lib==input$dep) %>% 
      filter(form_lib_vo==input$wished_diplom) %>% 
      select(g_ea_lib_vx,prop_tot_bt, prop_tot_bg,prop_tot_bp,prop_tot)
    
    print(input$type_bac_origin)
    
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
  
    print(etab_stat)
    
    
    getColor <- function(etab_stat) {
      sapply(etab_stat$tx_prop, function(tx_prop) {
        if(tx_prop >= 50) {
          "green"
        } else if(tx_prop >= 25) {
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
  
    leaflet(etab_stat) %>% 
      addTiles() %>% 
      addAwesomeMarkers(~long, ~lat, icon=icons, label = ~as.character(tx_prop))
  })
}

shinyApp(ui = ui, server = server)