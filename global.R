# Importing all libraries! Important! If you can render locally and can't publish it, it must be your library!!

library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)
library(htmltools)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(terra)
library(bslib)
library(fresh)
library(ggplot2)
library(plotly)
library(tidyr)
library(wesanderson)
library(rmapshaper)
library(glue)
library(ggthemes)
library(fontawesome)
library(scales)



# Upload files
listing <- read.csv("listings_berlin.csv")
region.map <- readOGR("neighbourhoods.geojson")

# Use this code for faster rendering the leaflet
region.map <- rmapshaper::ms_simplify(region.map, keep=0.01, keep_shapes=TRUE)

# Clean listing file
listing <-
  listing %>% 
  mutate(room_type=as.factor(room_type),
         neighbourhood=as.factor(neighbourhood),
         neighbourhood_group=as.factor(neighbourhood_group)) 

# Clean listing data for tab4 
listing.clean <-
  listing %>% 
  select(-c(last_review, calculated_host_listings_count, reviews_per_month, host_id))

# Create bins and color function for leaflet

col1 <- c(50,200,1500, 15000)

pal1 <- colorFactor(
  palette = colorRampPalette(rainbow(4))(length(col1)), 
  domain = c(0,15000))

# creating my theme (library fresh)
mytheme <- create_theme(
  
  adminlte_color(
    light_blue = "black"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#5d6063",
    dark_hover_bg = "black",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "white",
    info_box_bg = "white"
  )
)


# Creating a new column called ava.group
convert_ava <- function(y){ 
  if( y >= 0 & y <100){
    y <- "0 - 100 days"
  }else 
    if(y >= 100 & y< 200){
      y <- "100 -200 days"
    }else{
      y <- "200-365 days" 
    }  
}

# Data with listing ava and zero
zero <- listing[listing$price != 0,]
listing.ava <- zero
listing.ava$ava.group <- sapply(X =listing.ava$availability_365, FUN = convert_ava) 
listing.ava$ava.group <- as.factor(listing.ava$ava.group)
