function(input, output, session) {
  
output$map <- renderLeaflet({
  
  col1 <- c(1,10,100,16000)
  bins <- c(1,10,100,16000)
  pal1 <- colorBin(palette = c("green","yellow","orange"), 
                   domain =c(1,20000),
                   bins=bins)
  
   if(input$neigh == "All" & input$room == "All"){
  
    listing.filter <- 
      listing %>% 
      filter(price >= input$price.1[1] & price <= input$price.1[2]) 
  
    
    leaflet() %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    setView(lng = 13.4050 , lat =52.5200, zoom=10) %>% 
    addPolygons(data= region.map, 
                color="#A1B57D",
                weight=1,
                smoothFactor = 0.5) %>% 
      
    addCircleMarkers(
                     lat = listing.filter$latitude,
                     lng= listing.filter$longitude,
                     color="red",
                     radius=3,
                     weight=1,
                     popup = paste('Host Name :', listing.filter$host_name,'<br/>',
                                   'Room Type :', listing.filter$room_type,'<br/>',
                                   'Price : €', scales::comma(listing.filter$price),'<br/>',
                                   'Number of Reviews :', listing.filter$number_of_reviews,'<br/>'),
                     clusterOptions = markerClusterOptions(
                     showCoverageOnHover = FALSE)) %>% 
      addLegend(pal=pal1,
                values= col1,
                opacity = 0.7,
                position="bottomright",
                title="Number of Listings")
    
     
   }else if(input$neigh =="All" & input$room != "All"){
     
     col1 <- c(1,10,100,16000)
     bins <- c(1,10,100,16000)
     pal1 <- colorBin(palette = c("green","yellow","orange"), 
                      domain =c(1,20000),
                      bins=bins)
     
     listing.filter <- 
       listing %>% 
       filter(price >= input$price.1[1] & price <= input$price.1[2]) %>% 
       filter(room_type == input$room) 
       
     
     leaflet() %>% 
       addProviderTiles(providers$Stamen.TonerLite) %>% 
       setView(lng = 13.4050 , lat =52.5200, zoom=10) %>% 
       addPolygons(data= region.map, 
                   color="#A1B57D",
                   weight=1,
                   smoothFactor = 0.5) %>% 
       addCircleMarkers(lat = listing.filter$latitude,
                        lng= listing.filter$longitude,
                        color="red",
                        radius=3,
                        weight=1,
                        popup = paste('Host Name :', listing.filter$host_name,'<br/>',
                                      'Room Type :', listing.filter$room_type,'<br/>',
                                      'Price : €', scales::comma(listing.filter$price),'<br/>',
                                      'Number of Reviews :', listing.filter$number_of_reviews,'<br/>'),
                        clusterOptions = markerClusterOptions(
                          showCoverageOnHover = FALSE)) %>% 
       addLegend(pal=pal1,
                 values= col1,
                 opacity = 0.7,
                 position="bottomright",
                 title="Number of Listings")
     
   }else if(input$room =="All" & input$neigh != "All"){
     
     listing.filter <- 
       listing %>% 
       filter(price >= input$price.1[1] & price <= input$price.1[2]) %>% 
       filter(neighbourhood_group == input$neigh) 
     
     
     col1 <- c(1,10,100,16000)
     bins <- c(1,10,100,16000)
     pal1 <- colorBin(palette = c("green","yellow","orange"), 
                      domain =c(1,20000),
                      bins=bins)
     
     leaflet() %>% 
       addProviderTiles(providers$Stamen.TonerLite) %>% 
       setView(lng = 13.4050 , lat =52.5200, zoom=10) %>% 
       addPolygons(data= region.map, 
                   color="#A1B57D",
                   weight=1,
                   smoothFactor = 0.5) %>% 
       addCircleMarkers(lat = listing.filter$latitude,
                        lng= listing.filter$longitude,
                        color="red",
                        radius=3,
                        weight=1,
                        popup = paste('Host Name :', listing.filter$host_name,'<br/>',
                                      'Room Type :', listing.filter$room_type,'<br/>',
                                      'Price : €', scales::comma(listing.filter$price),'<br/>',
                                      'Number of Reviews :', listing.filter$number_of_reviews,'<br/>'),
                        clusterOptions = markerClusterOptions(
                          showCoverageOnHover = FALSE)) %>% addLegend(pal=pal1,
                                                                      values= col1,
                                                                      opacity = 0.7,
                                                                      position="bottomright",
                                                                      title="Number of Listings")
     
}else{
  
  listing.filter <- 
    listing %>% 
    filter(price >= input$price.1[1] & price <= input$price.1[2]) %>% 
    filter(neighbourhood_group == input$neigh) %>% 
    filter(room_type == input$room) 
  
  
  col1 <- c(1,10,100,16000)
  bins <- c(1,10,100,16000)
  pal1 <- colorBin(palette = c("green","yellow","orange"), 
                   domain =c(1,20000),
                   bins=bins)
  
  leaflet() %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    setView(lng = 13.4050 , lat =52.5200, zoom=10) %>% 
    addPolygons(data= region.map, 
                color="#A1B57D",
                weight=1,
                smoothFactor = 0.5) %>% 
    addCircleMarkers(lat = listing.filter$latitude,
                     lng= listing.filter$longitude,
                     color="red",
                     radius=3,
                     weight=1,
                     popup = paste('Host Name :', listing.filter$host_name,'<br/>',
                                   'Room Type :', listing.filter$room_type,'<br/>',
                                   'Price : €', scales::comma(listing.filter$price),'<br/>',
                                   'Number of Reviews :', listing.filter$number_of_reviews,'<br/>'),
                     clusterOptions = markerClusterOptions(
                       showCoverageOnHover = FALSE)
    ) %>% addLegend(pal=pal1,
                    values= col1,
                    opacity = 0.7,
                    position="bottomright",
                    title="Number of Listings")

}

})
  
output$summary_table <- renderDataTable({
  datatable(listing.clean, 
            options = list(pageLength = 7, autoWidth = TRUE, scrollX = T))
  
})

output$plot5 <- renderPlotly({
  
  # create ggplotly
  plot5<-zero %>% 
    filter(neighbourhood_group == input$corr1) %>% 
    
    ggplot(aes(x=price, group=room_type, fill=room_type, text = sprintf("Price (log10) : %.2f<br>Density : %.2f", x, ..density..))) +
    geom_density(adjust=1.5, alpha=.5) +
    scale_x_log10(labels=comma) +
    scale_fill_manual(values = wes_palette("Cavalcanti1"), name= "Room Type")+
    labs(title="Distribution Price Based on Room Type\n(with log(10) transformation of x-axis)",
         y="Density",
         x="Price in log (10)")+
    theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
    theme(
      plot.title = element_text(size = 17),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13 ),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      legend.position = "bottom")
  
  ggplotly(plot5, tooltip = "text")

})

output$plot8 <-renderPlotly({
  
# Create a segment for availability 360 
plot.case8 <- listing.ava %>% 
  filter(neighbourhood_group== input$corr1 ) %>% 
  ggplot(aes(y=availability_365, x=room_type, 
             text=glue("Host Name : {host_name}\nAvailability : {availability_365} nights per year")))+
  geom_jitter(aes(col=ava.group), alpha=0.4)+
  scale_color_manual(values = wes_palette(5, name = "Cavalcanti1"), name = "Range Availability")+
  geom_boxplot(alpha=1)+
  labs(x = "Room Type", y = "Availability (nights)",
       title = "Distribution of Availability Based on Room Type")+
  
  
  theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
  theme(plot.title = element_text(size= 17, color = 'black'),
        axis.title.x = element_text(size=13, color = 'black'),
        axis.title.y = element_text(size = 13, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        axis.text.y = element_text(size = 13, color = 'black'),
        legend.text = element_text(size = 11, color = 'black'),
        legend.title = element_text(size = 12, color = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "right"
  ) 

ggplotly(plot.case8, tooltip="text")

})

output$reg2 <- renderInfoBox({
  
  listing.value2 <- listing %>% 
    filter(neighbourhood_group == input$corr1)
  
  infoBox( strong("Average Availability Per Year"),
           value= HTML("<p style='font-size:45px'>",paste0(median(listing.value2$availability_365)),"Days","</p>"),
           icon = tags$i(icon("calendar"), style="color: white ; font-size: 50px"),
           color = "purple",
           fill=T)
})

output$reg1 <- renderInfoBox({

listing.value1 <- listing %>% 
  filter(neighbourhood_group == input$corr1)

  infoBox( strong("Average Price Per Night"),
           value= HTML("<p style='font-size:45px'>",paste0(median(listing.value1$price)),"</p>"),
           icon = tags$i(icon("euro-sign"), style="color: white ; font-size: 50px"),
           color = "navy",
           fill=T)
  })

output$reg3 <- renderInfoBox({
  
  listing.value3 <- listing %>% 
    filter(neighbourhood_group == input$corr1)
  
  infoBox(
    strong("Average Number of Review"), 
    value= HTML("<p style='font-size:45px'>",paste0(median(listing.value3$number_of_reviews)),"</p>"),
    icon = tags$i(icon("user-edit"), style="color: white ; font-size: 50px"),
    color = "red",
    fill=TRUE
  )
})

output$over4 <- renderInfoBox({
  infoBox(
    strong("The Most Expensive Area"), 
    value= HTML("<p style='font-size:30px'>",paste0("Pankow & Mitte"),"</p>"),
    icon = tags$i(icon("money-bill-wave"), style="color: white ; font-size: 40px"),
    color = "navy",
    fill=TRUE
  )
})

output$over2 <- renderInfoBox({
  infoBox(
    strong("Average Price Per Night"), 
    value= HTML("<p style='font-size:30px'>",paste0("52"),"</p>"),
    icon = tags$i(icon("euro-sign"), style="color: white ; font-size: 40px"),
    color = "red",
    fill=TRUE
  )
})

output$over3 <- renderInfoBox({
  infoBox(
    strong("The Most Reviewed Area"), 
    value= HTML("<p style='font-size:30px'>",paste0("Mitte"),"</p>"),
    icon = tags$i(icon("user-edit"), style="color: white ; font-size: 40px"),
    color = "aqua",
    fill=TRUE
  )
})

output$over1 <- renderInfoBox({
  infoBox(
    strong("The Largest Number Room Type"), 
    value= HTML("<p style='font-size:28px'>",paste0("Entire home/apt"),"</p>"),
    icon = tags$i(icon("home"), style="color: white ; font-size: 40px"),
    color = "purple",
    fill=TRUE
  )
})

output$over5 <- renderInfoBox({
  infoBox(
    strong("The Area With The Highest Number Room"), 
    value= HTML("<p style='font-size:23px'>",paste0("Friedrichshain-Kreuzberg"),"</p>"),
    icon = tags$i(icon("home"), style="color: white ; font-size: 40px"),
    color = "purple",
    fill=TRUE
  )
})

output$plot2 <-renderPlotly({
  
  case2 <- as.data.frame(table(listing$neighbourhood_group, listing$room_type))
  
  plot2<-case2%>% 
    ggplot(aes(y=reorder(Var1, Freq), x= Freq, fill=Var2, 
               text=glue("{Var2}\nNumber of Room : {scales::comma(Freq,accuracy=1)}")))+
    scale_x_continuous(labels = comma)+
    geom_col(width = 0.8, na.rm=TRUE)+
    scale_fill_manual(values = wes_palette("Cavalcanti1"), name = "Room Type")+
    labs(title="Quantity of Room Type Each Neighbourhood",
         y="",
         x="Quantity")+
    theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
    theme(plot.title = element_text(size= 17, color = 'black'),
          plot.subtitle = element_text(size = 8),
          plot.caption = element_text(size = 10),
          axis.title.x = element_text(size=12, color = 'black'),
          axis.title.y = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(size = 12, color = 'black'),
          axis.text.y = element_text(size = 12, color = 'black'),
          legend.text = element_text(size = 11, color = 'black'),
          legend.title = element_text(size = 12, color = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom")
  
  ggplotly(plot2, tooltip="text")

})

output$plot1 <-renderPlotly({
  
  case1 <- 
    listing %>% 
    group_by(neighbourhood_group) %>% 
    summarize(mean.price=median(price)) %>% 
    ungroup() %>% 
    mutate(mean.price=as.integer(mean.price))
  
  
  plot1<- case1 %>% 
    ggplot(aes(y = reorder(neighbourhood_group,mean.price), 
               x = mean.price, 
               text = glue("Average price : € {round(mean.price,2)}")),    na.rm=TRUE) +
    geom_segment(aes(x=0, xend=mean.price, yend=neighbourhood_group), color= "#A1B57D") +
    geom_point(size=4, color="#D4AC2B", fill=alpha("#D4AC2B", 0.1), alpha=0.7, shape=21, stroke=2) +
    xlim(0,65)+
    
    #geom_col(width = 0.8, fill="#A1B57D", na.rm=TRUE) +
    labs(title="Average Price Each Neighbourhood",
         y="",
         x="Price in Euro")+
    
    theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
    theme(plot.title = element_text(size= 17, color = 'black'),
          plot.subtitle = element_text(size = 8),
          plot.caption = element_text(size = 10),
          axis.title.x = element_text(size=12, color = 'black'),
          axis.title.y = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(size = 12, color = 'black'),
          axis.text.y = element_text(size = 12, color = 'black'),
          legend.text = element_text(size = 11, color = 'black'),
          legend.title = element_text(size = 12, color = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "")
  
  ggplotly(plot1, tooltip="text")
})

output$plot11 <-renderPlotly({
  
plot11<-zero %>% 
  #filter(neighbourhood_group=="Mitte") %>% 
  
  ggplot(aes(x=price, fill=price, text = sprintf("Price (log10) : %.2f<br>Density : %.2f", x, ..density..))) +
  geom_density(adjust=1.5, alpha=.6, fill="#8B9A46", color="#105652") +
  geom_vline(xintercept = round(median(zero$price), 2), size = 2, linetype = 3, color= "#D4AC2B")+
  annotate("text", x = 150, y = 1.5,label = paste("Median price : €", paste0(round(median(zero$price), 2))),
           color =  "#D4AC2B", size = 6)+
  
  #scale_fill_manual(values = wes_palette("Cavalcanti1"), name= "Room Type")+
  labs(title="Distribution Price (with log(10) transformation of x-axis)",
       y="Density",
       x="Price (€) in log(10)")+
  theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13 ),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "bottom")+
  scale_x_log10(labels=comma) 


ggplotly(plot11, tooltip = "text")

})

output$plot12 <-renderPlotly({
  
  
  case12<- as.data.frame(table(listing$room_type))
  
  colors <- c('rgb(119, 126, 76)', 
              'rgb(128,133,133)', 
              'rgb(212, 172, 43)', 
              'rgb(171,104,87)')
  
  
  t <- list(
    family = "Arial Narrow",
    size = 30,
    color = 'black')
  
  f <- list(
    family = "Arial Narrow",
    size = 16,
    color = 'black')
  
  plot12 <- plot_ly(
    case12, 
    labels = ~Var1, 
    values = ~Freq, 
    type = 'pie',
    textposition = 'outside',
    textinfo = 'label+percent',
    textfont=f,
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~paste('Quantity :', scales::comma(Freq)),
    marker = list(colors = colors,
                  line = list(color = '#FFFFFF', 
                              width = 1)),
    showlegend = F) %>% 
    layout(title = list(text='Proportion of Room Type', 
                        y = 0.97, font=t,
                        x = 0.50, 
                        xanchor = 'center', 
                        yanchor =  'top',
                        size="200px"),
           margin = list(b = 70, l = 50, t=50),
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
  
  plot12
  
  
  
  
  
})

output$plot13 <-renderPlotly({
  
  case13<-listing %>% 
    group_by(neighbourhood_group) %>% 
    summarize(sum.review = sum(number_of_reviews),
              ave=round(mean(number_of_reviews),2)) %>% 
    arrange(-ave)
  
  plot13 <- case13 %>% 
    ggplot(aes(y=reorder(neighbourhood_group, sum.review), x= sum.review, fill= neighbourhood_group, text=glue("Total Review : {scales::comma(sum.review)}"))) +
    geom_col(width = 0.8, na.rm=TRUE) +
    scale_x_continuous(labels = comma)+
    labs(title="Total Number of Reviews Each Neighbourhood",
         y="",
         x="Number of Review")+
    scale_fill_manual(values = wes_palette(21, name = "Cavalcanti1", type = "continuous"), name = "")+
    
    theme_set(theme_minimal() + theme(text = element_text(family="Arial Narrow"))) +
    theme(plot.title = element_text(size= 17, color = 'black'),
          plot.subtitle = element_text(size = 8),
          plot.caption = element_text(size = 10),
          axis.title.x = element_text(size=12, color = 'black'),
          axis.title.y = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(size = 12, color = 'black'),
          axis.text.y = element_text(size = 12, color = 'black'),
          legend.text = element_text(size = 11, color = 'black'),
          legend.title = element_text(size = 12, color = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "")
  
  
  hide_legend(ggplotly(plot13, tooltip="text"))
})

output$plot14 <-renderPlotly({
  
  listing.ava.1 <- listing.ava %>% 
    select(host_name, neighbourhood_group, price, number_of_reviews, availability_365, minimum_nights)
  
  listing.ava.1 <- pivot_longer(listing.ava.1,
                                cols=c("number_of_reviews", "availability_365", "minimum_nights"))
  
  listing.ava.1$name[listing.ava.1$name ==  "number_of_reviews"] <- "Number Reviews"
  listing.ava.1$name[listing.ava.1$name ==  "minimum_nights"] <- "Minimum Nights"
  listing.ava.1$name[listing.ava.1$name ==  "availability_365"] <- "Availability"
  
  plot14<-listing.ava.1 %>% 
    filter(neighbourhood_group==input$corr1) %>% 
    ggplot(aes(price, value)) + 
    geom_point(mapping=aes(color=name)) + 
    #facet_wrap(~name, scales = "free_x", ncol = 1)
    facet_grid(vars(name), scales = "free") + 
    labs(title="Correlation Between Price, Number of Review Availability and Minimum Night",
         y="",
         x="Price")+
    scale_x_continuous(labels = comma)+
    scale_color_manual(values = wes_palette(3, name = "Cavalcanti1", type = "continuous"))+
    theme_set(theme_bw()+ theme(text = element_text(family="Arial Narrow"))) +
    theme(plot.title = element_text(size= 15, color = 'black'),
          plot.subtitle = element_text(size = 8),
          plot.caption = element_text(size = 10),
          axis.title.x = element_text(size=15, color = 'black'),
          axis.title.y = element_text(size = 20, color = 'black'),
          axis.text.x = element_text(size = 12, color = 'black'),
          axis.text.y = element_text(size = 12, color = 'black'),
          legend.text = element_text(size = 11, color = 'black'),
          legend.title = element_text(size = 12, color = 'black'),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          legend.position = "")+
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    
    theme(strip.text.y = element_text(size = 12))+
    theme(strip.background =element_rect(fill="black"))+
    theme(strip.text = element_text(colour = 'white'))
  
  hide_legend(ggplotly(plot14, tooltip="x"))
  
  
  
  
})

}