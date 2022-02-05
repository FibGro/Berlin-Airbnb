
# Design header
header <- dashboardHeader(title = "Berlin Airbnb")

#----------------------------------------------------------------------------------------# 

# Design sidebar with four menus
sidebar <- dashboardSidebar(
  
  
  sidebarMenu(
    menuItem(text = "Overview", icon = icon("image"), tabName = "over"),
    menuItem(text = "Neighbourhood", icon = icon("city"), tabName = "reg"),
    menuItem(text = "Interactive Map", icon = icon("globe-europe"), tabName = "map"),
    menuItem(text = "Data", icon = icon("table"), tabName = "dat"),
    menuItem("Source Code", icon = icon("code"), 
             href = "https://github.com/FibGro/Berlin-Airbnb")

  )
)

#----------------------------------------------------------------------------------------# 
# Design body dashboard
body <- dashboardBody(
  
  # Use my theme (remember to put this first and then other theme! Otherwise, it will not work!)
  use_theme(mytheme),
  
  # Other design theme based on HTML 

  tags$style("*{font-family:'Arial Narrow';}"),
  tags$head(tags$style(HTML('/* logo */.skin-blue .main-header .logo {background-color: black;font-family:"Arial Narrow"; font-size: 20px; height:50px;}'))),
  tags$style(HTML(".main-sidebar { font-family:'Arial Narrow'; font-size: 16px; }")),
  tags$p(tags$style(HTML('.box {font-family:"Arial Narrow"; font-size: 17px; color:white;}'))),
  tags$style(type = 'text/css','.bg-red {background-color: #8B9A46!important; }'),
  tags$style(type = 'text/css','.bg-purple {background-color: #D4AC2B!important; }'),
  tags$style(type = 'text/css','.bg-aqua {background-color: #A1B57D!important; }'),
  tags$style(type = 'text/css','.bg-navy {background-color: #105652!important; }'),
  tags$p(tags$style(HTML('.nav-tabs>li>a {font-family: "Arial Narrow";font-size: 18px;}'))),
  
  # Change the InfoBox (Sooo long code)
  tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#666666
                    }

.box.box-solid.box-primary{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}

                                    ")),
  tags$style(HTML("


.box.box-solid.box-danger>.box-header {
  color:white;
  background:#0a0a0a
                    }

.box.box-solid.box-danger{
border-bottom-color:#0a0a0a;
border-left-color:#0a0a0a;
border-right-color:#0a0a0a;
border-top-color:#0a0a0a;
}

.box.box-danger>.box-header {
  color:#000000;
  background:#fff
                    }

.box.box-danger{
border-bottom-color:#0a0a0a;
border-left-color:#0a0a0a;
border-right-color:#0a0a0a;
border-top-color:#0a0a0a;
}")),
  
#----------------------------------------------------------------------------------------#  

#NOTE: I DIDN'T MAKE TABITEM IN ORDER SOO, FOLLOW CAREFULLY!!!!

#----------------------------------------------------------------------------------------# 

  # tabItems
  tabItems(
    
    # Menu tab 3 (Interactive Map)
    tabItem(tabName = "map",
            fluidRow(
              column(width = 9, 
                     leafletOutput(outputId = "map", height="740px")),
              
              box(width =3, height = "740px", background="black",
                br(),
                p("This interactive map describes quantity and location of accommodation."),  
                br(),
                p("To get the quantity of the accomodation, select desired price ranges, neighbourhood group, and room type."),
                br(),
                p(" Then click the circle marker on the map to get the location for each accommodation. Label is provided by clicking the red dot"),
                br(),
                setSliderColor("#84A98C",1),
                sliderInput(value = c(0,2000),
                            label = "Choose Price Range :",
                            inputId = "price.1",
                            min=0,
                            max=2000),
                br(),
                selectInput(label = "Choose Neighbourhood Group :",
                            inputId = "neigh",
                            choices = c("All", levels(listing$neighbourhood_group))),
            
                selectInput(label = "Choose Room Type :",
                            inputId = "room",
                            choices = c("All", levels(listing$room_type)))))),
            
#----------------------------------------------------------------------------------------#

    # Menu tab 2 (Region)
    tabItem(tabName = "reg",
            
            fluidRow(
              #Dynamic Box
              infoBoxOutput("reg1"),
              infoBoxOutput("reg2"),
              infoBoxOutput("reg3")
            ),
            
            br(),
            
            fluidRow(
            
              tabBox(width=9,  id = "tabset2", height= "600px",
                     tabPanel( strong("Price Distribution"), 
                               br(),
                               br(),
                               
                               column(width =12, 
                                      plotlyOutput(outputId = "plot5", height="500px")
                               )),
                    
                     tabPanel( strong("Price Correlation"), 
                               br(),
                               br(),
                               
                               column(width =12,
                                      plotlyOutput(outputId = "plot14", height="500px")
                               )),
                     
                     tabPanel( strong("Room Type and Availability"), 
                               br(),
                               br(),
                               
                               column(width =12,
                                      plotlyOutput(outputId = "plot8", height="500px")
                               ))
                     
              ),
              
               box(width =3, height = "600px", background="black",
          
                   p("Choose neighbourhood group to observe distribution and correlation plot"),
                   br(),
                   br(),
                   selectInput(label = "Choose Neighbourhood Group :",
                               inputId = "corr1",
                               choices = levels(listing$neighbourhood_group)
              )))),
            
#-----------------------------------------------------------------------------------#          

    # Menu tab 1 (Overview)
    tabItem(tabName = "over",
           
            h1(strong("Berlin Airbnb Analysis"), style = "font-size:30px; font-family: 'Arial Narrow'; color: black;"),
            br(),
            p("Airbnb is an American company that operates online marketplaces for lodging, such as vacation rentals, homestays and tourism activities", a(href = "https://en.wikipedia.org/wiki/Airbnb", "(Wikipedia, 2021)"),". Starting in 2010, Airbnb has changed the way people travel by offering a wider choice of places with a variety of price ranges and minimum nights to stay. More and more people use Airbnb services to promote their accommodation. Berlin is Germany's premier city in Western Europe with more than 15,000 vacation rentals registered in Airbnb. In this analysis, we would like to observe the overview of accommodation in Berlin in terms of room type, number of reviews and price. In addition, the correlation between few factors and also price distribution on selected neighbourhood group will be presented through graphs. Finally, an interactive map will be displayed to provide the quantity and location of the accommodation based on selected price ranges, neighbourhood group and room type.", style = "font-size:19px; font-family: 'Arial Narrow'; color: black;"),
            br(),
            br(), 

            fluidRow(
              tabBox(width=8,  id = "tabset1", height= "500px",
                     tabPanel( strong("Room Type"), 
                               br(),
                               br(),
                               
                               column(width =12, 
                                      plotlyOutput(outputId = "plot12", height="400px")
                               )),
                     
                     tabPanel( strong("Price"), 
                               br(),
                               br(),
                               
                               column(width =12, 
                                      plotlyOutput(outputId = "plot11", height="400px")
                     )),
                     tabPanel( strong("Reviews"), 
                               br(),
                               br(),
                               
                               column(width =12,
                                      plotlyOutput(outputId = "plot13", height="400px"))),
                     
                     tabPanel( strong("Price in Neighbourhood"), 
                               br(),
                               br(),
                               
                               column(width =12, 
                                      plotlyOutput(outputId = "plot1", height="400px"))),
                     
                     tabPanel( strong("Room Type in Neighbourhood"), 
                               br(),
                               br(),
                               
                               column(width =12, 
                                      plotlyOutput(outputId = "plot2", height="400px")))
              ),
              
              column(width=4,
                     br(),
                     br(),
                     br(),
                     infoBoxOutput("over1", width=12),
                     
                     infoBoxOutput("over2", width=12),
                     
                     infoBoxOutput("over3", width=12),
                     
                     infoBoxOutput("over4", width=12),
                     
                     #infoBoxOutput("over5", width=12)
              )
    ))

    ,
#---------------------------------------------------------------------------------------#
      # menu tab 4 (DATA - DONE)
      tabItem(tabName = "dat",
          h2(tags$b("Berlin Airbnb Listing")),
          br(),
          DT::dataTableOutput("summary_table")
        )

#---------------------------------------------------------------------------------------#
# PUT THIS IN HERE BECAUSE SOMETIMES TO FORGET () CHARACTER!!!

))
            
        
#----------------------------------------------------------------------------------------# 

dashboardPage(header = header, sidebar = sidebar, body = body)