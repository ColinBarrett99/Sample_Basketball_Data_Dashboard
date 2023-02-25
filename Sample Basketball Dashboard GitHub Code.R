library(shiny)
library(bslib)

# Modeling
library(modeldata)
library(DataExplorer)

# Widgets
library(plotly)

# Core
library(tidyverse)

library(readxl)  
library(viridis)
library(ggridges)
library(gghalves)
library(shinymanager)


library(scales)
library(ggplot2)
library(fmsb)


Average_Movement_Intensity <- read_excel("SamplePlayerData.xlsx", sheet = "AMI")
Movement_Efficiency <- read_excel("SamplePlayerData.xlsx", sheet = "ME")
Movement_Load <- read_excel("SamplePlayerData.xlsx", sheet = "ML")
Tap <- read_excel("SamplePlayerData.xlsx", sheet = "Tap")
TPM <- read_excel("SamplePlayerData.xlsx", sheet = "TPM")
TRIMP <- read_excel("SamplePlayerData.xlsx", sheet = "TRIMP")
BakerZones <- read_excel("SamplePlayerData.xlsx", sheet = "Baker")
ABrownZones <- read_excel("SamplePlayerData.xlsx", sheet = "A. Brown")
MBrownZones <- read_excel("SamplePlayerData.xlsx", sheet = "M. Brown")
ClaytonZones <- read_excel("SamplePlayerData.xlsx", sheet = "Clayton")
HadawayZones <- read_excel("SamplePlayerData.xlsx", sheet = "Hadaway")
HunterZones <- read_excel("SamplePlayerData.xlsx", sheet = "Hunter")
JamesZones <- read_excel("SamplePlayerData.xlsx", sheet = "James")
RoderickZones <- read_excel("SamplePlayerData.xlsx", sheet = "Roderick")
WilsonZones <- read_excel("SamplePlayerData.xlsx", sheet = "Wilson")
WiznitzerZones <- read_excel("SamplePlayerData.xlsx", sheet = "Wiznitzer")
BakerRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Baker Radar")
ABrownRadar <- read_excel("SamplePlayerData.xlsx", sheet = "A. Brown Radar")
MBrownRadar <- read_excel("SamplePlayerData.xlsx", sheet = "M. Brown Radar")
ClaytonRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Clayton Radar")
HadawayRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Hadaway Radar")
HunterRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Hunter Radar")
JamesRadar <- read_excel("SamplePlayerData.xlsx", sheet = "James Radar")
RoderickRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Roderick Radar")
WilsonRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Wilson Radar")
WiznitzerRadar <- read_excel("SamplePlayerData.xlsx", sheet = "Wiznitzer Radar")
BakerPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Baker PVG")
ABrownPVG <- read_excel("SamplePlayerData.xlsx", sheet = "A. Brown PVG")
MBrownPVG <- read_excel("SamplePlayerData.xlsx", sheet = "M. Brown PVG")
ClaytonPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Clayton PVG")
HadawayPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Hadaway PVG")
HunterPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Hunter PVG")
JamesPVG <- read_excel("SamplePlayerData.xlsx", sheet = "James PVG")
RoderickPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Roderick PVG")
WilsonPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Wilson PVG")
WiznitzerPVG <- read_excel("SamplePlayerData.xlsx", sheet = "Wiznitzer PVG")




data_list1 = list(
  "Average Movement Intensity" = Average_Movement_Intensity,
  "Movement Efficiency"    = Movement_Efficiency,
  "Movement Load" = Movement_Load,
  "Tap Test" = Tap,
  "TRIMP per Minute" = TPM,
  "TRIMP" = TRIMP
)

data_list2 = list(
  "Average Movement Intensity" = Average_Movement_Intensity,
  "Movement Efficiency"    = Movement_Efficiency,
  "Movement Load" = Movement_Load,
  "Tap Test" = Tap,
  "TRIMP per Minute" = TPM,
  "TRIMP" = TRIMP
)

data_list3 = list(
  "Average Movement Intensity" = Average_Movement_Intensity,
  "Movement Efficiency"    = Movement_Efficiency,
  "Movement Load" = Movement_Load,
  "Tap Test" = Tap,
  "TRIMP per Minute" = TPM,
  "TRIMP" = TRIMP
)

data_list4 = list(
  "Player A" = BakerZones,
  "Player B"    = ABrownZones,
  "Player C" = MBrownZones,
  "Player D" = ClaytonZones,
  "Player E" = HadawayZones,
  "Player F" = HunterZones,
  "Player G" = JamesZones,
  "Player H" = RoderickZones,
  "Player I" = WilsonZones,
  "Player J" = WiznitzerZones
)

data_list5 = list(
  "Player A" = BakerRadar,
  "Player B"    = ABrownRadar,
  "Player C" = MBrownRadar,
  "Player D" = ClaytonRadar,
  "Player E" = HadawayRadar,
  "Player F" = HunterRadar,
  "Player G" = JamesRadar,
  "Player H" = RoderickRadar,
  "Player I" = WiznitzerRadar,
  "Player J" = WilsonRadar
)

data_list6 = list(
  "Player A" = BakerPVG,
  "Player B"    = ABrownPVG,
  "Player C" = MBrownPVG,
  "Player D" = ClaytonPVG,
  "Player E" = HadawayPVG,
  "Player F" = HunterPVG,
  "Player G" = JamesPVG,
  "Player H" = RoderickPVG,
  "Player I" = WiznitzerPVG,
  "Player J" = WilsonPVG
)

###########User Interface#########
updateDate <- format(file.info("DateTrigger.R")$mtime, "%m-%d-%y")


ui <- navbarPage("Sample Basketball Data Dashboard",
                 
                 h4 ("Data Last Updated: ", updateDate, align = "right"),
                 theme = bslib::bs_theme(version = 4, bootswatch = "darkly"),
                 
                 tabPanel("Raincloud Plots",sidebarLayout(sidebarPanel(label = "sidebar",
                                                                       width = 3.5,
                                                                       h1("Explore a Variable"),
                                                                       
                                                                       shiny::selectInput(
                                                                         inputId = "dataset_choice3",
                                                                         label   = "Variable Connection",
                                                                         choices = c("Average Movement Intensity", "Movement Efficiency",
                                                                                     "Movement Load", "Tap Test","TRIMP", "TRIMP per Minute")
                                                                       ),
                                                                       # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                       downloadButton("downloadData3",
                                                                                      label = 'Download Plot'),
                                                                       
                                                                       
                 ),
                 
                 mainPanel(plotOutput("plot3", height = "750px"),
                           imageOutput("image3")
                 )
                 
                 )),
                 
                 tabPanel("Practice Zones",sidebarLayout(sidebarPanel(label = "sidebar",
                                                                      width = 3.5,
                                                                      h1("Choose a Player"),
                                                                      
                                                                      shiny::selectInput(
                                                                        inputId = "dataset_choice4",
                                                                        label   = "Player Connection",
                                                                        choices = c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                                    "Player G", "Player H", "Player I", "Player J") 
                                                                      ),
                                                                      # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                      downloadButton("downloadData4",
                                                                                     label = 'Download Plot'),
                                                                      
                                                                      
                                                                      
                 ),
                 
                 mainPanel(plotOutput("plot4", height = "750px"),
                           imageOutput("image4")
                 )
                 # 
                 )),
                 tabPanel("Exertion: Last 7 Days", sidebarLayout(sidebarPanel(label = "sidebar",
                                                                              width = 3.5,
                                                                              h1("Choose a Player"),
                                                                              
                                                                              shiny::selectInput(
                                                                                inputId = "dataset_choice5",
                                                                                label   = "Player Connection",
                                                                                choices = c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                                            "Player G", "Player H", "Player I", "Player J")
                                                                              ),
                                                                              # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                              downloadButton("downloadData5",
                                                                                             label = 'Download Plot'),
                                                                              
                 ),
                 
                 mainPanel(plotOutput("plot5", height = "750px"))
                 )
                 ),
                 
                 tabPanel("Exertion: Practice vs. Game", sidebarLayout(sidebarPanel(label = "sidebar",
                                                                                    width = 3.5,
                                                                                    h1("Choose a Player"),
                                                                                    
                                                                                    shiny::selectInput(
                                                                                      inputId = "dataset_choice6",
                                                                                      label   = "Player Connection",
                                                                                      choices = c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                                                  "Player G", "Player H", "Player I", "Player J")
                                                                                    ),
                                                                                    # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                                    downloadButton("downloadData6",
                                                                                                   label = 'Download Plot'),
                 ),
                 
                 mainPanel(plotOutput("plot6", height = "750px"))
                 )
                 ),
                 
                 
                 
                 tabPanel("Violin Plots",sidebarLayout(sidebarPanel(label = "sidebar",
                                                                    width = 3.5,
                                                                    h1("Explore a Variable"),
                                                                    
                                                                    shiny::selectInput(
                                                                      inputId = "dataset_choice2",
                                                                      label   = "Variable Connection",
                                                                      choices = c("Average Movement Intensity", "Movement Efficiency",
                                                                                  "Movement Load", "Tap Test", "TRIMP", "TRIMP per Minute")
                                                                    ),
                                                                    # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                    downloadButton("downloadData2",
                                                                                   label = 'Download Plot'),
                                                                    
                 ),
                 
                 mainPanel(plotOutput("plot2", height = "750px"),
                           imageOutput("image2")
                 )
                 
                 )),
                 
                 
                 tabPanel("Box Plots",sidebarLayout(sidebarPanel(label = "sidebar",
                                                                 width = 3.5,
                                                                 h1("Explore a Variable"),
                                                                 
                                                                 shiny::selectInput(
                                                                   inputId = "dataset_choice1",
                                                                   label   = "Variable Connection",
                                                                   choices = c("Average Movement Intensity", "Movement Efficiency",
                                                                               "Movement Load", "Tap Test", "TRIMP", "TRIMP per Minute")
                                                                 ),
                                                                 # img(src= "Ohio-Bobcats-Logo.png", height = 100, width = 180, style = "display: block; margin-left: auto; margin-right: auto;"),
                                                                 downloadButton("downloadData1",
                                                                                label = 'Download Plot'),
                                                                 
                 ),
                 mainPanel(plotOutput("plot1", height = "750px"),
                           imageOutput("image1")
                 )
                 
                 ))
                 
                 
)


# ui <- secure_app(ui)


server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  # res_auth <- secure_server(       #     check_credentials = check_credentials(credentials)
  # )
  #
  # output$auth_output <- renderPrint({
  #     reactiveValuesToList(res_auth)
  # })
  
  rv <- reactiveValues()
  
  observe({
    
    rv$data_set1 <- data_list1 %>% pluck(input$dataset_choice1)
    
  })
  
  observe({
    
    rv$data_set2 <- data_list2 %>% pluck(input$dataset_choice2)
    
  })
  
  observe({
    
    rv$data_set3 <- data_list3 %>% pluck(input$dataset_choice3)
    
  })
  
  observe({
    
    rv$data_set4 <- data_list4 %>% pluck(input$dataset_choice4)
    
  })
  
  observe({
    
    rv$data_set5 <- data_list5 %>% pluck(input$dataset_choice5)
    
  })
  
  observe({
    
    rv$data_set6 <- data_list6 %>% pluck(input$dataset_choice6)
    
  })
  
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
  
  # box plots
  output$plot1 <- renderPlot({ ggplot(data=remove_missing(rv$data_set1, na.rm = TRUE, vars = 'Variable'),
                                      aes(x=Variable, y=Value))+
      geom_boxplot()+
      theme_test(base_size = 18) +
      facet_wrap(~factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                          "Player G", "Player H", "Player I", "Player J"))) })
  
  # box plot table 
  output$image1 <- renderImage({
    list(src = paste0("www/",input$dataset_choice1,"sample.png"),
         width = 1219,
         height = 348)
    # alt = "table")
  }, deleteFile = FALSE)
  
  
  #violin plots
  output$plot2 <- renderPlot({ ggplot(data=remove_missing(rv$data_set2, na.rm = TRUE, vars = 'Variable'),
                                      aes(x=Variable, y=Value))+
      geom_boxplot(width = 0.1)+
      geom_violin(alpha = 0.4, fill = 'skyblue1')+
      theme_test(base_size = 18)+
      facet_wrap(~factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                          "Player G", "Player H", "Player I", "Player J"))) })
  
  #violin plot table
  output$image2 <- renderImage({
    list(src = paste0("www/",input$dataset_choice2,"sample.png"),
         width = 1219,
         height = 348)
    # alt = "table")
  }, deleteFile = FALSE)
  
  #raincloud plots
  output$plot3 <- renderPlot({ggplot(rv$data_set3,
                                     aes(x = factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                     "Player G", "Player H", "Player I", "Player J")), y = Value, colour = Athlete, fill = factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                                                                                                                                   "Player G", "Player H", "Player I", "Player J")))) +
      #dot size
      geom_half_point(side = "l", size = 0.8,
                      colour = "black") +
      # lines of the box plot
      geom_half_boxplot(side = "l", width = 0.5,
                        alpha = 0.3, nudge = 0.05,
                        colour = "black") +
      geom_half_violin(aes(colour = Athlete),
                       side = "r",
                       colour = "black",
                       scale =3.5) +
      guides(fill = "none", color = "none") +
      scale_fill_viridis(discrete=TRUE) +
      ylab(paste(input$dataset_choice3)) +
      
      
      scale_x_discrete(labels=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                "Player G", "Player H", "Player I", "Player J"), na.translate = FALSE) +
      
      theme_classic() +
      ggtitle(paste(input$dataset_choice3,"Distribution by Practice")) +
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.text.x = element_text(face="bold", color="black", size=14),
            axis.text.y = element_text(face="bold", color="black",  size=14),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face="bold", color="black", size=14))})
  
  
  #Heart Zone Bar Chart
  output$plot4 <- renderPlot(drop_na(rv$data_set4) %>%{ggplot(data=remove_missing(rv$data_set4, na.rm = TRUE, vars = 'Key'), aes(x = factor(Zone, level=c("Time Under Zones", "Recovery Training","Aerobic Zone 1",
                                                                                                                                                          "Aerobic Zone 2","Anaerobic Zone","High Intensity Training")), y=value, fill=Key)) +
      ggtitle(paste(input$dataset_choice4,"Practice Activity by Heart Rate Zone"))+
      xlab("Heart Rate Zones") +
      scale_x_discrete(na.translate = FALSE) +
      geom_bar(stat="identity", position=position_dodge(), na.rm =TRUE)+
      geom_text(aes(label=sprintf("%0.2f", round(value, digits = 2))),vjust=-0.3, color="black",
                position = position_dodge(0.9), size=8)+
      scale_fill_brewer(palette="Paired")+
      
      scale_y_continuous("Percentage of Practice Time", labels = label_percent(), limits=c(0,.33))+
      
      
      theme_classic()+
      theme(plot.title = element_text(size=28,hjust = 0.5),
            text=element_text(size=18),
            legend.title=element_text(size=24),
            legend.text=element_text(size=18)
            
      )
    
  })
  
  #7 day radar chart
  output$plot5 <- renderPlot({
    radarchart(rv$data_set5, axistype = 1,
               # Customize the polygon
               pcol = colors_in, pfcol = scales::alpha(colors_in, 0.3), plwd = 8 , plty = 1,
               # Customize the grid
               cglcol = "grey", cglty = 1, cglwd = 0.8,
               # Customize the axis
               axislabcol = "black", vlcex = 1.5,
               # Variable labels
               caxislabels = c(0,25,50,75,100), calcex = 1.5)
    
    
    mtext(side = 3, line = 1.5, at = 0, cex = 1.75, text = paste(input$dataset_choice5,"Exertion Last 7 Days"), font = 2)
    
    legend(x=1.1, y=.5, legend = c("Season to Date", "Last 7 Days"), bty = "n", pch=16 , col=colors_in, text.font = 12, text.col = "black", cex=2, pt.cex=4)
    
    
  })
  
  #Exertion Practice vs Game Radar Chart
  output$plot6 <- renderPlot({
    radarchart(rv$data_set6, axistype = 1,
               # Customize the polygon
               pcol = colors_in, pfcol = scales::alpha(colors_in, 0.3), plwd = 5, plty = 1,
               # Customize the grid
               cglcol = "grey", cglty = 1, cglwd = 0.8,
               # Customize the axis
               axislabcol = "black", vlcex = 1.5,
               # Variable labels
               caxislabels = c(0,25,50,75,100), calcex = 1.5)
    
    
    mtext(side = 3, line = 1.5, at = 0, cex = 1.75, text = paste(input$dataset_choice6,"Exertion in Practice vs. Game"), font = 2)
    
    legend(x=1.5, y=.5, legend = c("Practice", "Game"), bty = "n", pch=16 , col=colors_in, text.font = 12, text.col = "black", cex=2, pt.cex=4)
    
    
  })
  
  
  
  
  
  #Download button for TabPanel 1 - Raincloud Plots  
  
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice3," Raincloud Plot.png")
      
    },
    content = function(file)  {
      png(file, width = 1100, height = 800)
      plot({ggplot(data=remove_missing(rv$data_set3,na.rm = TRUE, vars = 'Variable'),
                   aes(x = factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                   "Player G", "Player H", "Player I", "Player J")), y = Value, colour = Athlete, fill = factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                                                                                                                                                 "Player G", "Player H", "Player I", "Player J")))) +
          #dot size
          geom_half_point(side = "l", size = 0.8,
                          colour = "black") +
          # lines of the box plot
          geom_half_boxplot(side = "l", width = 0.5,
                            alpha = 0.3, nudge = 0.05,
                            colour = "black") +
          geom_half_violin(aes(colour = Athlete),
                           side = "r",
                           colour = "black",
                           scale =3.5) +
          guides(fill = "none", color = "none") +
          scale_fill_viridis(discrete=TRUE) +
          ylab(paste(input$dataset_choice3)) +
          
          scale_x_discrete(labels=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                    "Player G", "Player H", "Player I", "Player J")) +
          theme_classic() +
          ggtitle(paste(input$dataset_choice3,"Distribution by Practice")) +
          theme(plot.title = element_text(hjust = 0.5, size=20),
                axis.text.x = element_text(face="bold", color="black", size=14),
                axis.text.y = element_text(face="bold", color="black",  size=14),
                axis.title.x = element_blank(),
                axis.title.y = element_text(face="bold", color="black", size=14))})
      dev.off()},
    contentType = 'image/png'
  )
  
  #Bar chart download
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice4," Practice Activity by Heart Rate Zone.png")
      
    },
    content = function(file)  {
      png(file, width = 1100, height = 800)
      plot({ggplot(data=remove_missing(rv$data_set4, na.rm = TRUE, vars = 'Key'), aes(x = factor(Zone, level=c("Time Under Zones", "Recovery Training","Aerobic Zone 1",
                                                                                                               "Aerobic Zone 2","Anaerobic Zone","High Intensity Training")), y=value, fill=Key)) +
          ggtitle(paste(input$dataset_choice4,"Practice Activity by Heart Rate Zone"))+
          xlab("Heart Rate Zones") +
          scale_x_discrete(na.translate = FALSE) +
          geom_bar(stat="identity", position=position_dodge(), na.rm =TRUE)+
          geom_text(aes(label=sprintf("%0.2f", round(value, digits = 2))),vjust=-0.3, color="black",
                    position = position_dodge(0.9), size=8)+
          scale_fill_brewer(palette="Paired")+
          
          scale_y_continuous("Percentage of Practice Time", labels = label_percent(), limits=c(0,.31))+
          
          
          theme_classic()+
          theme(plot.title = element_text(size=28,hjust = 0.5),
                text=element_text(size=18),
                legend.title=element_text(size=24),
                legend.text=element_text(size=18)
                
          )
      })
      dev.off()},
    contentType = 'image/png'
  )
  
  #violin download
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice2," Violin Plot.png")
      
    },
    content = function(file)  {
      png(file, width = 900, height = 700)
      plot({ggplot(data=remove_missing(rv$data_set2,na.rm = TRUE, vars = 'Variable'),
                   aes(x=Variable, y=Value))+
          geom_boxplot(width = 0.1)+
          geom_violin(alpha = 0.4, fill = 'skyblue1')+
          theme_test(base_size = 18)+
          facet_wrap(~factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                              "Player G", "Player H", "Player I", "Player J"))) })
      dev.off()},
    contentType = 'image/png'
  )
  
  output$image3 <- renderImage({
    list(src = paste0("www/",input$dataset_choice3,"sample.png"),
         width = 1219,
         height = 348)
    # alt = "table")
  }, deleteFile = FALSE)
  
  
  output$image4 <- renderImage({
    list(src = paste0("www/",input$dataset_choice4,".png"),
         width = 1219,
         height = 216)
    # alt = "table")
  }, deleteFile = FALSE)
  
  # Box Plot Downloader
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice1," Box Plot.png")
      
    },
    content = function(file)  {
      png(file, width = 900, height = 700)
      plot({ggplot(data=remove_missing(rv$data_set1,na.rm = TRUE, vars = 'Variable'),
                   aes(x=Variable, y=Value))+
          geom_boxplot()+
          theme_test(base_size = 18) +
          facet_wrap(~factor(Athlete, level=c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F",
                                              "Player G", "Player H", "Player I", "Player J"))) })
      dev.off()},
    contentType = 'image/png'
  )
  
  
  # Download for Practice vs. Game
  select_plot2 = function() {radarchart(rv$data_set6, axistype = 1,
                                        # Customize the polygon
                                        pcol = colors_in, pfcol = scales::alpha(colors_in, 0.3), plwd = 5, plty = 1,
                                        # Customize the grid
                                        cglcol = "grey", cglty = 1, cglwd = 0.8,
                                        # Customize the axis
                                        axislabcol = "black", vlcex = 1.5,
                                        # Variable labels
                                        caxislabels = c(0,25,50,75,100), calcex = 1.5)
    
    
    mtext(side = 3, line = 1.5, at = 0, cex = 1.75, text = paste(input$dataset_choice6,"Exertion in Practice vs. Game"), font = 2)
    
    legend(x=1.5, y=.5, legend = c("Practice", "Game"), bty = "n", pch=16 , col=colors_in, text.font = 12, text.col = "black", cex=2, pt.cex=4)}
  
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice6," Exertion in Practice vs. Game.png")
      
    },
    content = function(file)  {
      png(file, width =1100, height = 700)
      select_plot2()
      dev.off()},
    contentType = 'image/png'
  )
  
  
  
  # Download for Exertion Last 7 Days
  select_plot3 = function() {radarchart(rv$data_set5, axistype = 1,
                                        # Customize the polygon
                                        pcol = colors_in, pfcol = scales::alpha(colors_in, 0.3), plwd = 8 , plty = 1,
                                        # Customize the grid
                                        cglcol = "grey", cglty = 1, cglwd = 0.8,
                                        # Customize the axis
                                        axislabcol = "black", vlcex = 1.5,
                                        # Variable labels
                                        caxislabels = c(0,25,50,75,100), calcex = 1.5)
    
    
    mtext(side = 3, line = 1.5, at = 0, cex = 1.75, text = paste(input$dataset_choice5,"Exertion Last 7 Days"), font = 2)
    
    legend(x=1.1, y=.5, legend = c("Season to Date", "Last 7 Days"), bty = "n", pch=16 , col=colors_in, text.font = 12, text.col = "black", cex=2, pt.cex=4)}
  
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice5," Exertion in Last 7 Days.png")
      
    },
    content = function(file)  {
      png(file, width =1100, height = 700)
      select_plot3()
      dev.off()},
    contentType = 'image/png'
  )
  
}
shinyApp(ui, server)

