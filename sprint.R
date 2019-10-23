
library(shiny)
library(ggmap)
library(ggplot2)
register_google(key="AIzaSyALp7JBbp_TjgojSnQwphtuQ5BdgsM81lU")

# as.numeric(strptime("10/21/19 11:00:00", "%m/%d/%y %H:%M:%S") - dat$StatusDate[dat$SRRecordID == "5004M00000ZwN7rQAF"])

load("./sample_data.rda")
dist <- dist[c(1,5:2),c(1,5:2)]
dat <- readRDS("./dat.rds")
# bal_map <- get_googlemap(center = c(lon = -76.6, lat = 39.3),
#                          zoom = 13, scale = 2,
#                          maptype ='satellite')
# save(bal_map, file = "map.RData")
load(file = "./map.RData")

Origin <- as.data.frame(matrix(c("5004M00000ZwG4RQAV", "2019-10-16 15:45:22", 39.2980264, -76.5894243, "JHSPH"), nrow = 1))
colnames(Origin) <- c("SRRecordID", "StatusDate", "Latitude", "Longitude", "ID")

dat <- rbind(dat, Origin)
dat$IS.Origin <- c(0,0,0,0,1)

## weight in the greedy algorithm
w0 <- 0.9

# Scoring greedy algorithm
score <- function(location, time, dist, w){
  candi <- names(dist)
  
  res <- numeric()
  for (i in 1:length(candi)) {
    time.dif <- as.numeric(difftime(time, dat$StatusDate[dat$SRRecordID == candi[i]], units="mins"))
    dist.dif <- dist[candi[i]]
    res[i] <- w*time.dif - (1-w)*dist.dif ## Key idea of greedy algorithm 
  }
  names(res) <- candi
  return(res)
}


ui <- fluidPage(
  
  tags$h1("Next work location"),
  
  
  sidebarLayout(
    sidebarPanel(
      ## Here we define that everything starts in JHSPH
      selectInput("location", 
                  "Please select your starting point", 
                  "JHSPH",
                  selected = "JHSPH" ),
      
      textInput("time", 
                label = "Please input the current time (mm/dd/yy HH:MM:SS)",
                value = "10/21/19 11:00:00"),
      
      selectInput("finished", 
                  "Please select the finished location ID", 
                  dat$ID[dat$ID != "JHSPH"],
                  multiple = TRUE),
      
      textOutput("nextwork"),
      width = 5
    ),
    mainPanel(
      plotOutput("plot"),
      width = 7
    )
  )
  
)

server <- function(input, output){
  rv = reactiveValues()
  
  observe({rv$time = strptime(input$time, "%m/%d/%y %H:%M:%S")})
  observe({rv$finished = c("JHSPH", input$finished)})
  observe({rv$location = input$location})
  
  
  observe({
    
    dat.new <- dat[dat$StatusDate < rv$time, ]
    if(is.null(rv$finished) == FALSE){
      dat.new <- dat.new[!(dat.new$ID %in% rv$finished), ]
      rv$location <- switch (rv$finished[length(rv$finished)],
                            "Location 1" = "5004M00000ZwGHFQA3",
                            "Location 2" = "5004M00000ZwJ8HQAV",
                            "Location 3" = "5004M00000ZwN7rQAF",
                            "Location 4" = "5004M00000ZwOMTQA3",
                            "JHSPH" = "JHSPH"
      )
    }
    
    # Input the "finished location" ID
    tmp <- which(colnames(dist)==rv$location)
    dist.new <- dist[-c(1,tmp), tmp]
    dist.new <- dist.new[names(dist.new) %in% dat.new$SRRecordID]
    
    score.new <- score(location=rv$location, time=rv$time, dist=dist.new, w=w0)
    
    output$nextwork <- renderText({
      paste("The recommended next littering site:" , switch (names(score.new)[which(score.new == max(score.new))][1],
              "5004M00000ZwGHFQA3" = "Location 1",
              "5004M00000ZwJ8HQAV" = "Location 2",
              "5004M00000ZwN7rQAF" = "Location 3",
              "5004M00000ZwOMTQA3" = "Location 4"
      )
      )
      
      }, quoted = TRUE)
    
    output$plot = renderPlot({
      dat.now <- dat[dat$ID == rv$finished[length(rv$finished)], ]
      dat.now$IS.Origin <- 1
      dat.new.plot <- as.data.frame(rbind(dat.new, dat.now))
      ## Recommended
      dat.new.plot$IS.Origin[dat.new.plot$SRRecordID == names(score.new)[which(score.new == max(score.new))][1]] <- 2
      
      
      ggmap(bal_map) +
        geom_point(aes(x=as.numeric(Longitude), y=as.numeric(Latitude), color = as.character(IS.Origin), shape = as.character(IS.Origin)),data = dat.new.plot, size = 4) +
        scale_color_manual(values = c("0" = "yellow", "2" = "red", "1" = "green")) +
        scale_shape_manual(values = c("0" = 16, "2" = 8, "1" = 17)) +
        labs(x = "Longitude", y = "Latitude") +
        geom_text(aes(x=as.numeric(Longitude), y=as.numeric(Latitude), label=ID), data = dat.new.plot, size = 4, col = "cyan", hjust = 1, vjust = 2) +
        theme(legend.position = "none") +
        labs(title = "Green: Current Location \nRed: Recommended Littering Site\nYellow: Other Candidate Littering Sites")
      })
    
  })
  
}

shinyApp(ui=ui, server=server)

