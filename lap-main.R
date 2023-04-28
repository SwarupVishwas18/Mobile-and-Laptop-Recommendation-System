install.packages("shiny")
library(shiny)

recomm <- function(brand, price, ram, i){
  lap <- read.csv('laptops.csv')
  head(lap)
  lap <- lap[complete.cases(lap$rating), ]
  lap$brands <- gsub("([A-Za-z]+).*", "\\1", lap$name)
  lap$brands <- toupper(lap$brands)
  lap$brands
  lap$ram <- as.integer(gsub("([A-Za-z]+).*", "\\", lap$ram))
  lap$ram
  lap$brands <- as.factor(lap$brands)
  typeof(lap$brands)
  br <- levels(lap$brands)
  means <- aggregate(rating ~ brands, data = lap, mean)
  means2 <- aggregate(price.in.Rs.. ~ brands, data = lap, mean)
  
  lap$brands = as.character(lap$brands)
  #png(file="./rating_price.png",width=600, height=350)
  #plot(lap$rating , lap$price.in.Rs.., xlab = "Rating", ylab = "Price")
  #dev.off()
  #png(file="./brandNrating.png",width=600, height=350)
  #barplot(means$Rating, names.arg = br, xlab = "Brand", ylab = "Rating",ylim=c(0,5), main = "Brand Ratings",las=2,cex.names=0.55)
  #dev.off()
  lapList <- lap[lap$brands == brand , ]
  lapList <- lapList[lapList$price.in.Rs.. <= price , ]
  lapList <- lapList[lapList$ram >= ram , ]
  lapList <- lapList[order(-lapList$rating, lapList$price.in.Rs..), ]
  
  lapList <- paste("Recommended Laptops : ",'\nName : ',lapList$name, 'Price : ',lapList$price.in.Rs.., 'Ratings : ',lapList$rating,  'RAM : ', lapList$ram, sep="\n")
  if(i==0){
    return(lapList[[1]])
  }
  else if(i==1){
    tryCatch({
      return(lapList[[2]])
    },
    error=function(cond) {
      return(" ")
    })
  }
  else if(i==2){
    tryCatch({
      return(lapList[[2]])
    },
    error=function(cond) {
      return(" ")
    })
  }else{
    return(" ")
  }
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Laptop Recommendation System"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      brand <- 
        textInput("brand", 
                  label = "Brand", 
                  value = "", 
                  width = "100%",
                  placeholder = "Enter Brand")
      ,
      price <- 
        numericInput("price", "Enter Price", value = 0, min = 0, max = 400000),
      ram <- 
        numericInput("ram", "Enter RAM", value = 0, min = 0, max = 32),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Result", 
                               textOutput("selected_var"),
                             
                             textOutput("selected_var3"),
                             textOutput("selected_var2"))
                             
                             ,
                    tabPanel("Bar Graph", plotOutput("plot", width = "400px")),
                    tabPanel("Line Chart", plotOutput("plot2", width = "400px")),
                    tabPanel("ScatterPlot", plotOutput("plot3", width = "400px"))
                    
        )
      )
      
    )
  )
)

server <- function(input, output) {
  output$selected_var <- renderText({ 
    paste(recomm(toupper(input$brand), input$price, input$ram, 0))
  })
  
  output$selected_var2 <- renderText({ 
    paste(recomm(toupper(input$brand), input$price, input$ram, 1))
  })
  
  output$selected_var3 <- renderText({ 
    paste(recomm(toupper(input$brand), input$price, input$ram, 2))
  })
  
  output$plot <- renderPlot(barplot(means$Rating,names.arg = br, xlab = "Brand", ylab = "Rating",ylim=c(0,5), main = "Brand and Ratings",las=2,cex.names=0.55, col=rainbow(length(br))), res = 96)
  
  output$plot2 <- renderPlot(plot(means2$Selling.Price,names.arg=br,type="o", xlab = "Brand", col=rainbow(length(means$Rating)) ,ylab = "Price", main = "Brand and Price",las=2,cex.names=0.55), res = 96)
  
  output$plot3 <- renderPlot(plot(means2$Selling.Price,means$Rating, xlab = "Price", ylab = "Rating",col="purple", main = "Price and Rating",las=2,cex.names=0.55), res = 96)
  
  # output$plot2 <- renderPlot(draw_plot_1(input$brand, input$price, input$ram))
}

length(means$Rating)

shinyApp(ui = ui, server = server)
