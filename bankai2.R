install.packages("shiny")
library(shiny)

recomm <- function(brand,storage, price, ram, i){
  phones <- read.csv('mobiles.csv')
  phones
  phones <- phones[complete.cases(phones), ]
  sum(!complete.cases(phones))
  phones$Color <- gsub("([A-Za-z]+).*", "\\1", phones$Color)
  contains_MB <- grepl("MB", phones$Memory)
  phones <- phones[!contains_MB, ]
  contains_MB <- grepl("MB", phones$Storage)
  phones <- phones[!contains_MB, ]
  phones$Memory <- as.integer(gsub("([A-Za-z]+).*", "\\", phones$Memory))
  phones$Memory
  phones$Storage <- as.integer(gsub("([A-Za-z]+).*", "\\", phones$Storage))
  typeof(phones$Storage)
  phones$Storage
  phones <- phones[complete.cases(phones), ]
  
  phones$Brand <- as.factor(phones$Brand)
  means <- aggregate(Rating ~ Brand, data = phones, mean)
  means
  means2 <- aggregate(Selling.Price ~ Brand, data = phones, mean)
  br <- levels(phones$Brand)
  barplot(means$Rating, names.arg = br, xlab = "Brand", ylab = "Rating",ylim=c(0,5), main = "Brand Ratings",las=2,cex.names=0.55)
  plot(phones$Rating, phones$Selling.Price, xlab = "Rating", ylab = "Selling Price")
  typeof(phones$Brand)
  phones[1,]
  phones$Brand = as.character(phones$Brand)
  phones$Brand
  phoneList <- phones[phones$Brand == brand , ]
  phoneList <- phoneList[phones$Storage >= storage , ]
  phoneList <- phoneList[phones$Memory >= ram , ]
  phoneList <- phoneList[phones$Selling.Price <= price, ]
  #  phones$Brand <- trimws(phones$Brand)
  #phoneList = phoneList[order(phoneList$Rating, decreasing = TRUE, )]
  phoneList <- phoneList[order(-phoneList$Rating, phoneList$Selling.Price), ]
  
    myphones=head(phoneList)
    myphones
    plot(myphones$Selling.Price ,type="o",xlab = "Recommended Laptops", ylab = "Selling Price")
    #axis(side = 1, at = myphones$Selling.Price, labels = c(myphones$Model), tick = FALSE, las = 2)

  
    phoneList <- paste("Recommended Phones : ",'\nName : ',phoneList$Model, 'Price : ',phoneList$Selling.Price, 'Ratings : ',phoneList$Rating,  'RAM : ', phoneList$Memory,'STORAGE : ', phoneList$Storage, " gb", sep="\n")
    #return(phoneList)
    
    if(i==0){
      return(phoneList[[1]])
    }
    else if(i==1){
      tryCatch({
        return(phoneList[[2]])
      },
      error=function(cond) {
        return(" ")
      })
    }
    else if(i==2){
      tryCatch({
        return(phoneList[[2]])
      },
      error=function(cond) {
        return(" ")
      })
    }else{
      return(" ")
    }
  
  
}

calcMean <- function(){
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
  br <- levels(x_clean$brands)
  means <- aggregate(Rating ~ Brand, data = phones, mean)
  return(list("rating"=means$rating, "br"=br))
}

calcMean()

ui <- fluidPage(
  
  # App title ----
  titlePanel("Mobile Recommendation System"),
  
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
      store <- 
        numericInput("store", "Enter Storage", value = 0, min = 0, max = 32),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      mainPanel(
        textOutput("selected_var"),
        textOutput("selected_var2"),textOutput("selected_var3"),
        plotOutput("plot", width = "400px"),
        #  plotOutput("plot2", width = "400px"),
      )
      
    )
  )
)

server <- function(input, output) {
  output$selected_var <- renderText({ 
    paste("We recommend : ", recomm(toupper(input$brand),input$store, input$price, input$ram, 0))
  })
  
  output$selected_var2 <- renderText({ 
    paste("We recommend : ", recomm(toupper(input$brand),input$store, input$price, input$ram, 1))
  })
  
  output$selected_var3 <- renderText({ 
    paste("We recommend : ", recomm(toupper(input$brand),input$store, input$price, input$ram, 2))
  })
  
  output$plot <- renderPlot(plot(phones$Rating, phones$Selling.Price, xlab = "Rating", ylab = "Selling Price"), res = 96)
 }

br

shinyApp(ui = ui, server = server)
