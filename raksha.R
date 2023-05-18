recLap <- function(brand, price, ram, i) {
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
  means <- aggregate(rating ~ brands, data = x_clean, mean)
  means2 <- aggregate(price.in.Rs.. ~ brands, data = x_clean, mean)
  barplot(means$rating, names.arg = br, xlab = "Brand", ylab = "Rating",ylim=c(0,5), main = "Brand Ratings",las=2,cex.names=0.55)
  lap$brands = as.character(lap$brands)
png(file="./Laptop-Images/rating_price.png",width=600, height=350)
  plot(lap$rating , lap$price.in.Rs.., xlab = "Rating", ylab = "Price")
  dev.off()
  lap$brands
  brand
  lap$brands[663]
  lapList <- lap[lap$brands == brand , ]
  lapList <- lapList[lapList$price.in.Rs.. <= price , ]
  lapList <- lapList[lapList$ram >= ram , ]
  lapList
  lap$brands <- trimws(lap$brands)
  if (!brand %in% lap$brands) {
    if(i==1){
      stop("Brand not found in dataframe.")
    }
    recLap(brand, price, storage, 1)
  }
  return(lapList[1:3,])
}


recPhone <- function(brand, price, storage, i) {
  phones <- read.csv('mobiles.csv')
  length(phones[,1])
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
  means2 <- aggregate(Selling.Price ~ Brand, data = phones, mean)
  br <- levels(phones$Brand)
  png(file="./Mobile-Images/brandRating.png",width=600, height=350)
  barplot(means$Rating, names.arg = br, xlab = "Brand", ylab = "Rating",ylim=c(0,5), main = "Brand Ratings",las=2,cex.names=0.55)
  dev.off()
    png(file="./Mobile-Images/ratingSP.png",width=600, height=350)
   plot(phones$Rating, phones$Selling.Price, xlab = "Rating", ylab = "Selling Price")
dev.off()
   typeof(phones$Brand)
  length(phones[,1])
  phones$Brand = as.character(phones$Brand)
  
  phones$Brand
  phoneList <- phones[phones$Brand == brand & phones$Storage >= storage & phones$Selling.Price <= price , ]
  
  #  phones$Brand <- trimws(phones$Brand)
  #phoneList = phoneList[order(phoneList$Rating, decreasing = TRUE, )]
  phoneList <- phoneList[order(-phoneList$Rating, phoneList$Selling.Price), ]
  if (!brand %in% phones$Brand) {
    if(i==1){
      stop("Brand not found in dataframe.")
    }
    recPhone(brand, price, storage, color, 1)
  }
  else
  {
    myphones=head(phoneList)
    myphones
    plot(myphones$Selling.Price ,type="o",xlab = "Recommended Laptops", ylab = "Selling Price")
    #axis(side = 1, at = myphones$Selling.Price, labels = c(myphones$Model), tick = FALSE, las = 2)
    }
  
  return(phoneList[1:3, ])
}

# Take Input : 

brand =  as.character(toupper(readline(prompt = "Enter brand that you want : ")))
price = as.integer(readline(prompt = "Enter the maximum price you can pay : "))
ram = as.integer(readline(prompt = "Enter the Minimum RAM : "))

recLap(brand, price, ram, 0)

brand =  as.character(toupper(readline(prompt = "Enter brand that you want : ")))
price = as.integer(readline(prompt = "Enter the maximum price you can pay : "))
storage = as.integer(readline(prompt = "Enter the Minimum Storage : "))
# color = as.character(readline(prompt = "Enter the color you want : "))
recPhone(brand, price, storage, 0)
