# METADATA ====
# Description: Leaply data preparation and cleaning batch script
# Created: 2018-03-09 (Reid Falconer)
# Updated: 2018-04-11 (Neil Rankin)
# Reviewed: 

# NOTES: Script to take opportunity downloads from dropbox and prepare the data
# for analysis.
# Different parts of the preparation have been modularised.

# Functions ===
getdata <- function(file){
    #returns the file specified with processed dates.
    
    #file si the pathof the file to be read
    mydata <- read.csv(file, header = T)
    
    # create new object
    mydata$date1 <- mydata$Date 
    
    # This overwrites the variable date1 and enters the first 7 digits of the string-variable date.
    mydata$date1 <- substr(mydata$date, start = 1, stop = 7)
    
    # now we can convert it to the Date class, but first setting the locale
    #lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

    mydata$date2 <- as.Date(mydata$date1, "%b%d%y")
   # mydata$date2 <- ts(mydata$date2)
    
    mydata
}

createVariables <- function(mydata) {
    #creates variables of interest for regression
    
    price <- mydata$Sales_USD/mydata$Sales_U #first 16 values are nonsensical
    sales_u <-  mydata$Sales_U #first 16 values are nonsensical
    ln_sales_u <- log(mydata$Sales_U) #first 16 values are nonsensical
    ln_price <- log(price)#first 16 values are nonsensical
    
    df <- data.frame(price = price, sales_u = sales_u, ln_sales_u = ln_sales_u, ln_price = ln_price, date2 = mydata$date2) 
}

runRegression <- function(vars.df) {
    #runs regression on variables of interest
    lm(vars.df$ln_sales_u ~ vars.df$ln_price)
}

merged.df <- function(data1, data2){
    #removes ln_sales from data1 and merges with data2
    merge(data1[, -3], data2, by = "date2")
}

##############################################################################
# 1. Hellman at Jewel

#get raw data and create new variables
hellman_at_jewel <- createVariables(getdata("../data/JWL_HL32.csv"))

#time series plots
plot(hellman_at_jewel$date2, hellman_at_jewel$price, col = "blue", xy.labels = F, type = "l")
plot(hellman_at_jewel$date2, hellman_at_jewel$sales_u, col = "red", xy.labels = F, type = "l")

#scatter plot
plot(hellman_at_jewel$price, hellman_at_jewel$sales_u)

#regression
summary(runRegression(hellman_at_jewel))
##############################################################################3

# 2. Kraft at Jewel
kraft_at_jewel <- createVariables(getdata("../data/JWL_KR32.csv"))

#time series plots
plot(kraft_at_jewel$date2, kraft_at_jewel$price, col = "blue", xy.labels = F, type = "l")
plot(kraft_at_jewel$date2, kraft_at_jewel$sales_u, col = "red", xy.labels = F, type = "l")

#scatter plot
plot(kraft_at_jewel$price, kraft_at_jewel$sales_u)

#regression
summary(runRegression(kraft_at_jewel))
##############################################################################

#3. Hellman at Central
hellman_at_central <- createVariables(getdata("../data/KC_HL32.csv"))

#time series plots
plot(hellman_at_central$date2, hellman_at_central$price, col = "blue", xy.labels = F, type = "l")
plot(hellman_at_central$date2, hellman_at_central$sales_u, col = "red", xy.labels = F, type = "l")
# 
# ggplot(hellman_at_central, aes(x=date2)) +
#     geom_line(aes(y=sales_u, col="Sales")) +
#   #  ylim(-8.5, 5.5) +
#     labs(#title="ANN Regression Forecast",
#   #       subtitle="Forecasts from the ANN model vs actual GDP data",
#     caption="Source: Own Compilation", y="Sales", x = "Date") +  # title and caption
#     scale_color_manual(name="", values = c("Sales"="turquoise4"))  # line color
#   #  theme(legend.key = element_rect(fill = "white", colour = "black"))
# 
# ggplot(hellman_at_central, aes(x=date2)) +
#     geom_line(aes(y=price, col="Price")) +
#     #  ylim(-8.5, 5.5) +
#     labs(#title="ANN Regression Forecast",
#         #       subtitle="Forecasts from the ANN model vs actual GDP data",
#     caption="Source: Own Compilation", y="Price", x = "Date") +  # title and caption
#     scale_color_manual(name="", values = c("Price"="coral"))  # line color
# #  theme(legend.key = element_rect(fill = "white", colour = "black"))

#scatter plot
plot(hellman_at_central$price, hellman_at_central$sales_u)

#regression
summary(runRegression(hellman_at_central))
##############################################################################

# 4. Kraft at Central
kraft_at_central <- createVariables(getdata("../data/KC_KR32.csv"))

#time series plots
plot(kraft_at_central$date2, kraft_at_central$price, col = "blue", xy.labels = F, type = "l")
plot(kraft_at_central$date2, kraft_at_central$sales_u, col = "red", xy.labels = F, type = "l")

#scatter plot
plot(kraft_at_central$price, kraft_at_central$sales_u)

#regression
summary(runRegression(kraft_at_central))
#############################################################################

#############################################################################
#Analysis on merged data
hellman_at_jewel <- createVariables(getdata("../data/JWL_HL32.csv"))
kraft_at_jewel <- createVariables(getdata("../data/JWL_KR32.csv"))
hellman_at_central <- createVariables(getdata("../data/KC_HL32.csv"))
kraft_at_central <- createVariables(getdata( "../data/KC_KR32.csv"))

#############################################################################
#Hellman at Jewel with Kraft price
hellman.jewel_kraft.price <- merged.df(kraft_at_jewel, hellman_at_jewel)
#x = kraft_at_jewel, y = hellman_at_jewel
summary(lm(ln_sales_u ~ ln_price.x + ln_price.y, data = hellman.jewel_kraft.price))

#############################################################################
#Hellman at Central with Kraft price
hellman.central_kraft.price <- merged.df(kraft_at_central, hellman_at_central)
#x = kraft_at_central, y = hellman_at_central
summary(lm(ln_sales_u ~ ln_price.x + ln_price.y, data = hellman.central_kraft.price))

#############################################################################
#Kraft at Central with Hellman price
kraft.central_hellman.price <- merged.df(hellman_at_central, kraft_at_central)
#x = hellman_at_central, y = kraft_at_central
summary(lm(ln_sales_u ~ ln_price.x + ln_price.y, data = kraft.central_hellman.price))
    
#############################################################################
#Kraft at Jewel with Hellman price
kraft.jewel_hellman.price <- merged.df(hellman_at_jewel, kraft_at_jewel)
#x = hellman_at_jewel, y = kraft_at_jewel
summary(lm(ln_sales_u ~ ln_price.x + ln_price.y, data = kraft.jewel_hellman.price))


