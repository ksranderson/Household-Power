#Exploratory Data Analysis Week1 Project
#
#1 household's electric power consumption over 4 years
#data points collected literally every minute

#download dataset, 19.7MB, works with new link(from current webpage button) and proper working directory
download.file("https://archive.ics.uci.edu/static/public/235/individual+household+electric+power+consumption.zip", "./temp")

#unzip to file in working directory
unzip("./temp") #does unzip
#maximum source editor file size is 5MB to display
#size of the dataset = 2,075,259 rows and 9 columns

#read dataset, size matches
dataset <- read.table("./household_power_consumption.txt", header = TRUE, sep = ';') #read in dataset 'household_power_consumption.txt'

#filter or more accurately subset by dates: 2007-02-01 and 2007-02-02
household_data <- subset(dataset, Date %in% c("1/2/2007", "2/2/2007"))

#get 'Global_active_power' variable into numeric variable type and save the dataframe change
household_data <- transform(household_data, Global_active_power = as.numeric(Global_active_power))

#check the data for 2 dates only European dates for Feb. 1-2, 2007(dd/mm/yyyy)
library(dplyr) #load package
#select(household_data, Date) %>% unique #verified only 2 dates


#PLOT2

#Get dataframe correct for plotting

#Try strptime(), does not work
#Try library 'chron', get variable class changed from 'chr' to 'times'
#times is a value 0.0 - 1.0, NOT HOURS:MINUTES:SECONDS -> FAIL
#library(chron)
#household_data$Time <- times(household_data$Time)

#load library for mutate and pipe function
library(dplyr)

#Date changed to Date class successfully
household_data$Date <- as.Date(household_data$Date, format = "%d/%m/%Y")

#mutate new variable DateTime to get only 1 line
household_data <- household_data %>% mutate(DateTime = paste(Date, Time))

#make DateTime variable POSIXlt class with PST time zone for summer
household_data$DateTime <- strptime(household_data$DateTime, "%Y-%m-%d %H:%M:%S", tz = "PST")

#clear plot area not needed
#dev.off()

#set filename for plot, size, and output to png
png(filename = "~/plot2.png", width = 480, height = 480)

#Plot2 
#check for NA, both variables have zero NA values ("integer(0)")
#which(is.na(household_data$Global_active_power), arr.ind = TRUE)

#make plot
with(household_data, plot(DateTime, Global_active_power, type = 'l', xlab = "", ylab = "Global Active Power(kilowatts)"))

#Label for "Plot2"
par(xpd=NA)
#plot(rnorm(100))
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
txt <- "Plot2"
x <- x[1] + strwidth(txt, cex=3) / 2
y <- y[2] - strheight(txt, cex=3) / 2
text(x, y, txt, cex=1.5)

#turn off output to png so plot is sent to file
dev.off()
