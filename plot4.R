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


#Date changed to Date class successfully
household_data$Date <- as.Date(household_data$Date, format = "%d/%m/%Y")

#mutate new variable DateTime to get only 1 line
household_data <- household_data %>% mutate(DateTime = paste(Date, Time))

#make DateTime variable POSIXlt class with PST time zone for summer
household_data$DateTime <- strptime(household_data$DateTime, "%Y-%m-%d %H:%M:%S", tz = "PST")

#PLOT 4

#set filename for plot, size, and output to png
png(filename = "~/plot4.png", width = 480, height = 480)

#define 4 places for plots going left to right then down
par(mfrow = c(2,2)) #2 x 2 plots on screen, plots left to right then down 1 row

#plot the 4 plots
#1
with(household_data, plot(DateTime, Global_active_power, type = 'l', xlab = "", ylab = "Global Active Power(kilowatts)"))

#Label for "Plot4", will it work, not sure
par(xpd=NA)
#plot(rnorm(100))
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
txt <- "Plot4"
x <- x[1] + strwidth(txt, cex=3) / 2
y <- y[2] - strheight(txt, cex=3) / 2
text(x, y, txt, cex=1.5)

#2
#Voltage plot
with(household_data, plot(DateTime, Voltage, type = 'l', xlab = "datetime", ylab = "Voltage"))
#3
with(household_data, plot(DateTime, Sub_metering_1, type = 'l', col= "black", ylab="Energy sub metering", xlab = "", lty=1))
with(household_data, lines(DateTime, Sub_metering_2, col= "red", lty=1))
with(household_data, lines(DateTime, Sub_metering_3, col= "blue", lty=1))
#legend
legend("topright", lty = c(1,1,1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) #legend
#4
#Global Reactive Power plot
with(household_data, plot(DateTime, Global_reactive_power, type = 'l', xlab = "datetime"))

#turn off output to png so plot is sent to file
dev.off()
