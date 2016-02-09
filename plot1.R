library(data.table)

# Question 1
#==============================================================================
# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Load the relevant file.
dat.main <- if(file.exists("summarySCC_PM25.rds")) {
    readRDS("summarySCC_PM25.rds") } else {
    readRDS(unzip("exdata_data_NEI_data.zip",files="summarySCC_PM25.rds")) 
    }

dat.main <- data.table(dat.main)

# Sum emission entries by year.
dat <- dat.main[,sum(Emissions),by=year]

# Renaming the generic variable as apropriate.
setnames(dat,c("year","V1"),c("year","Total_Emissions"))

png(filename="plot1.png",width=480,height=480,units="px")

with(dat,plot(year,Total_Emissions,type="b",col="blue",
              xlab="Year",ylab="Total Emissions (tonnes)",
              main="PM2.5 Emissions in the U.S. by Year"))

dev.off()

# Yes, a consistent decrease in total emission is observed.