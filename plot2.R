library(data.table)

# Question 2
#==============================================================================
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make
# a plot answering this question.

# Load the relevant file.
dat.main <- if(file.exists("summarySCC_PM25.rds")) {
    readRDS("summarySCC_PM25.rds") } else {
    readRDS(unzip("exdata_data_NEI_data.zip",files="summarySCC_PM25.rds")) 
    }

dat.main <- data.table(dat.main)

# Sum emission entries by year in Baltimore
dat <- subset(dat.main,fips=="24510")[,sum(Emissions),by=year]

# Renaming the generic variable as apropriate.
setnames(dat,c("year","V1"),c("year","Total_Emissions"))

png(filename="plot2.png",width=480,height=480,units="px")

with(dat,plot(year,Total_Emissions,type="b",col="blue",
              xlab="Year",ylab="Emissions (tonnes)",
              main="PM2.5 Emissions in the Baltimore by Year"))

dev.off()

# Yes, an overall decrease in total emission is observed.
