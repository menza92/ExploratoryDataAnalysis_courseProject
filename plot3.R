library(data.table)
library(ggplot2)

# Question 3
#==============================================================================
# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in 
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

# Load the relevant file.
dat.main <- if(file.exists("summarySCC_PM25.rds")) {
    readRDS("summarySCC_PM25.rds") } else {
    readRDS(unzip("exdata_data_NEI_data.zip",files="summarySCC_PM25.rds")) 
    }

dat.main <- data.table(dat.main)

# init a factor variable to group by year and type
dat.main[,key:= paste(year,type,sep="-")]

# Data set to be plotted.
dat <- subset(dat.main,fips=="24510")[,sum(Emissions),by=key]
setnames(dat,c("key","V1"),c("key","Combined_Em"))

# A little table to lookup the year and type from the combined key.
keyTable <- data.table(
              expand.grid(
                unique(dat.main$year),
                unique(dat.main$type)
))
setnames(keyTable,c("Var1","Var2"),c("year","type"))
# Generating the key factor by which to match.
keyTable[,keyhole:= paste(year,type,sep="-") ]
setkey(keyTable,keyhole)
# Adding back the variable columns
dat[,year:= keyTable[key,year] ]
dat[,type:= keyTable[key,type] ]

png(filename="plot3.png",width=480,height=480,units="px")

# Plot using color to differentiate the type.
qplot(year,Combined_Em,data=dat,color=type,geom="line",
      main="PM2.5 Emissions in Baltimore by Year",
      xlab="Year",ylab="Emissions (tonnes)")

dev.off()

# Onroad and nonroad sources decreased somewhat consistently. Point source
# emissions seem to be the only type to have a net increase over the years, as
# well as an increasing trend in general.