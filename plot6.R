library(data.table)
library(ggplot2)

# Question 6
#==============================================================================
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

# Load the relevant files.
dat.main <- if(file.exists("summarySCC_PM25.rds")) {
          data.table(readRDS("summarySCC_PM25.rds")) } else {
          data.table(
            readRDS(
              unzip("exdata_data_NEI_data.zip",
                    files="summarySCC_PM25.rds")
))}
dat.ref <- if(file.exists("Source_Classification_Code.rds")) {
          data.table(readRDS("Source_Classification_Code.rds")) } else {
          data.table(
            readRDS(
              unzip("exdata_data_NEI_data.zip",
                    files="Source_Classification_Code.rds")
))}
# RegEx expression/function to find the IDs for motor-vehicle sources.
isMotorVehicle <- grepl( "[Mm]obile .+ [Vv]ehicle([s]?)", dat.ref$EI.Sector )
# List of coal combustion source IDs.
mvSCC <- dat.ref$SCC[ isMotorVehicle ]

# Sum total motor-vehicle emissions in baltimore by year.
dat.mvBalt <- dat.main[fips=="24510" & SCC %in% mvSCC ][,sum(Emissions),by=year]
dat.mvLA   <- dat.main[fips=="06037" & SCC %in% mvSCC ][,sum(Emissions),by=year]

setnames(dat.mvBalt,c("year","V1"),c("year","Total_Emissions"))
setnames(dat.mvLA,  c("year","V1"),c("year","Total_Emissions"))

dat.mvBalt[,location := "Baltimore"]
dat.mvLA[  ,location := "LA"]

dat <- rbind(dat.mvBalt,dat.mvLA)
dat[, logEmissions := log10(Total_Emissions)]

png(filename="plot6.png",width=480,height=480,units="px")

# Plotting two side by side line plots.
par(mfrow=c(1, 2), mar=c(4, 4, 1, 1))

qplot(year,Total_Emissions,data=dat,color=location,geom="line",
      main="Motor-Vehicle PM2.5 Emissions by Year",
      xlab="Year",ylab="Emissions (tonnes)")

qplot(year,logEmissions,data=dat,color=location,geom="line",
      main="Motor-Vehicle PM2.5 Emissions by Year (log10)",
      xlab="Year",ylab="log_10( Emissions )")

dev.off()

# So the L.A. has a massive volume of emission at 1999 which increases
# consistently until 2005, where it begins to decrease. Overall, the emission
# in L.A. increased by 170.201 tonnes (4.33%). A base 10 log plot better shows
# the changes with respect to overall magnitude of the emission (ie decrease 
# of 0.69 means emission is halved, increase of 0.69 means emission is
# doubled). So while LA changes are much larger than Baltimore, Baltimore
# experiences a larger change percent-wise, finishing at 25.45% of it’s initial
# emissions relative to LA’s finishing 104.33%