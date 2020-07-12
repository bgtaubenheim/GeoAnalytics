library(dplyr)
library(gstat)
library(ggplot2)
library(maps)
library(tidyr)
library(spdep)
library(colorRamps)


str(ByState)

dataobj <- as.data.frame(ByState)
str(dataobj)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

dataobj$region <- trim(dataobj$region)

par(mfrow=c(1,1))
county <- map('county')
conty <- map('state')

all_counties <- as.data.frame(map_data("county"))
head(all_counties)

all_states <- as.data.frame(map_data("state"))
head(all_states)

all_states$region <- trim(all_states$region)

total <- merge(x=all_counties, y=dataobj, by.x=c("region"), by.y=c("region"), all = FALSE, no.dups = TRUE)
total <- na.omit(total)
head(total)

###################
#   Section 1     #
###################

# median household income

p <- ggplot()
p <- p + geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$`Median household income`),colour="white")+ scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Household Income (Dollars)" 
                             ,title = "Median Household Income", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

summary(dataobj$`Median household income`)
hist(dataobj$`Median household income`, main = "Median Household Income Histogram", xlab = "Income in $")
boxplot(dataobj$`Median household income`, main = "Median Household Income Boxplot")
sd(dataobj$`Median household income`)

# poverty
p <- ggplot()
p <- p + geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$`Poverty rates`),colour="white")+ scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Poverty rates (%)" 
                             ,title = "Poverty rates", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

summary(dataobj$`Poverty rates`)
hist(dataobj$`Poverty rates`, main = "Poverty Rates Histogram", xlab = "Income in $")
boxplot(dataobj$`Poverty rates`, main = "Poverty Rates Boxplot")
sd(dataobj$`Poverty rates`)

# education

selectregions <- dataobj %>%
  filter(dataobj$region %in% c("south dakota", "minnesota", "north dakota", "nebraska", "california"))

dat1 <- data.frame(selectregions$`Less than high school diploma`,
                   selectregions$`High school graduate (includes equivalency)`,
                   selectregions$`Some college or associate's degree`,
                   selectregions$`Bachelor's degree`,
                   selectregions$`Graduate or professional degree`,
                   region = selectregions$region)
dat1_long <- dat1 %>%
  gather("Stat","Value", -region)

ggplot(dat1_long, aes(x = region, y = Value, fill = Stat)) +
  geom_col(position = "dodge")

p <- ggplot()
p <- p + geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$`Bachelor's degree or higher`),colour="white")+ scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Bachelor's degree or higher" 
                             ,title = "Bachelor's degree or higher", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

summary(dataobj$`Bachelor's degree or higher`)
hist(dataobj$`Bachelor's degree or higher`, main = "Bachelor's degree or higher Histogram", xlab = "Bachelor's degree or higher")
boxplot(dataobj$`Bachelor's degree or higher`, main = "Bachelor's degree or higher Boxplot")
sd(dataobj$`Bachelor's degree or higher`)

# age

p <- ggplot()
p <- p + geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$`Median age`),colour="white")+ scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Median Age" 
                             ,title = "Median Age", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

summary(dataobj$`Median age`)
hist(dataobj$`Median age`, main = "Median Age", xlab = "Age in years")
boxplot(dataobj$`Median age`, main = "Median Age")
sd(dataobj$`Median age`)

# health insurance

dat1 <- data.frame(selectregions$`Private health insurance`,
                   selectregions$`Public health insurance`,
                   selectregions$`No health insurance`,
                   region = selectregions$region)
dat1_long <- dat1 %>%
  gather("Stat","Value", -region)

ggplot(dat1_long, aes(x = region, y = Value, fill = Stat)) +
  geom_col(position = "dodge")

p <- ggplot()
p <- p + geom_polygon(data=total, aes(x=long, y=lat, group = group, fill=total$`No health insurance`),colour="white")+ scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "No health insurance" 
                             ,title = "Individuals with no health insurance", x="", y="")

P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +geom_polygon(data = all_states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

summary(dataobj$`No health insurance`)
hist(dataobj$`No health insurance`, main = "No health insurance Histogram", xlab = "No health insurance")
boxplot(dataobj$`No health insurance`, main = "No health insurance Boxplot")
sd(dataobj$`No health insurance`)


#######################
#   Section 2         #
#######################

# Use spatial location determined by distance in X and Y direction
data.sp <- total
data.sp$x <- data.sp$long
data.sp$y <- data.sp$lat
coordinates(data.sp) <- c("x", "y")
head(data.sp)

# Using k nearest neighbors
nlistk <- knn2nb(knearneigh(data.sp, k = 4))
W.kW <- nb2listw(nlistk, style = "B")

# Median income
resI <- localmoran(data.sp$`Median household income`,W.kW,alternative="two.sided")
hist(resI[,1],main="Local Moran's I for Median Income")
LMII.NN <- resI[,1]  # Get Local Moran's I value for each state
summary(LMII.NN)
LMII.NNPV <- resI[,5]  # Get the p-value for each state


# Median Age
resI <- localmoran(data.sp$`Median age`,W.kW,alternative="two.sided")
hist(resI[,1],main="Local Moran's I for Median Age")
LMIA.NN <- resI[,1]  # Get Local Moran's I value for each state
summary(LMIA.NN)
LMIA.NNPV <- resI[,5]  # Get the p-value for state

# Median Age
resI <- localmoran(data.sp$`No health insurance`,W.kW,alternative="two.sided")
hist(resI[,1],main="Local Moran's I for No Health Insurance")
LMIH.NN <- resI[,1]  # Get Local Moran's I value for each state
summary(LMIH.NN)
LMIH.NNPV <- resI[,5]  # Get the p-value for state

# Add these results to original data object and create new spatial object
data.sp2 <- cbind(total[,1:6], LMII.NN,LMII.NNPV,LMIA.NN,LMIA.NNPV, LMIH.NN, LMIH.NNPV)
head(data.sp2)

data.sp2$x2 <- data.sp$long
data.sp2$y2 <- data.sp$lat
coordinates(data.sp2) <- c("x2", "y2")
spplot(data.sp2,"LMII.NN",col.regions=blue2red(20),
       main="Local Moran's I for Median Income")
spplot(data.sp2,"LMII.NNPV",col.regions=blue2red(20),
       main="Local Moran's I P-Values for Median Income")
spplot(data.sp2,"LMIA.NN",col.regions=blue2red(20),
       main="Local Moran's I for Median Age ")
spplot(data.sp2,"LMIA.NNPV",col.regions=blue2red(20),
       main="Local Moran's I P-Values for Median Age")
spplot(data.sp2,"LMIH.NN",col.regions=blue2red(20),
       main="Local Moran's I for No Health Insurance")
spplot(data.sp2,"LMIH.NNPV",col.regions=blue2red(20),
       main="Local Moran's I P-Values for No Health Insurance")
