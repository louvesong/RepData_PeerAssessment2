library(dplyr)
library(ggplot2)
library(gridExtra)
## Load and Process the Raw Data

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#download.file(fileUrl, "peer2.bz2", method="curl")
#storm <- read.csv(bzfile("peer2.bz2"), header=TRUE)

### BGN_DATE, EVTYPE, 
#storm$BGN_DATE <- as.Date(as.character(storm$BGN_DATE), "%m/%d/%Y")

## Q1. Across the United States, which types of events(as indicated in the EVTYPE variable)
## are most harmful with respect to population health?

# make dplyr function
calculateTotalNumber <- function (data, x, y) {
        data %>%
                select(x, y) %>%
                group_by(x) %>%
                summarise(total_cnt=sum(y, na.rm=TRUE)) %>%
                arrange(desc(total_cnt)) %>%
                head(10) %>%
                within(x <- factor(y, levels=y))
}

#total.t1 <- calculateTotalNumber(storm, storm$EVTYPE, storm$FATALITIES)
#total.t2 <- calculateTotalNumber(storm, storm$EVTYPE, storm$INJURIES) 
### fatalities & injuries
total_fatality <- 
        storm %>% 
        select(EVTYPE, FATALITIES) %>% 
        group_by(EVTYPE) %>% 
        summarise(total_cnt=sum(FATALITIES,na.rm=TRUE)) %>%
        arrange(desc(total_cnt)) %>%
        head(10) %>%
        within(EVTYPE <- factor(EVTYPE, levels=EVTYPE))

total_injury <- 
        storm %>%
        select(EVTYPE, INJURIES) %>%
        group_by(EVTYPE) %>%
        summarise(total_cnt=sum(INJURIES, na.rm=TRUE)) %>%
        arrange(desc(total_cnt)) %>%
        head(10) %>%
        within(EVTYPE <- factor(EVTYPE, levels=EVTYPE))

### tables & figures
drawBarplot <- function(d, title_text, x_text, y_text) {
        ggplot(data=d, aes(x=EVTYPE, y=total_cnt)) + 
                geom_bar(stat="identity") +
                theme(axis.text.x=element_text(angle=30, hjust=1)) +
                labs(title=title_text, x=x_text, y=y_text)
}

figure_fatality <- drawBarplot(total_fatality, "the top 10 highest fatalities", "weather type", "total number")
figure_injury <- drawBarplot(total_injury, "the top 10 hightest injuries", "weather type", "total number")
grid.arrange(figure_fatality, figure_injury, ncol=2)

## Q2. Across the United States, which types of events have the greatest economic consequences?
toTenPower <- function(x) {
        if(is.numeric(x)) {
                x <- as.numeric(x)
        }
        else if(grepl("h", x, ignore.case=TRUE)) {
                x <-2
        }
        else if(grepl("k", x, ignore.case=TRUE)) {
                x <-3
        }
        else if(grepl("m", x, ignore.case=TRUE)) {
                x <-6
        }
        else if(grepl("b", x, ignore.case=TRUE)) {
                x <-9
        }
        else if(x=="" | x==" ") {
                x <-1
        }
        else {
                x <- 0
        }
        x
}
calculateNum <- function(num, exp) {
        pow <- toTenPower(exp)
        if(is.na(pow)){
                num <-0
        }
        else {
                if(is.numeric(num)) {
                        num <- num*10^pow
                }
                else {
                        num <-0
                }
        }
                
        num
}
storm_prop <- storm %>% select(EVTYPE, PROPDMG, PROPDMGEXP)
storm_prop$propDmage <- mapply(calculateNum, storm_prop$PROPDMG, storm_prop$PROPDMGEXP)
storm_prop$propDmage <- storm_prop$propDmage/(10^9)
storm_crop <- storm %>% select(EVTYPE, CROPDMG, CROPDMGEXP)
storm_crop$cropDmage <- mapply(calculateNum, storm_crop$CROPDMG, storm_crop$CROPDMGEXP)
storm_crop$cropDmage <- storm_crop$cropDmage/(10^9)
#storm <- storm %>% select(PROPDMG, PROPDMGEXP) %>% mutate(propDmage = calculateNum(PROPDMG, PROPDMGEXP))
#storm <- mutate(storm, cropDmage = calculateNum(CROPDMG, CROPDMGEXP))

total_prop <- 
        storm_prop %>%
        select(EVTYPE, propDmage) %>%
        group_by(EVTYPE) %>%
        summarise(total_cnt=sum(propDmage, na.rm=TRUE)) %>%
        arrange(desc(total_cnt)) %>%
        head(10) %>%
        within(EVTYPE<-factor(EVTYPE, levels=EVTYPE))
total_crop <-
        storm_crop %>%
        select(EVTYPE, cropDmage) %>%
        group_by(EVTYPE) %>%
        summarise(total_cnt=sum(cropDmage, na.rm=TRUE)) %>%
        arrange(desc(total_cnt)) %>%
        head(10) %>%
        within(EVTYPE<-factor(EVTYPE, levels=EVTYPE))

figure_prop <- drawBarplot(total_prop, "the top 10 highest propertities", "weather type", "cost($ billions)")
figure_crop <- drawBarplot(total_crop, "the top 10 higest crop", "weather type", "cost($ billions)")
grid.arrange(figure_prop, figure_crop, ncol=2)
