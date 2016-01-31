# Reproducible Research: Peer Assessment 2
Tobias Ford  
`r Sys.Date()`  



# Health and Economic Impact of Weather Events in the US determined thru Reproducible Data Analysis of NOAA Storm Data 


## Synopsis

The National Oceanic and Atmospheric Administration (NOAA) officially publishes a database called *Storm Data* which documents the occurence and location of weather phenomena which cause loss of life, injury, property damage, and/or disruption to commerce. 

The goal of this data analysis assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events and their health and economic impacts. This report will present and attempt to answer the basic questions and show the R code used to answer the questions. The analysis contained within will consist of tables and figures to help the reader better understand the answers to the basic questions.

## Questions 
### Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population health?

### Across the United States, which types of events have the greatest economic consequences?

## Getting Started : Initial Setup

- Remove all objects from the workspace and load required libraries
- Uncompress the original raw data file
- Pull data into a data.frame using read.csv and converts strings to characters or numerics instead of factors (stringsAsFactors=FALSE)


```r
rm(list=ls())
library(R.utils)
library(plotrix)
library(plyr)
```


```r
dataFile <- 'repdata-data-StormData.csv'

if(!file.exists(dataFile)) {
    bunzip2('repdata-data-StormData.csv.bz2', dataFile, overwrite=T, remove=F)
}

df <- read.csv(dataFile, stringsAsFactors=F)
head(df,1)
```

```
##   STATE__          BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE  EVTYPE BGN_RANGE BGN_AZI
## 1       1 4/18/1950 0:00:00     0130       CST     97     MOBILE    AL TORNADO         0        
##   BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F
## 1                                       0         NA         0                        14   100 3
##   MAG FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES LATITUDE
## 1   0          0       15      25          K       0                                         3040
##   LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1      8812       3051       8806              1
```

## Data Processing - Prepare, clean and better organize the data

- Simplify data by removing unneeded columns for now, helps to speed things up and make analysis easier


```r
dff <- df[, c('EVTYPE', 'INJURIES', 'FATALITIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]
```
- Because there are many redundnant event types, we need to group the redundant event types together. An example displayed here is related to variations in events related to 'Wind'; table of the first 10 of 216 event types with the word wind in them.


```r
table(dff[grep("WIND", dff$EVTYPE), c('EVTYPE')])[1:10]
```

```
## 
##                      TSTM WIND                TSTM WIND (G45)                           WIND 
##                              4                              1                              1 
##              BITTER WIND CHILL BITTER WIND CHILL TEMPERATURES BLIZZARD AND EXTREME WIND CHIL 
##                              1                              3                              2 
##             BLIZZARD/HIGH WIND BLOWING SNOW & EXTREME WIND CH BLOWING SNOW- EXTREME WIND CHI 
##                              1                              2                              1 
## BLOWING SNOW/EXTREME WIND CHIL 
##                              1
```

- Also, as we've discovered below with the improper spelling of FLOOD, the spelling of many of the events is incorrect which causes data to be missed. We'll take this opportunity to clean that up.


```r
table(dff[grep("FLOOOD", dff$EVTYPE), c('EVTYPE')])
```

```
## 
## FLASH FLOOODING 
##               1
```

- Filter obvious redundancies into EV_GROUPs. The order makes a difference.
- In the process, we will create a new grouping of EVTYPE called EV_GROUP.


```r
eg <- c("EV_GROUP")
dff[grepl("WIND|WND|TSTMW", dff$EVTYPE, ignore.case=T), eg] <- 'Wind'
dff[grepl("HEAT|HOT|WARM|HIGH", dff$EVTYPE, ignore.case=T), eg] <- 'Heat'
dff[grepl("COLD|FROST|FREEZE|LOW|COOL", dff$EVTYPE, ignore.case=T), eg] <- 'Cold'
dff[grepl("DROUGHT|DRY", dff$EVTYPE, ignore.case=T), eg] <- 'Drought'
dff[grepl("RAIN|HAIL|PELLETS|BURST|MIX|SLEET|SHOWER|DRIZZLE|PRECIPITATION|PRECIPATATION|SPRAY|WET", dff$EVTYPE, ignore.case=T), eg] <- 'Precipitation'
dff[grepl("AVALANCHE|AVALANCE", dff$EVTYPE, ignore.case=T), eg] <- 'Avalanche'
dff[grepl("SNOW|BLIZZARD", dff$EVTYPE, ignore.case=T), eg] <- 'Snow'
dff[grepl("GLAZE|ICE|ICY", dff$EVTYPE, ignore.case=T), eg] <- 'Winter Effects'
dff[grepl("FLOOD|FLOOOD|FLD|STREAM", dff$EVTYPE, ignore.case=T), eg] <- 'Flood'
dff[grepl("DUST", dff$EVTYPE, ignore.case=T), eg] <- 'Dust'
dff[grepl("TORNADO|TORNDAO|FUNNEL|SPOUT|WHIRLWIND|DEVEL|DEVIL|GUSTNADO", dff$EVTYPE, ignore.case=T), eg] <- 'Tornados'
dff[grepl("FIRE|SMOKE", dff$EVTYPE, ignore.case=T), eg] <- 'Fire'
dff[grepl("VOLCANIC", dff$EVTYPE, ignore.case=T), eg] <- 'Volcanic Activity'
dff[grepl("LIGHTNING|LIGHTING|LIGNTNING", dff$EVTYPE, ignore.case=T), eg] <- 'Lightning'
dff[grepl("STORM|WEATHER", dff$EVTYPE, ignore.case=T), eg] <- 'Weather & Storms'
dff[grepl("HURRICANE|TYPHOON", dff$EVTYPE, ignore.case=T), eg] <- 'Hurricanes'
dff[grepl("DEPRESSION", dff$EVTYPE, ignore.case=T), eg] <- 'Tropical Depression'
dff[grepl("TROPICAL STORM", dff$EVTYPE, ignore.case=T), eg] <- 'Tropical Storm'
dff[grepl("FOG|VOG|CLOUD", dff$EVTYPE, ignore.case=T), eg] <- 'Fog'
dff[grepl("LANDSLIDE|MUDSLIDE|SLIDE|LANDSLUMP", dff$EVTYPE, ignore.case=T), eg] <- 'Landslides'
dff[grepl("DROWNING|MARINE|SEAS|SURF|CURRENT|TIDE|TSUNAMI|WATER|SEICHE|WAVE|SWELL|SURGE", dff$EVTYPE, ignore.case=T), eg] <- 'Surf & Seas'
dff[grepl("EROSION|EROSIN|DAM", dff$EVTYPE, ignore.case=T), eg] <- 'Erosion'
dff[grepl("OTHER", dff$EVTYPE, ignore.case=T), eg] <- 'Other'
dff[grepl("EXPOSURE|HYPERTHERMIA|HYPOTHERMIA", dff$EVTYPE, ignore.case=T), eg] <- 'Exposure'

table(dff$EV_GROUP)
```

```
## 
##           Avalanche                Cold             Drought                Dust             Erosion 
##                 387                3932                2539                   8                  72 
##            Exposure                Fire               Flood                 Fog                Heat 
##                   8                4260               86117                8829               24521 
##          Hurricanes          Landslides           Lightning               Other       Precipitation 
##                 296                 654               15768                  52              302542 
##                Snow         Surf & Seas            Tornados Tropical Depression      Tropical Storm 
##               20346               19449               60903                  60                 697 
##   Volcanic Activity    Weather & Storms                Wind      Winter Effects 
##                  29              125874              224566                 225
```

```r
sum(is.na(dff$EV_GROUP))
```

```
## [1] 163
```
- There a few remaining event types in the database that don't make sense to bring forward. At this point, all remaining items have no injuries or fatalities.
- One refinement on this filtering process for the future would be to deal with items that really cover two different groupings for example entries with FLOOD and WIND in their title.

## Creating a dollar amount column for both property and crop damage

- There are EXP exponent fields within the data. Let's take a look at their values.


```r
table(dff$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5      6      7      8      B 
## 465934      1      8      5    216     25     13      4      4     28      4      5      1     40 
##      h      H      K      m      M 
##      1      6 424665      7  11330
```

```r
table(dff$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

- From this information let's convert these exponent columns into a number we can use later.
- First starting with the alphanumeric versions of the exponents and expanding them
- And then moving on to the numeric versions, convert PROPDMGEXP character column to numeric 
- Create final PropertyDamage column
- Repeate for Crop Damage lines


```r
dff$PropExpNumeric <- as.numeric(dff$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
dff$PropExp <- (10 ^ dff$PropExpNumeric)

dff[is.na(dff$PropExpNumeric), c('PropExp')] <- 1
dff[grepl("H", dff$PROPDMGEXP, ignore.case=T), c('PropExp')] <- (10 ^ 2)
dff[grepl("K", dff$PROPDMGEXP, ignore.case=T), c('PropExp')] <- (10 ^ 3)
dff[grepl("M", dff$PROPDMGEXP, ignore.case=T), c('PropExp')] <- (10 ^ 6)
dff[grepl("B", dff$PROPDMGEXP, ignore.case=T), c('PropExp')] <- (10 ^ 9)
dff$PropertyDamage <- dff$PROPDMG * dff$PropExp


dff$PropExp <- NULL
dff$PropExpNumeric <- NULL

head(dff,2)
```

```
##    EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP EV_GROUP PropertyDamage
## 1 TORNADO       15          0    25.0          K       0            Tornados          25000
## 2 TORNADO        0          0     2.5          K       0            Tornados           2500
```

```r
dff[grepl("\\-", dff$PROPDMGEXP, ignore.case=T), ][1, ]
```

```
##           EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP EV_GROUP PropertyDamage
## 229327 HIGH WIND        0          2      15          -       0                Heat             15
```

```r
dff[grepl("\\?", dff$PROPDMGEXP, ignore.case=T), ][1, ]
```

```
##                    EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 198689 THUNDERSTORM WINDS        0          0       0          ?       0           
##                EV_GROUP PropertyDamage
## 198689 Weather & Storms              0
```

```r
dff[grepl("\\+", dff$PROPDMGEXP, ignore.case=T), ][1, ]
```

```
##                  EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP EV_GROUP
## 188780 BREAKUP FLOODING        0          0      20          +       0               Flood
##        PropertyDamage
## 188780             20
```

```r
dff[grepl("h", dff$PROPDMGEXP, ignore.case=F), ][1, ]
```

```
##                       EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 209285 THUNDERSTORM WIND G50        0          0       2          h       0           
##                EV_GROUP PropertyDamage
## 209285 Weather & Storms            200
```

```r
dff[grepl("m", dff$PROPDMGEXP, ignore.case=F), ][1, ]
```

```
##                EVTYPE INJURIES FATALITIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP   EV_GROUP
## 187584 HURRICANE OPAL        0          0      20          m      10          m Hurricanes
##        PropertyDamage
## 187584          2e+07
```

```r
dff$PROPDMG <- NULL
dff$PROPDMGEXP <- NULL

head(dff,2)
```

```
##    EVTYPE INJURIES FATALITIES CROPDMG CROPDMGEXP EV_GROUP PropertyDamage
## 1 TORNADO       15          0       0            Tornados          25000
## 2 TORNADO        0          0       0            Tornados           2500
```


```r
dff$CropExpNumeric <- as.numeric(dff$CROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
dff$CropExp <- (10 ^ dff$CropExpNumeric)

dff[is.na(dff$CropExpNumeric), c('CropExp')] <- 1
dff[grepl("H", dff$CROPDMGEXP, ignore.case=T), c('CropExp')] <- (10 ^ 2)
dff[grepl("K", dff$CROPDMGEXP, ignore.case=T), c('CropExp')] <- (10 ^ 3)
dff[grepl("M", dff$CROPDMGEXP, ignore.case=T), c('CropExp')] <- (10 ^ 6)
dff[grepl("B", dff$CROPDMGEXP, ignore.case=T), c('CropExp')] <- (10 ^ 9)
dff$CropDamage <- dff$CROPDMG * dff$CropExp


dff$CropExp <- NULL
dff$CropExpNumeric <- NULL

head(dff,2)
```

```
##    EVTYPE INJURIES FATALITIES CROPDMG CROPDMGEXP EV_GROUP PropertyDamage CropDamage
## 1 TORNADO       15          0       0            Tornados          25000          0
## 2 TORNADO        0          0       0            Tornados           2500          0
```

```r
dff[grepl("\\?", dff$CROPDMGEXP, ignore.case=T), ][1, ]
```

```
##                   EVTYPE INJURIES FATALITIES CROPDMG CROPDMGEXP EV_GROUP PropertyDamage CropDamage
## 192467 FLASH FLOOD WINDS        0          0       0          ?    Flood           0.41          0
```

```r
dff[grepl("m", dff$CROPDMGEXP, ignore.case=F), ][1, ]
```

```
##                EVTYPE INJURIES FATALITIES CROPDMG CROPDMGEXP   EV_GROUP PropertyDamage CropDamage
## 187584 HURRICANE OPAL        0          0      10          m Hurricanes          2e+07      1e+07
```

```r
dff$CROPDMG <- NULL
dff$CROPDMGEXP <- NULL

head(dff,2)
```

```
##    EVTYPE INJURIES FATALITIES EV_GROUP PropertyDamage CropDamage
## 1 TORNADO       15          0 Tornados          25000          0
## 2 TORNADO        0          0 Tornados           2500          0
```

- Aggregate Dollar Amounts for the different Event Type Groups against Property Damage, Crop Damage, and then Both for a Total.
- Merge into a single data frame for final reporting


```r
property_damage <- aggregate(PropertyDamage ~ EV_GROUP, data=dff, FUN=sum)
crop_damage <- aggregate(CropDamage ~ EV_GROUP, data=dff, FUN=sum)
total_damage <- aggregate(PropertyDamage + CropDamage ~ EV_GROUP, data=dff, FUN=sum)
total_damage <- rename(total_damage, c("PropertyDamage + CropDamage"="TotalDamage"))

property_damage <- merge(property_damage, crop_damage, by='EV_GROUP')
total_damage <- merge(property_damage, total_damage, by='EV_GROUP')

total_damage
```

```
##               EV_GROUP PropertyDamage  CropDamage  TotalDamage
## 1            Avalanche        3721800           0      3721800
## 2                 Cold      256729400  3021454000   3278183400
## 3              Drought     1046106000 13972571780  15018677780
## 4                 Dust          20000           0        20000
## 5              Erosion       62447000   296285000    358732000
## 6             Exposure              0           0            0
## 7                 Fire     8496728500   403281630   8900010130
## 8                Flood   168218674485 12388567200 180607241685
## 9                  Fog       25206100           0     25206100
## 10                Heat     5888116743  1578155400   7466272143
## 11          Hurricanes    85356335010  5516117800  90872452810
## 12          Landslides      327452100    20017000    347469100
## 13           Lightning      940532430    12092090    952624520
## 14               Other          55500     1034400      1089900
## 15       Precipitation    16518788803  4119261673  20638050476
## 16                Snow     1678863650   246723100   1925586750
## 17         Surf & Seas    48316562140    48177500  48364739640
## 18            Tornados    58552992106   417463070  58970455176
## 19 Tropical Depression        1737000           0      1737000
## 20      Tropical Storm     7714390550   694896000   8409286550
## 21   Volcanic Activity         500000           0       500000
## 22    Weather & Storms    20111440916  5726117888  25837558804
## 23                Wind     4683795735   641975850   5325771585
## 24      Winter Effects       23607750         800     23608550
```

# Question #1 Results 
Will first determine which event types cause the most injuries..

- Aggregate the data based on INJURIES against EV_GROUP
- Display Top 10 Event Types based on sum of Injuries for the EV_GROUP


```r
dfa_injuries <- aggregate(INJURIES ~ EV_GROUP, data=dff, FUN=sum, na.rm=T)
dfa_injuries[order(-dfa_injuries$INJURIES), ][1:10,]
```

```
##            EV_GROUP INJURIES
## 18         Tornados    91407
## 10             Heat    10271
## 8             Flood     8676
## 23             Wind     7367
## 22 Weather & Storms     6780
## 13        Lightning     5231
## 16             Snow     1959
## 15    Precipitation     1916
## 7              Fire     1608
## 17      Surf & Seas     1531
```

Now determine which event types cause the most fatalities

- Aggregate the data based on FATALITIES against EV_GROUP
- Display Top 10 Event Types based on sum of Fatalities for the EV_GROUP


```r
dfa_fatalities <- aggregate(FATALITIES ~ EV_GROUP, data=dff, FUN=sum, na.rm=T)
dfa_fatalities[order(-dfa_fatalities$FATALITIES), ][1:10,]
```

```
##            EV_GROUP FATALITIES
## 18         Tornados       5661
## 10             Heat       3238
## 8             Flood       1553
## 17      Surf & Seas       1099
## 13        Lightning        817
## 23             Wind        663
## 22 Weather & Storms        599
## 2              Cold        434
## 16             Snow        265
## 1         Avalanche        225
```

- Make a "Tornado" plot to display injuries and fatalities ordered by injuries.
- A "Tornado" plot would allow us to visualize how injuries and fatalities compare and if there is correlation between them.
- Merge data together and then create the "Tornado" diagram with pyramid.plot from the plotrix library.
- In order to make the "Tornado" diagram more illutrative we're going to ironically remove the 'Tornado" event type out of the data because clearly looking at the data the "Tornados" have the greatest impact but will skew any graph we make.

- Remove this graph before publishing .. in order to stay under 

```r
df_merge <- merge(dfa_injuries, dfa_fatalities, by='EV_GROUP')
tiny <- df_merge[order(df_merge$INJURIES), ]

par(mar=pyramid.plot(tiny$INJURIES, tiny$FATALITIES, tiny$EV_GROUP, 
			    top.labels=c("Injuries", "Event Type", "Fatalities"), 
			    main="Health Impact of Weather Events", 
			    laxlab=c(1,100000), raxlab=c(1,100000),
			    unit="", lxcol=rev(heat.colors(24)), rxcol=rev(heat.colors(24)), 
			    gap=35000, space=0.1, 
			    ppmar=c(3,2,3.5,4), labelcex=0.7,
			    show.values=T, ndig=0
			    ))
```

![](PA2_template_files/figure-html/unnamed-chunk-13-1.png)

- In the above chart we can see that 'Surf & Sea' fatalities is a bit of an outlier in terms of having proportionally more fatalities then other categories in the same area of injury counts
- Let's compare with the same plot ordered by fatalties to see if other outliers / lack of correlation pops out
- Maybe keep this graph for publishing - 


```r
tiny <- df_merge[order(df_merge$FATALITIES), ]

par(mar=pyramid.plot(tiny$INJURIES, tiny$FATALITIES, tiny$EV_GROUP, 
			    top.labels=c("Injuries", "Event Type", "Fatalities"), 
			    main="Health Impact of Weather Events", 
			    laxlab=c(1,100000), raxlab=c(1,100000),
			    unit="", lxcol=rev(heat.colors(24)), rxcol=rev(heat.colors(24)), 
			    gap=35000, space=0.1, 
			    ppmar=c(3,2,3.5,4), labelcex=0.7,
			    show.values=T, ndig=0
			    ))
```

![](PA2_template_files/figure-html/unnamed-chunk-14-1.png)

- Comparison of these two graphs shows how injuries and fatalities are not entirely corrrelated
- Using the 'Tornado' plot type allows us to see this lack of correlation easily.
- Clearly in the 2nd fatalities ordered graph, 'Lightning', 'Cold', and 'Avalanche' pop out as being further outliers from just 'Surf & Seas'
- Given the 'Tornados' event type skews the scale of the graph.. let's see what the latter graph looks like without that event in the data
- Definately keep this graph for publishing


```r
df_sans <- df_merge[!grepl('Tornados', df_merge$EV_GROUP),]
head(df_sans[order(-df_sans$INJURIES), ])
```

```
##            EV_GROUP INJURIES FATALITIES
## 10             Heat    10271       3238
## 8             Flood     8676       1553
## 23             Wind     7367        663
## 22 Weather & Storms     6780        599
## 13        Lightning     5231        817
## 16             Snow     1959        265
```

```r
tiny <- df_sans[order(df_sans$FATALITIES), ]

par(mar=pyramid.plot(tiny$INJURIES, tiny$FATALITIES, tiny$EV_GROUP, 
			    top.labels=c("Injuries", "Event Type", "Fatalities"), 
			    main="Health Impact of Weather Events", 
			    laxlab=c(1,10000), raxlab=c(1,10000),
			    unit="", lxcol=rev(heat.colors(24)), rxcol=rev(heat.colors(24)), 
			    gap=4000, space=0.1, 
			    ppmar=c(3,2,3.5,4), labelcex=0.7,
			    show.values=T, ndig=0
			    ))
```

![](PA2_template_files/figure-html/unnamed-chunk-15-1.png)

- And sure enough we can see further outliers to the smooth injury curve.. 'Hurricanes', 'Tropical Storms', and 'Landslides' skews towards fatalities over injuries.
- Curious what the injuries ordered graph looks like...
- Remove this graph before publishing .. in order to stay under 


```r
tiny <- df_sans[order(df_sans$INJURIES), ]

par(mar=pyramid.plot(tiny$INJURIES, tiny$FATALITIES, tiny$EV_GROUP, 
			    top.labels=c("Injuries", "Event Type", "Fatalities"), 
			    main="Health Impact of Weather Events", 
			    laxlab=c(1,10000), raxlab=c(1,10000),
			    unit="", lxcol=rev(heat.colors(24)), rxcol=rev(heat.colors(24)), 
			    gap=4000, space=0.1, 
			    ppmar=c(3,2,3.5,4), labelcex=0.7,
			    show.values=T, ndig=0
			    ))
```

![](PA2_template_files/figure-html/unnamed-chunk-16-1.png)

