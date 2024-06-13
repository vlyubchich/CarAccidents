# Use this code to prepare data for USA.
# Sections with [titles] in brackets can be skipped as they do not modify the data.

rm(list = ls())
library(dplyr)


# US accidents ----

## US-Accidents: A Countrywide Traffic Accident Dataset (CTAD v4)
## downloaded 2021-04-09
## https://smoosavi.org/datasets/us_accidents
# untar("./dataraw/US_Accidents_Dec20.tar.gz") # unzip archive
# D <- read.csv("./dataraw/US_Accidents_Dec20.csv") # ~5 min
# saveRDS(D, "./dataderived/CTADv4.rds")

CTAD <- readRDS("./dataderived/CTADv4.rds")
length(unique(CTAD$Airport_Code)) # 2023

## filter to keep only states for comparisons within the needed dates ----
min(CTAD$Start_Time) #[1] "2016-02-08 00:37:08"
max(CTAD$Start_Time) #[1] "2020-12-31 23:28:56"
DateUSAStart <- as.Date("2016-03-01") # date of start of weather data for USA (end will be cut with DateCovid)
DateCovid <- as.Date("2020-01-01") # date of start of COVID-19 impacts
## states resulting from propensity score matching
STATES <- c(c("AZ","CA","FL","IN","KS","MA","MI","MT","NC","NV","NY","SD","TX","UT","VA","WV"),
            "NJ")
ctad <- CTAD %>%
    filter(is.element(State, STATES)) %>%
    filter(Airport_Code != "") %>%
    filter(DateUSAStart <= Start_Time & Start_Time < DateCovid)
nrow(ctad) # 1816571 1866509+NJ
Airports <- ctad %>%
    select(State, Airport_Code) %>%
    distinct(Airport_Code, .keep_all = TRUE)
nrow(Airports) # 944 961
# saveRDS(Airports, "./dataderived/USA_airports.rds")
# saveRDS(ctad, "./dataderived/ctad.rds")
# check for data quality
ctad0 <- CTAD %>%
    filter(is.element(State, STATES)) %>% #c("CA", "MA", "NV")
    filter(Airport_Code == "") %>%
    filter(DateUSAStart <= Start_Time & Start_Time < DateCovid) %>%
    mutate(Date = as.Date(Start_Time)) %>%
    group_by(State, Date) %>%
    summarise(Nacc = n())
ctad0 %>%
    ggplot() +
    aes(x = Date, y = Nacc, group = State, color = State) +
    geom_line() +
    theme_minimal()

## [exploratory check of existing weather data] ----
table(ctad$State)
head(ctad)
vars_weather <- c("Temperature.F.", "Wind_Chill.F.", "Humidity...",
                  "Pressure.in.", "Visibility.mi.", #"Wind_Direction", "Weather_Condition",
                  "Wind_Speed.mph.", "Precipitation.in.")
summary(ctad[,vars_weather])
# Temperature.F.   Wind_Chill.F.      Humidity...      Pressure.in.   Visibility.mi.   Wind_Speed.mph.  Precipitation.in.
# Min.   :-77.80   Min.   :-65.9     Min.   :  1.00   Min.   : 0.00   Min.   :  0.00   Min.   :  0.00   Min.   : 0
# 1st Qu.: 55.00   1st Qu.: 37.0     1st Qu.: 47.00   1st Qu.:29.84   1st Qu.: 10.00   1st Qu.:  4.60   1st Qu.: 0
# Median : 66.90   Median : 60.0     Median : 66.00   Median :29.97   Median : 10.00   Median :  7.00   Median : 0
# Mean   : 65.06   Mean   : 56.0     Mean   : 63.56   Mean   :29.85   Mean   :  9.19   Mean   :  8.26   Mean   : 0
# 3rd Qu.: 77.40   3rd Qu.: 75.0     3rd Qu.: 83.00   3rd Qu.:30.09   3rd Qu.: 10.00   3rd Qu.: 10.40   3rd Qu.: 0
# Max.   :170.60   Max.   :115.0     Max.   :100.00   Max.   :33.04   Max.   :140.00   Max.   :822.80   Max.   :24
# NA's   :37905    NA's   :1243393   NA's   :40218    NA's   :32387   NA's   :39583    NA's   :283999   NA's   :1291203
table(ctad$Wind_Direction)
table(ctad$Weather_Condition)


# US fatalities ----

# copied from above:
DateUSAStart <- as.Date("2016-03-01")
DateCovid <- as.Date("2020-01-01")

YearCovid <- as.numeric(format(DateCovid, "%Y"))
YearUSAStart <- as.numeric(format(DateUSAStart, "%Y"))
FARS <- lapply(YearUSAStart:(YearCovid - 1), function(y)
    read.csv(paste0("./dataraw/USA_FARS/FARS", y, "NationalCSV/accident.CSV"))
    )
FARS <- do.call(rbind, FARS) %>%
    rename(States = STATENAME,
           Year = YEAR,
           Month = MONTH,
           Day = DAY,
           Hour = HOUR,
           Minute = MINUTE,
           lat = LATITUDE,
           lon = LONGITUD,
           Fatals = FATALS) %>%
    select(States, Year, Month, Day, Hour, Minute, lat, lon, Fatals)
# saveRDS(FARS, paste0("./dataderived/FARS_", YearUSAStart, "_", YearCovid - 1, ".rds"))


# Weather download ----

redownload <- FALSE
ctad <- readRDS("./dataderived/ctad.rds")
# copied from above:
DateUSAStart <- as.Date("2016-03-01")
DateCovid <- as.Date("2020-01-01")
STATES <- c(c("AZ","CA","FL","IN","KS","MA","MI","MT","NC","NV","NY","SD","TX","UT","VA","WV"),
            "NJ")

Airports <- readRDS("./dataderived/USA_airports.rds") %>%
    filter(is.element(State, STATES))

## hourly airport data ----
# https://mesonet.agron.iastate.edu/request/download.phtml?network=AWOS
library(riem)
library(geosphere)

### [check network names] ----
if (redownload) {
    rn <- riem::riem_networks()
    all(grepl("ASOS", rn$name))
    rn[!grepl("ASOS", rn$name),]
    rn[grepl("Iowa", rn$name),]
}
## NOTE: for Iowa, also have "AWOS"

### get stations from each state ----
if (redownload) {
    Wstations <- lapply(STATES, function(s)
        riem::riem_stations(network = paste0(s, "_ASOS"))
    )
    Wstations <- do.call(rbind, Wstations)
    saveRDS(Wstations, "./dataderived/USA_Wstations.rds")
} else {
    Wstations <- readRDS("./dataderived/USA_Wstations.rds")
}

### select airports for which to download data ----
# add K to IDs so they match CTAD IDs
Wstations$id <- paste0("K", Wstations$id)
# add lat lon to airports
Airports <- Wstations %>%
    select(id, elevation, lon, lat) %>%
    right_join(Airports, by = c("id" = "Airport_Code")) %>%
    rename(Airport_Code = id) %>%
    mutate(IsReplaced = is.na(lat) & is.na(lon))
# approximate lat lon of missing airports (without weather data) using reported lat lon of accidents
Airports_miss <- Airports %>%
    filter(IsReplaced) %>%
    pull(Airport_Code)
Airports_replace <- ctad %>%
    filter(is.element(Airport_Code, Airports_miss)) %>%
    group_by(Airport_Code) %>%
    summarise(lat = mean(Start_Lat, na.rm = TRUE),
              lon = mean(Start_Lng, na.rm = TRUE)) %>%
    mutate(DistanceToNearestKm = NA,
           Replacement = NA)
# find geodesic distances from each missing airport to non-missing
# and select the closest non-missing as a Replacement
for (i in 1:nrow(Airports_replace)) { # i = 1
    # distance from this airport to existing (km)
    d <- geosphere::distm(Airports_replace[i, c("lon", "lat")],
                          Airports[, c("lon", "lat")], fun = distGeo) / 1000L
    di <- which.min(d)
    Airports_replace$DistanceToNearestKm[i] <- d[di]
    Airports_replace$Replacement[i] <- Airports$Airport_Code[di]
}
# merge the data about missing airports back to the table of Airports
Airports$lat[Airports$IsReplaced] <- Airports_replace$lat
Airports$lon[Airports$IsReplaced] <- Airports_replace$lon
Airports <- Airports_replace %>%
    select(-lat, -lon) %>%
    right_join(Airports, by = "Airport_Code")
Airports_dwnld <- Airports %>%
    filter(!IsReplaced)
# saveRDS(Airports, "./dataderived/USA_airports_coords.rds")

### download hourly airport data ----
# (download speed ~ 2 stations per minute)
# and save as separate files
STATES <- c("NJ") # only those that need to add
if (redownload) {
    for (s in STATES) { # s = "AZ"; station = "KBXK"
        for (station in Airports_dwnld$Airport_Code[Airports_dwnld$State == s]) {
            weatherdata <-  riem::riem_measures(station = station,
                                                date_start = DateUSAStart,
                                                date_end = DateCovid - 1)
            saveRDS(weatherdata, paste0("./dataderived/USA_weather/weather_", s, "_", station, ".rds"))
        }
    }
}

## daily daymet data ----
# https://cran.r-project.org/web/packages/daymetr/vignettes/daymetr-vignette.html
library(daymetr)
if (redownload) {
    for (i in 1:nrow(Airports)) { # i = 1
        s <- Airports$State[i]
        station <- Airports$Airport_Code[i]
        weatherdata <- daymetr::download_daymet(lat = Airports$lat[i], lon = Airports$lon[i],
                                                start = as.numeric(format(DateUSAStart, "%Y")),
                                                end = as.numeric(format(DateCovid - 1, "%Y")),
                                                internal = TRUE, simplify = TRUE)
        saveRDS(weatherdata, paste0("./dataderived/USA_weather_daymet/weather_", s, "_", station, "_daymet.rds"))
    }
}

# https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html (accessed 2021-12-28)
# The Daymet calendar is based on a standard calendar year. All Daymet years, including leap years, have 1 - 365 days. For leap years, the Daymet database includes leap day (February 29) and values for December 31 are discarded from leap years to maintain a 365-day year.
# combine into one
library(tidyr)

Airports <- readRDS("./dataderived/USA_airports.rds")
WD <- tibble()
for (i in 1:nrow(Airports)) { # i = 1
    s <- Airports$State[i]
    station <- Airports$Airport_Code[i]
    filename <- paste0("./dataderived/USA_weather_daymet/weather_", s, "_", station, "_daymet.rds")
    WD <- readRDS(filename) %>%
        pivot_wider(names_from = measurement, values_from = value) %>%
        mutate(Date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
        select(Date, prcp..mm.day., tmax..deg.c., tmin..deg.c.) %>%
        mutate(Airport_Code = station) %>%
        bind_rows(WD)
}
WD <- WD %>%
    rename(Precip = prcp..mm.day.,
           Tmax = tmax..deg.c.,
           Tmin = tmin..deg.c.) %>%
    mutate(Temp = (Tmax + Tmin) / 2 )
# saveRDS(WD, "./dataderived/USA_daymet.rds")


# Aggregate ----
# merge accidents with weather and other covariates
rm(list = ls())
library(dplyr)
library(tidyr)
library(suncalc)
library(timeDate)
library(lutz)

ctad <- readRDS("./dataderived/ctad.rds")
Airports <- readRDS("./dataderived/USA_airports_coords.rds")
# copied from above:
DateUSAStart <- as.Date("2016-03-01")
DateCovid <- as.Date("2020-01-01")
STATES <- c(c("AZ","CA","FL","IN","KS","MA","MI","MT","NC","NV","NY","SD","TX","UT","VA","WV"),
            "NJ")

## accidents ----
ctad <- ctad %>%
    mutate(Year = as.numeric(substring(ctad$Start_Time, 1, 4)),
           Month = as.numeric(substring(ctad$Start_Time, 6, 7)),
           Day = as.numeric(substring(ctad$Start_Time, 9, 10)),
           Hour = as.numeric(substring(ctad$Start_Time, 12, 13))
    )
# aggregate daily
ctad_d <- ctad %>%
    group_by(Airport_Code, Year, Month, Day) %>%
    summarise(Severity = mean(Severity, na.rm = TRUE),
              Nacc = n())
nrow(ctad) == sum(ctad_d$Nacc) # check
# aggregate hourly (same as daily, just add Hour below)
ctad_h <- ctad %>%
    group_by(Airport_Code, Year, Month, Day, Hour) %>%
    summarise(Severity = mean(Severity, na.rm = TRUE),
              Nacc = n())
nrow(ctad) == sum(ctad_h$Nacc) # check

## time matrix ----
# daily
TMd <- data.frame(Date = seq(from = DateUSAStart, to = DateCovid - 1, by = 1)) %>%
    mutate(Year = as.numeric(format(Date, "%Y")), # see ?strptime
           Month = as.numeric(format(Date, "%m")),
           Day = as.numeric(format(Date, "%d")),
           Weekday = as.numeric(format(Date, "%u"))
    ) %>%
    mutate(Weekend = is.element(Weekday, c(6, 7)))
Holidays <- as.Date(with(TMd, timeDate::holidayNYSE(min(Year):max(Year))))
TMd <- TMd %>%
    mutate(Holiday = is.element(Date, Holidays))

# hourly
TMh <- expand.grid(Date = TMd$Date, Hour = 0:23, KEEP.OUT.ATTRS = FALSE) %>%
    mutate(Year = as.numeric(format(Date, "%Y")), # see ?strptime
           Month = as.numeric(format(Date, "%m")),
           Day = as.numeric(format(Date, "%d")),
           Weekday = as.numeric(format(Date, "%u"))
    ) %>%
    mutate(Weekend = is.element(Weekday, c(6, 7))) %>%
    mutate(Holiday = is.element(Date, Holidays))

## combine general variables ----
# intersect dates with locations (i.e. set up the spatio-temporal domain)
# and add already computed variables like lat lon Weekend...
D <- expand.grid(Date = TMd$Date, Airport_Code = as.character(Airports$Airport_Code), KEEP.OUT.ATTRS = FALSE) %>%
    left_join(Airports, by = "Airport_Code") %>%
    left_join(TMd, by = "Date") %>%
    mutate(TimeZone = lutz::tz_lookup_coords(lat, lon, method = "accurate", warn = FALSE))
# the sun calculations are separate because cannot run different time zones in one vector
library(parallel)
Dtmp <- D %>%
    select(Date, lat, lon, TimeZone) # select only needed columns to use in cluster
ncores <- detectCores()
clust <- makeCluster(ncores)
clusterExport(clust, "Dtmp")
sun <- parSapply(clust, 1:nrow(Dtmp), function(x) {
    timePOSIXct <- suncalc::getSunlightTimes(date = Dtmp$Date[x],
                                             lat = Dtmp$lat[x],
                                             lon = Dtmp$lon[x],
                                             tz = Dtmp$TimeZone[x],
                                             keep = c("sunrise", "sunset"))[c("sunrise", "sunset")]
    # calculate decimal hour as hour + minutes/60
    sunriseH <- as.numeric(format(timePOSIXct$sunrise, "%H")) + as.numeric(format(timePOSIXct$sunrise, "%M")) / 60
    sunsetH <- as.numeric(format(timePOSIXct$sunset, "%H")) + as.numeric(format(timePOSIXct$sunset, "%M")) / 60
    c(sunriseH = sunriseH, sunsetH = sunsetH) #, DayLength = sunsetH - sunriseH
})
stopCluster(clust)
# sun <- sapply(1:nrow(D), function(x) {
#     timePOSIXct <- suncalc::getSunlightTimes(date = D$Date[x],
#                                              lat = D$lat[x],
#                                              lon = D$lon[x],
#                                              tz = D$TimeZone[x],
#                                              keep = c("sunrise", "sunset"))[c("sunrise", "sunset")]
#     # calculate decimal hour as hour + minutes/60
#     sunriseH <- as.numeric(format(timePOSIXct$sunrise, "%H")) + as.numeric(format(timePOSIXct$sunrise, "%M")) / 60
#     sunsetH <- as.numeric(format(timePOSIXct$sunset, "%H")) + as.numeric(format(timePOSIXct$sunset, "%M")) / 60
#     c(sunriseH = sunriseH, sunsetH = sunsetH, DayLength = sunsetH - sunriseH)
# })
D <- D %>%
    mutate(sunriseH = sun["sunriseH",],
           sunsetH = sun["sunsetH",],
           DayLength = sunsetH - sunriseH)
# saveRDS(D, "./dataderived/Dcommon.rds")

## combine with daymet ----
D <- readRDS("./dataderived/Dcommon.rds")
WD <- readRDS("./dataderived/USA_daymet.rds")
D <- D %>%
    left_join(WD, by = c("Airport_Code", "Date")) %>%
    left_join(ctad_d, by = c("Airport_Code", "Year", "Month", "Day"))
# when no accidents reported, insert zeros
D$Severity[is.na(D$Severity)] <- D$Nacc[is.na(D$Nacc)] <- 0
# saveRDS(D, "./dataderived/USA_daily_daymet.rds")


## combine with FARS ----
FARS <- readRDS("./dataderived/FARS_2016_2019.rds")
# add short state names and closest airport
fars <- FARS %>%
    left_join(data.frame(State = state.abb, States = state.name), by = "States") %>%
    filter(is.element(State, STATES),
           lon < 360 & lat < 90) %>%
    mutate(Airport_Code = NA,
           DistanceToAirport = NA)
for (i in 1:nrow(fars)) { # i = 1
    # distance from this place to existing airports (km)
    d <- geosphere::distm(fars[i, c("lon", "lat")],
                          Airports[, c("lon", "lat")], fun = distGeo) / 1000L
    di <- which.min(d)
    fars$DistanceToAirport[i] <- d[di]
    fars$Airport_Code[i] <- Airports$Airport_Code[di]
}
# aggregate daily
fars_d <- fars %>%
    group_by(Airport_Code, Year, Month, Day) %>%
    summarise(Fatals = sum(Fatals, na.rm = TRUE),
              NFatAcc = n())
nrow(fars) == sum(fars_d$NFatAcc) # check
D <- D %>%
    left_join(fars_d, by = c("Airport_Code", "Year", "Month", "Day"))
D$Fatals[is.na(D$Fatals)] <- D$NFatAcc[is.na(D$NFatAcc)] <- 0
# saveRDS(D, "./dataderived/USA_daily_daymet_fars.rds")


## [checks] ----
summary(D)
tmp <- D %>% filter(is.na(Precip))
head(tmp) # daymet doesn't have 2016-12-31 because leap year https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html

# > summary(D)
# Date            Airport_Code       DistanceToNearestKm Replacement          elevation            lon               lat
# Min.   :2016-03-01   Length:1346361     Min.   :  6.5       Length:1346361     Min.   : -37.00   Min.   :-124.24   Min.   :24.56
# 1st Qu.:2017-02-14   Class :character   1st Qu.: 26.6       Class :character   1st Qu.:  33.75   1st Qu.:-109.51   1st Qu.:32.71
# Median :2018-01-30   Mode  :character   Median : 41.2       Mode  :character   Median : 193.25   Median : -94.85   Median :36.70
# Mean   :2018-01-30                      Mean   : 49.8                          Mean   : 361.55   Mean   : -94.34   Mean   :36.61
# 3rd Qu.:2019-01-15                      3rd Qu.: 70.5                          3rd Qu.: 424.25   3rd Qu.: -80.82   3rd Qu.:40.72
# Max.   :2019-12-31                      Max.   :132.0                          Max.   :2312.00   Max.   : -69.99   Max.   :48.61
# NA's   :1216068                        NA's   :130293
# State           IsReplaced           Year          Month             Day           Weekday       Weekend         Holiday
# Length:1346361     Mode :logical   Min.   :2016   Min.   : 1.000   Min.   : 1.00   Min.   :1.000   Mode :logical   Mode :logical
# Class :character   FALSE:1216068   1st Qu.:2017   1st Qu.: 4.000   1st Qu.: 8.00   1st Qu.:2.000   FALSE:961961    FALSE:1314648
# Mode  :character   TRUE :130293    Median :2018   Median : 7.000   Median :16.00   Median :4.000   TRUE :384400    TRUE :31713
# Mean   :2018   Mean   : 6.739   Mean   :15.74   Mean   :3.999
# 3rd Qu.:2019   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.:6.000
# Max.   :2019   Max.   :12.000   Max.   :31.00   Max.   :7.000
#
# TimeZone            sunriseH        sunsetH        DayLength          Precip             Tmax             Tmin
# Length:1346361     Min.   :4.967   Min.   :16.08   Min.   : 8.267   Min.   :  0.000   Min.   :-25.58   Min.   :-38.320
# Class :character   1st Qu.:6.367   1st Qu.:17.87   1st Qu.:10.667   1st Qu.:  0.000   1st Qu.: 15.06   1st Qu.:  2.620
# Mode  :character   Median :6.867   Median :19.40   Median :12.350   Median :  0.000   Median : 23.78   Median : 10.710
# Mean   :6.817   Mean   :19.08   Mean   :12.268   Mean   :  2.804   Mean   : 21.84   Mean   :  9.723
# 3rd Qu.:7.300   3rd Qu.:20.22   3rd Qu.:13.833   3rd Qu.:  1.100   3rd Qu.: 29.95   3rd Qu.: 17.590
# Max.   :8.750   Max.   :21.92   Max.   :16.150   Max.   :479.870   Max.   : 50.94   Max.   : 34.560
# NA's   :961       NA's   :961      NA's   :961
#       Temp            Severity           Nacc             Fatals            NFatAcc
#  Min.   :-31.145   Min.   :0.0000   Min.   :  0.000   Min.   : 0.00000   Min.   :0.00000
#  1st Qu.:  9.005   1st Qu.:0.0000   1st Qu.:  0.000   1st Qu.: 0.00000   1st Qu.:0.00000
#  Median : 17.225   Median :0.0000   Median :  0.000   Median : 0.00000   Median :0.00000
#  Mean   : 15.781   Mean   :0.5941   Mean   :  1.386   Mean   : 0.05469   Mean   :0.05035
#  3rd Qu.: 23.635   3rd Qu.:0.0000   3rd Qu.:  0.000   3rd Qu.: 0.00000   3rd Qu.:0.00000
#  Max.   : 41.130   Max.   :4.0000   Max.   :165.000   Max.   :20.00000   Max.   :5.00000
#  NA's   :961
# >






