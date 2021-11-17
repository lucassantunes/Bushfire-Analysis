# Cleaning environment list
rm(list = ls())

# Setting wd
setwd(getwd())

# Checking for installed packages
check_packages <- c("tidyverse", "leaflet", "ggplot2", "naniar", 
                    "widgetframe", "sf", "raster", "spData", 
                    "tibbletime", "maps", "scales", "gridtext",
                    "lubridate", "grid", "gridExtra", "shiny",
                    "gganimate", "xts", "tmap", "gifski", 
                    "ggmap", "transformr", "reshape", "mapview", "ozmaps")

# Installing the missing ones
for (i in seq_along(check_packages)) {
  if(!(check_packages[i] %in% rownames(installed.packages()))){
    install.packages(check_packages[i])
  }
}

# Loading packages
for (pkg in check_packages) {
  library(pkg, character.only = TRUE)
}

########################################################################################################
#                                       Datasets
#


# Extracting downloaded dataset file from NASA
# MODIS/Aqua+Terra Thermal Anomalies/Fire locations 1km V006 NRT (Vector data) distributed by LANCE FIRMS
# I couldn't set it to be downloaded straight from website because NASA requires an authorization request
# and the link is deleted after a week
# I split the files so that I could upload it to github

filenames <- list.files("DL_FIRE_M6_156533", pattern="*.csv", full.names=TRUE)
bushfire_data_raw <- tibble()
for (file in filenames) {
  df <- read_csv(file)
  bushfire_data_raw <- rbind(bushfire_data_raw, df)
}


# Extracting dataset acquired from Australian Government Bureau of Meteorology for temperature
# http://www.bom.gov.au/climate/data/acorn-sat/#tabs=Data-and-networks


# Station data for 112 sites
stations <- read_csv("Acorn-Sat/acorn_sat_v2.1.0_daily_tmax/acorn_sat_v2.1.0_stations.csv")

# Extraction the station numbers, to use it afterwards for loading each of its min/max temperature
# registered
stn_nums <- as.character(stations$stn_num)
stn_nums_form <- c()
for (num in stn_nums) {
  ifelse(nchar(num) == 4, num <- paste0("00", num), ifelse(nchar(num) == 5, num <- paste0("0", num), num))
  stn_nums_form <- c(stn_nums_form, num) #formating station numbers as 6 digits
}

# Temperature data from Bureau of Metereology for each station
temperature_raw <- tibble()
col_names <- c("date", "temp", "type")

for (station in stn_nums_form) {
  tmax_temp <- read_csv(paste0("Acorn-Sat/acorn_sat_v2.1.0_daily_tmax/tmax.", station, ".daily.csv"))
  tmax_temp <- tmax_temp[2:length(tmax_temp$date), 1:2]
  tmax_temp$type <- "max"
  tmax_temp <- tmax_temp %>% setNames(col_names)
  
  tmin_temp <- read_csv(paste0("Acorn-Sat/acorn_sat_v2.1.0_daily_tmin/tmin.", station, ".daily.csv"))
  tmin_temp <- tmin_temp[2:length(tmin_temp$date), 1:2]
  tmin_temp$type <- "min"
  tmin_temp <- tmin_temp %>% setNames(col_names)
  
  temp_df <- rbind(tmax_temp, tmin_temp) # row binding tmax and tmin tibbles
  
  temperature_raw <- rbind(temperature_raw, temp_df) # row binding all data from each station
}

# Loading spending dataset
spending <- read_csv("spending/spending.csv")


# Extracting dataset for AQI (Air Quality Index) from ACT open data
# https://www.data.act.gov.au/Environment/Air-Quality-Monitoring-Data/94a5-zqnn
# We've focused on showing the variation of AQI in one specific region, instead of doing
# for the whole Australia. The reason for that is the lack of quality historic data from
# many of the states.
aqi_act_raw <- read_csv("AQI/Air_Quality_Monitoring_Data.csv")



# Extracting dataset for Forest impacts
forest_raw <- read_csv("Forest/forest-fires-2019-20.csv")



########################################################################################################
#                                     Preprocessing, Wrangling and Visualizing
#


# Representing the 112 stations spread out in Australian territory, from which we extracted the
# mean temperature by year

# Creating weather station icon
weather_stn_icon <- makeIcon(iconUrl = "https://i.imgur.com/pQ8JHqX.png", 
                             iconWidth = 18, iconHeight = 18,
                             iconAnchorX = 18, iconAnchorY = 18)
# Representing all the 112 stations on map
map_stations <- leaflet(data = stations) %>% 
                  setView(lng = 136.327253, lat = -27.167054, zoom = 3.7) %>% 
                  addTiles() %>% addMarkers(lng = ~lon, lat = ~lat, icon = weather_stn_icon)
mapshot(map_stations, file = paste0(getwd(), "/map_stations.png"))


# Calculating max and min average temperature by year from temperature_raw dataset
temperature_df <- temperature_raw %>% mutate(year = trim(format(date, "%Y"), .keep = "unused"))
temperature_df <- temperature_df %>% group_by(year) %>% summarise(mean_t = mean(temp, na.rm = TRUE))
mean_t_1910_2019 <- mean(temperature_df$mean_t)

# Subtracting the mean (1910 - 2019) from the column mean_t, so that we have the anual mean 
# temperature above or below the mean
temperature_df <- temperature_df %>% mutate(above_below = mean_t - mean_t_1910_2019, 
                                            date = parse_date(temperature_df$year, format = "%Y"),
                                            flag = ifelse(above_below >= 0, "ab", "bl"))

# Plotting temperature above/below average from 1910-2019
temp_plt <- ggplot(temperature_df) +
              geom_bar(stat = "identity", aes(x = date, y = above_below, fill = flag), size = 0.1, color = "white") +
              theme_minimal() +
              geom_hline(yintercept = 0, size = 0.3) +
              theme(panel.grid.minor = element_blank(),
                    legend.position = "none",
                    axis.text = element_text(size = 6),
                    axis.title = element_blank(),
                    axis.line.y = element_line(color = "#ACB0BD", size = 0.1),
                    plot.margin = unit(c(1,2,1,1), "cm"),
                    plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 7),
                    plot.caption = element_text(color = "#4E4B5C", hjust = 0, vjust = -1.5, size = 4)) +
              scale_fill_manual(values = c("#FF0D00", "#002AFF")) +
              labs(title = "Australia has been warming",
                   subtitle = "Mean annual temperature below or above average (ºC)",
                   caption = "Note: Average temperature calculated considering the timeframe 1910-2019") +
              annotate(geom = "curve", x = as.Date("2010-01-01", "%Y-%m-%d"), y = -0.25, 
                       xend = as.Date("2019-01-01", "%Y-%m-%d"), yend = -0.05,
                       curvature = 0.3, size = 0.2,arrow = arrow(length = unit(2, "mm"))) +
              annotate(geom = "text", x = as.Date("2010-01-01", "%Y-%m-%d"), y = -0.25, 
                       label = "2019 was the hottest\nrecorded year", hjust = "right", size = 1.5)
png(filename = "temperature.png", width = 5, height = 3.5,
    units = "in", res = 300, pointsize = 12)
print(temp_plt)
dev.off()



# Excluding volcano and offshore types of hotspots (type 1 and 3)
bushfire_data <- bushfire_data_raw %>% filter(type == 0 | type == 2 | type == 4)

# Dropping initially unnecessary variables and changing name to Lat / Long
bushfire_data <- bushfire_data %>% dplyr::select(-c("satellite", "version", "instrument", "scan", "track"))
names(bushfire_data)[1] <- "Lat"
names(bushfire_data)[2] <- "Long"

# Checking for missing values, even though this data is assumed to have been treated already
sum(is.na(bushfire_data))

# Eliminating frp(Fire Radiative Power - MW) <= 0
bushfire_data <- bushfire_data %>% filter(frp > 0)

# Parsing acq_time to HH:MM:SS
bushfire_data <- bushfire_data %>% mutate(acq_time = parse_time(acq_time, format = "%H%M"), .keep = "unused")

# Brightness and bright_t31 from Kelvin to Celsius
bushfire_data <- bushfire_data %>% mutate(brightness = brightness - 273, 
                                          bright_t31 = bright_t31 - 273, 
                                          .keep = "unused")

# Visualising confidence distribution
ggplot(bushfire_data, aes(y = confidence, x = "Fire")) +
  geom_boxplot() +
  ggtitle("Distribution of Confidence level for fire detection")
# Filtering the observations with confidence higher than 80
bushfire_data <- bushfire_data %>% filter(confidence > 80)

# Adding year / month columns
bushfire_data <- bushfire_data %>% 
                  mutate(month = format(acq_date, "%m"), year = format(acq_date, "%Y"))

# Eliminating year 2000
bushfire_data <- bushfire_data %>% filter(year != 2000)




# Considering fire seasons between 1st october to 31 january

fire_season_filt <- bushfire_data %>% filter(month %in% c("10", "11", "12", "01"))

fire_season <- fire_season_filt %>% 
                  group_by(year, month) %>% 
                  summarise(hotspots = n())
fire_season <- fire_season[2:length(fire_season$year), ] # Eliminating 2001-jan since it's part of
                                                         # season 2000-2001 and not 2001-2002

fire_season$flag <- rep(1:19, each = 4)
fire_season <- fire_season %>% 
                group_by(flag) %>% summarise(hotspots = sum(hotspots)) %>% 
                mutate(date = unique(fire_season_filt$year)[-20]) %>% .[, 2:3]


# Plotting line plot of fire seasons
fire_season <- fire_season %>% mutate(date2 = parse_date(date, format = "%Y"))
line_plt_season_fire <- ggplot(fire_season, aes(x = date2, y = hotspots)) +
                            geom_hline(yintercept = median(fire_season$hotspots), size = 0.1, linetype = "dashed") +
                            geom_line(position = "identity", size = 1, color = "#A31621") +
                            geom_point(data = fire_season[c(2, 12, 19),], 
                                       color = "black", size = 3, shape = 1) +
                            theme_minimal() +
                            labs(title = "Hotspots per fire season",
                                 y = "Hotspots", x = "Season",
                                 caption = "Note: Fire season considered from october to january of the next year") +
                            scale_y_continuous(labels = scales::label_comma(big.mark = ","),
                                               breaks = seq(0, 180000, by = 30000),
                                               limits = c(0, 180000)) +
                            scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
                            theme(plot.title = element_text(size = 20, face = "bold"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(color = "#4D5061"),
                                  axis.title.y = element_text(color = "#ACB0BD", hjust = 1, size = 8),
                                  axis.title.x = element_text(color = "#ACB0BD", hjust = 0, size = 8),
                                  axis.ticks = element_line(color = "#4D5061"),
                                  axis.text = element_text(color = "#ACB0BD", size = 6),
                                  plot.margin = unit(c(1,1,1,1), "cm"),
                                  plot.caption = element_text(color = "#4E4B5C", 
                                                              hjust = 0, vjust = -1.5, size = 4)) +
                            annotate(geom = "text", x = as.Date("2001-01-01", "%Y-%m-%d"), y = 138000, 
                                     label = "2002-2003 bushfires\nhas caused 4 deaths\nand 435 non-fatal injuries\ncausing severe damage specially to Canberra", 
                                     hjust = "left", size = 1.8, color = "#2A2B2A") +
                            annotate(geom = "text", x = as.Date("2011-01-01", "%Y-%m-%d"), y = 138000, 
                                     label = "2012-2013 bushfires\nhas caused 4 deaths\nand 11 non-fatal injuries", 
                                     hjust = "left", size = 1.8, color = "#2A2B2A") +
                            annotate(geom = "text", x = as.Date("2016-01-01", "%Y-%m-%d"), y = 116000, 
                                     label = "2019-2020 bushfires\nhas caused 479 deaths", 
                                     hjust = "left", size = 1.8, color = "#2A2B2A") +
                            annotate(geom = "text", x = as.Date("2018-01-01", "%Y-%m-%d"), y = 45000, 
                                     label = "Median hotspots\naround 51k", 
                                     hjust = "left", size = 1.4, color = "#4D5061")
png(filename = "fire_season_line.png", width = 6, height = 4,
    units = "in", res = 300, pointsize = 12)
print(line_plt_season_fire)
dev.off()



# AQI - ACT information from 2012 onwards
aqi_act <- aqi_act_raw[colnames(aqi_act_raw)[-(4:15)]]
aqi_act$Date <- parse_date(aqi_act$Date, format = "%d %B %Y")
# The maximum measure of each day is given by the highest AQI
aqi_daily <- aqi_act %>% group_by(Date) %>% summarise(AQI = max(AQI_Site)) %>% mutate(Year = year(Date),
                                                                                      Month = month(Date),
                                                                                      Day = day(Date))

year_2019 <- aqi_daily %>% filter(Year == 2019)

plt_AQI <- ggplot(year_2019, aes(x = Date, y = AQI)) +
                  geom_hline(yintercept = median(year_2019$AQI), size = 0.1, linetype = "dashed") +
                  geom_area(alpha = 0.25, color = "#275DAD", size = 0.2, fill = "#134074") +
                  theme_minimal() +
                  labs(title = "ACT Air Quality in 2019",
                       y = "AQI", x = "Year") +
                  theme(plot.title = element_text(size = 20, face = "bold"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.line = element_line(color = "#4D5061"),
                        axis.title.y = element_text(color = "#ACB0BD", hjust = 1, size = 8),
                        axis.title.x = element_text(color = "#ACB0BD", hjust = 0, size = 8),
                        axis.ticks = element_line(color = "#4D5061"),
                        axis.text = element_text(color = "#ACB0BD", size = 6),
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        plot.caption = element_text(color = "#4E4B5C", 
                                                    hjust = 0, vjust = -1.5, size = 4)) +
                        annotate(geom = "text", x = as.Date("2019-12-10", "%Y-%m-%d"), y = 82,     # median text
                                 label = "Median AQI of 59", 
                                 hjust = "left", size = 1.4, color = "#4D5061") +
                        annotate(geom = "segment", x = as.Date("2019-01-25", "%Y-%m-%d"), y = 500, # horiz left seg
                                 xend = as.Date("2019-02-14", "%Y-%m-%d"), yend = 500,
                                 size = 0.15, color = "#4D5061") +
                        annotate(geom = "segment", x = as.Date("2019-02-04", "%Y-%m-%d"), y = 500, # vertical seg
                                 xend = as.Date("2019-02-04", "%Y-%m-%d"), yend = 800,
                                 size = 0.15, color = "#4D5061") +
                        annotate(geom = "segment", x = as.Date("2019-02-04", "%Y-%m-%d"), y = 800, # shorter hor seg
                                 xend = as.Date("2019-02-10", "%Y-%m-%d"), yend = 800,
                                 size = 0.15, color = "#4D5061") +
                        annotate(geom = "text", x = as.Date("2019-02-12", "%Y-%m-%d"), y = 800, 
                                 label = "Part of 2018-2019\nbushfire season", 
                                 hjust = "left", size = 1.4, color = "#4D5061") +
                        annotate(geom = "segment", x = as.Date("2020-01-01", "%Y-%m-%d"), y = 1500, # horiz segm on the right
                                 xend = as.Date("2019-10-23", "%Y-%m-%d"), yend = 1500,
                                 size = 0.15, color = "#4D5061") +
                        annotate(geom = "segment", x = as.Date("2019-10-23", "%Y-%m-%d"), y = 1500, # vertical seg on the right
                                 xend = as.Date("2019-10-23", "%Y-%m-%d"), yend = 1450,
                                 size = 0.15, color = "#4D5061") +
                        annotate(geom = "text", x = as.Date("2019-10-23", "%Y-%m-%d"), y = 1390, 
                                 label = "2019-2020\nbushfire season", 
                                 hjust = "center", size = 1.4, color = "#4D5061")
png(filename = "aqi_2019.png", width = 6, height = 4,
    units = "in", res = 300, pointsize = 12)
print(plt_AQI)
dev.off()


# Forest impact comparison in 2019-2020
forest <- forest_raw[, c(1, 2, 4)]
forest <- forest %>% setNames(c("Jurisdiction", "Area", "Forest_area_burnt"))
forest <- forest %>% mutate(Percentage_area_burnt = (Forest_area_burnt/Area))

# Forest burnt area plot
forest_plt <- ggplot(forest) +
                geom_bar(aes(x = Jurisdiction, y = Percentage_area_burnt), 
                         stat = "identity", fill = "#B5BEC6") + 
                geom_bar(data = forest[c(1), ], 
                         aes(x = Jurisdiction, y = Percentage_area_burnt), fill = "#A31621", 
                         stat = "identity") +
                theme_minimal() +
                labs(title = "Forest impact",
                     subtitle = "2019-20 bushfire season",
                     y = "Percentage in area burnt") +
                scale_y_continuous(breaks = seq(0, 0.5, by = 0.1),
                                   limits = c(0, 0.6), labels = scales::percent_format(accuracy = 5L)) +
                theme(plot.title = element_text(size = 20, face = "bold"),
                      axis.ticks.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_text(color = "grey", size = 10),
                      axis.line = element_line(),
                      axis.title.x = element_text(hjust = 0.03, color = "grey"),
                      axis.line.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      plot.caption = element_text(color = "#4E4B5C", hjust = 0, vjust = -1.5, size = 4)) +
                annotate(geom = "text", x = 7, y = 0.08,
                         label = "Had roughly 6.5%\nof it's area burnt", 
                         hjust = "left", size = 2.2, color = "grey") +
                annotate(geom = "text", x = 2, y = 0.08,
                         label = "Had roughly 6.5%\nof it's area burnt", 
                         hjust = "left", size = 2.2, color = "grey") +
                annotate(geom = "text", x = 1, y = 0.36,
                         label = "ACT has been affected really hard in\nthis bushfire season, with around 35%\nof its area burnt", 
                         hjust = "left", size = 2.2, color = "red") +
                coord_flip()
png(filename = "forest_plt.png", width = 6, height = 4,
    units = "in", res = 300, pointsize = 12)
print(forest_plt)
dev.off()



# Plotting spending graph
spending_2 <- spending %>% mutate(Percentage = Expenditure/sum(Expenditure))
spending_plt <- ggplot(spending_2) +
                    geom_bar(aes(x = Sector, y = Percentage), 
                             stat = "identity", fill = "#968E85") +
                    geom_bar(data = spending_2[c(2), ], 
                             aes(x = Sector, y = Percentage), fill = "#6E0D25", 
                             stat = "identity") +
                    theme_minimal() +
                    labs(title = "Government spending",
                         subtitle = "19-20 bushfire season",
                         y = "Percentage spent") +
                    scale_y_continuous(breaks = seq(0, 0.7, by = 0.1),
                                       limits = c(0, 0.7), labels = scales::percent_format(accuracy = 5L)) +
                    theme(plot.title = element_text(size = 20, face = "bold"),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.y = element_text(color = "grey", size = 10),
                          axis.line = element_line(),
                          axis.title.x = element_text(hjust = 0.03, color = "grey"),
                          axis.line.y = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          plot.caption = element_text(color = "#4E4B5C", hjust = 0, vjust = -1.5, size = 4)) +
                    coord_flip()
png(filename = "spending_plt.png", width = 6, height = 4,
    units = "in", res = 300, pointsize = 12)
print(spending_plt)
dev.off()



# Using Shiny package to visualise bushfires throughout the years in the dataset

ui1 <- fluidPage(
  sliderInput("time", "date", as.numeric(min(bushfire_data$year)), 
              as.numeric(max(bushfire_data$year)),
              value = max(bushfire_data$year),
              step = 1,
              animate = T),
  leafletOutput("mymap")
)

server1 <- function(input, output, session) {
  points <- reactive({
    bushfire_data %>% 
      filter(year == input$time)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data = bushfire_data) %>%
      setView(lng = 136.327253, lat = -26.367054, zoom = 3.7) %>% 
      addTiles() %>% addCircleMarkers(data = points(), 
                                      color = "red", stroke = FALSE,
                                      radius = 1.7,
                                      fillOpacity = 0.3)
  })
}

shinyApp(ui1, server1)


# Plotting animated map for season 2019-2020
season_2019 <- seq(as.Date("2019-10-01"), as.Date("2020-02-01"), by = "day")
filtered_season_2019 <- bushfire_data %>% filter(acq_date %in% season_2019)


# Animated map season 2019-2020
season19_20_animate <- ggplot() +
                        geom_sf(data = ozmap_states) +
                        theme_minimal() +
                        geom_point(data = filtered_season_2019, 
                                   mapping = aes(x = Long, y = Lat, group = seq_along(month(acq_date))),
                                   colour = "red", alpha = 1, size = 2) +
                        transition_time(acq_date) +
                        labs(title = 'Date is {frame_time}')

# Chosing fps and frames
animate(season19_20_animate, nframes = 100, fps = 5)

