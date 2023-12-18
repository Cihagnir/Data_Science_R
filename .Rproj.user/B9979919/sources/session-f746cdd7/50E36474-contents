install.packages("dplyr")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("plyr")
install.packages("sf")

library("dplyr")
library("readxl")
library("ggplot2")
library("tidyr")
library("plyr")
library("sf")

## GENERAL DEFINE SECTION 

list_months = c("January","February","March","April","May","June","July","August","September","October","November","December")
list_season = c(rep("Winter",times=3),rep("Spring",times=3),rep("Summer",times=3),rep("Autumn",times=3))



## DATA CLEANING SECTION 

### Province Data Cleaning 
province_data <- read_excel("ForeignHouseSellBasedonProvince.xls", range = cell_rows(3:135))

  # Change the columns names 
colnames(province_data)  = c("Year", "City", "Total",list_months)
  
  # Replace the CITY columns value's Turkish onces
province_data$City <- sub(".*- ", "", province_data$City)

  # Fill the year columns to get rid of the empty values
province_data|>
  mutate(Year = rep(2013:2023, each=12)) -> province_data

  
  # Drop the " Total " values inside the CITY columns
province_data <- province_data[!(province_data$City == "Total"), ]

  # !!! Change the data shape to transform it into long format 
province_data |>
  gather(key = "Month", value = "Value", -c(Year, City, Total)) -> province_data_long_version

province_data_long_version <- province_data_long_version[, !names(province_data_long_version) %in% "Total"]

province_data_long_version |> 
  mutate(
    Season = mapvalues(province_data_long_version$Month, from=list_months, to=list_season)
  ) -> province_data_long_version

province_data_long_version <- na.omit(province_data_long_version)


### Sales Data Cleaning 
sales_data <- read_excel("ForeginHouseSellPercentage.xls", range = cell_rows(3:143))

  # Change the columns names 
colnames(sales_data) = c('Year','Month','Total','Foreigners_Sales','Percentage')

  # Little Function Define
cleanFunction = function(val){
  
  if(val > 500000){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

  # Create the fılter to useles values 
lapply(sales_data$Total, cleanFunction) -> filterList

# Apply the our filter into our function 
sales_data|>filter(unlist(filterList)) -> sales_data

# Replace the CITY columns value's Turkish onces
sales_data$Month <- sub(".*- ", "", sales_data$Month)

# Fill the empty year values on our data set
sales_data|>
  mutate(
    Year = c(rep(2013:2022, each=12),rep(2023,each=9)),
    Season = mapvalues(sales_data$Month, from=list_months, to=list_season)
  ) -> sales_data



  


### Nationality Data Cleaning 
nationality_data <- read_xls("ForeingHouseSellBasedNationality.xls", range = cell_rows(3:201))

  # Change the columns names 
colnames(nationality_data) = c("Year","Country", "Total",list_months)


  # Replace the CITY columns value's Turkish onces
nationality_data$Country <- sub(".*- ", "", nationality_data$Country)
nationality_data$Country <- sub(".*-", "", nationality_data$Country)

  # Fill the year columns to get rid of the empty values
nationality_data|>
  mutate(Year = rep(2015:2023, each=22)) -> nationality_data


  # Drop the " Total " values inside the CITY columns
nationality_data <- nationality_data[!(nationality_data$Country == "Total"), ]

  # !!! Change the data shape to transform it into long format 
nationality_data |>
  gather(key = "Month", value = "Value", -c(Year, Country, Total)) -> nationality_data_long_version
nationality_data_long_version <- nationality_data_long_version[, !names(nationality_data_long_version) %in% "Total"]
nationality_data_long_version <- na.omit(nationality_data_long_version)


## NULL VALUE CHECK SECTION

### Province Data 
province_data_null_table = colSums(is.na(province_data))
View(province_data_null_table)

sales_data_null_table = colSums(is.na(sales_data))
View(sales_data_null_table)

nationality_data_null_table = colSums(is.na(nationality_data))
View(nationality_data_null_table)



## YEAR VALUES CHECK 
	
total_sales

unique(province_data$Year)

unique(sales_data$Year)

unique(nationality_data$Year)


## DATA VISUALISATION  SECTION 

names(sales_data)
### Graph for house sales to foreigners by percentages
sales_data |> 
  group_by(Year)|> 
    summarise(
      total_sales = sum(Total), 
      foreigners_sales = sum(Foreigners_Sales),
      percentage = sum(Percentage) 
      )|> 
        ggplot(aes(x= Year)) + 
        geom_line(aes(y = percentage), col = '#add8e6', linewidth=1.5) +
        geom_point(aes(y = percentage), col = '#00008B')+ 
        scale_x_continuous(breaks=c(rep(2013:2023, each=1)))



  ## Map Graph
turkey_map <- st_read("tr-cities.json")

map_data <- merge(turkey_map, province_data_long_version, by.x = 'name', by.y = 'City', all.x= TRUE)
# map_data <- replace(map_data, is.na(map_data), 0)

custom_theme <- theme_void()+
  theme(
    plot.margin = margin(1,1,10,1,"pt"),
    plot.background = element_rect(fill="#001219",color=NA),
    legend.position = "bottom",
    legend.title = element_text(hjust=0.5,color="white",face="bold"),
    legend.text = element_text(color="white"),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )



ggplot(map_data, aes(fill = Season)) + 
  geom_sf() + 
  scale_fill_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Autumn" = "orange")) +
  labs(title = "Seasonal Distribution of House Sales in Turkish Cities",
      subtitle = "Color represents the most dominant season of sales",
      fill = "Season Color Scale") +   
  guides(fill=guide_legend( nrow=1, title.position="top", label.position="bottom" )) +
  custom_theme



  ## World Map Graph
world_map <- st_read("world-map.json")


map_data <- merge(world_map, nationality_data_long_version , 
                  by.x = 'name', by.y = 'Country', all.x= TRUE)
map_data <- replace(map_data, is.na(map_data), 0)

ggplot(map_data, aes(fill = Value))+
  geom_sf()+
  labs(title = "Foreigns House Sell Heat Map",
       subtitle = "Color represents amount of Sales",
       fill = "House Sales") +   
  guides(fill=guide_legend( nrow=1, title.position="top", label.position="bottom" )) +
  custom_theme


lol <- map_data |> group_by(name) |> dplyr::summarise(total = sum(Value))




#########################################
# That is lego type map as dangerous as fuck 

# # Make grid
# map_grid <- st_make_grid(
#   map_data, # map name 
#   n = c(60,60) # number of cells per longitude/latitude 
#     )|>
#   # convert back to sf object
#   st_sf()
# 
# # Extract centroids
# centroid <- map_grid |> st_centroid()
# 
# 
# # Intersect centroids with basemap
# centroid_clean <- centroid |>
#   st_intersection(map_data)
# 
# # Make a centroid without geom
# # (convert from sf object to tibble)
# centroid_no_geom <- centroid_clean |>
#   st_drop_geometry()
# 
# # Join with grid thanks to id column
# map_grid_clean <- map_grid |>
#   cross_join(centroid_no_geom)
# 
# 
# 
# ggplot() +
#   geom_sf(
#     map_grid_clean |> drop_na(), 
#     mapping=aes(geometry=st_make_grid.map_data..n...c.60..60..,fill=clss)
#   )+
#   geom_sf(centroid_clean,mapping=aes(geometry=st_make_grid.map_data..n...c.60..60..),fill=NA,pch=21,size=0.5)+
#   labs(title = "Seasonal Distribution of House Sales in Turkish Cities",
#        subtitle = "Color represents the most dominant season of sales",
#        fill = "Season Color Scale") +   
#   guides(fill=guide_legend( nrow=1, title.position="top", label.position="bottom" )) +
#   custom_theme
###########################























