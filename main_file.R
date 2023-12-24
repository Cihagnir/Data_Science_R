install.packages("dplyr")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("plyr")
install.packages("sf")
install.packages("treemap")
install.packages('plotly')
install.packages('rjson')

library("dplyr")
library("readxl")
library("ggplot2")
library("tidyr")
library("plyr")
library("sf")
library('treemap')
library('plotly')

## GENERAL DEFINE SECTION 

list_months = c("January","February","March","April","May","June","July","August","September","October","November","December")
list_season = c(rep("Winter",times=3),rep("Spring",times=3),rep("Summer",times=3),rep("Autumn",times=3))
list_months_numeric = c(rep(1:12, each=1))

turkey_map <- st_read("MapJson/tr-cities.json")
world_map <- st_read("MapJson/world-map.json")

#####  DATA CLEANING SECTION ##### 

#####  Province Data Cleaning #####  
province_data <- read_excel("DataSets/ForeignHouseSellBasedonProvince.xls", range = cell_rows(3:135))

  # Change the columns names 
colnames(province_data)  = c("Year", "City", "Total",list_months)
  
  # Replace the CITY columns value's Turkish onces
province_data$City <- sub(".*- ", "", province_data$City)

  # Fill the year columns to get rid of the empty values
province_data|>
  mutate(Year = rep(2013:2023, each=12)) -> province_data

  
  # Drop the " Total " values inside the CITY columns
province_data <- province_data[!(province_data$City == 'Total') & !(province_data$City == 'Other provinces'), ]


  # !!! Change the data shape to transform it into long format 
province_data |>
  gather(key = "Month", value = "Value", -c(Year, City, Total)) -> province_data_long_version

province_data_long_version <- province_data_long_version[, !names(province_data_long_version) %in% "Total"]

province_data_long_version |> 
  mutate(
    Season = mapvalues(province_data_long_version$Month, from=list_months, to=list_season)
    ) -> province_data_long_version

province_data_long_version <- na.omit(province_data_long_version)


#####  

#####  Sales Data Cleaning #####  
sales_data <- read_excel("DataSets/ForeginHouseSellPercentage.xls", range = cell_rows(3:143))

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

  # Create the fD1lter to useles values 
lapply(sales_data$Total, cleanFunction) -> filterList

# Apply the our filter into our function 
sales_data|>filter(unlist(filterList)) -> sales_data

# Replace the CITY columns value's Turkish onces
sales_data$Month <- sub(".*- ", "", sales_data$Month)

# Fill the empty year values on our data set
sales_data|>
  mutate(
    Year = c(rep(2013:2022, each=12),rep(2023,each=9)),
    Season = mapvalues(sales_data$Month, from=list_months, to=list_season),
    Date_numeric = 
      paste(
        mapvalues(sales_data$Month, from = list_months, to = list_months_numeric),
        Year,sep = '/'),
    norm_forg_sales = log(Foreigners_Sales),
    norm_total_sales = log(Total)
  ) -> sales_data

sales_data$Date_numeric <- factor(sales_data$Date_numeric, levels = sales_data[['Date_numeric']])


#####  

#####  Nationality Data Cleaning #####  
nationality_data <- read_xls("DataSets/ForeingHouseSellBasedNationality.xls", range = cell_rows(3:201))

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

#####  

#####  GPD Data Cleaning #####  
gdp_data <- read_xls("DataSets/GpdPerCapita.xls", range = cell_rows(4:86))

gdp_data <- gdp_data[colSums(is.na(gdp_data)) < nrow(gdp_data)]

colnames(gdp_data) = c("Code", 'City', rep(2004:2022, each=1), rep(2004:2022, each=1))

#####  


##### NULL VALUE CHECK SECTION ##### 

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

#####  


##### DATA VISUALISATION  SECTION ##### 

#####  Graph for house sales to foreigners by percentages #####  
sales_data |> 
  group_by(Year)|> 
    dplyr::summarise(
      total_sales = sum(Total), 
      foreigners_sales = sum(Foreigners_Sales),
      percentage = sum(Percentage) 
      )|>
        ggplot(aes(x= Year)) + 
        geom_line(aes(y = percentage), col = '#add8e6', linewidth=1.5) +
        geom_point(aes(y = percentage), col = '#00008B')+ 
        scale_x_continuous(breaks=c(rep(2013:2023, each=1)))

#####  

#####  Whole sales data we have #####  
graph_two <- plot_ly()

graph_two |> 
  add_trace(
    data = sales_data,
    x = ~Date_numeric,
    y = ~norm_forg_sales,
    name = 'Forg House Sales',
    type = 'scatter', mode = 'lines+markers',
    line = list(width = 3)) -> graph_two

graph_two |> 
  add_trace(
    data = sales_data,
    x = ~Date_numeric,
    y = ~norm_total_sales,
    name = 'Total House Sales',
    type = 'scatter', mode = 'lines+markers',
    line = list(width = 3)) -> graph_two

graph_two

##### 

#####  Detailed search for sales on specific year as 2018 #####  

graph_three <- plot_ly()
  
temp_data = sales_data|> filter(Year == 2018)
temp_data$Month <- factor(temp_data$Month, levels = temp_data[['Month']])
temp_data <- temp_data |> 
  mutate(
    norm_forg_sales = log(Foreigners_Sales),
    norm_total_sales = log(Total)
  )

graph_three |> 
  add_trace(
    data = temp_data,
    x = ~Month,
    y = ~norm_forg_sales,
    name = '2019 Forg House Sales',
    type = 'scatter', mode = 'lines+markers',
    line = list(width = 3)) -> graph_three

graph_three |> 
  add_trace(
    data = temp_data,
    x = ~Month,
    y = ~norm_total_sales,
    name = '2019 Total House Sales',
    type = 'scatter', 
    mode = 'lines+markers',
    line = list(width = 3)) -> graph_three

graph_three

#####

#####  Detailed search for sales on specific year as 2018 #####  

graph_four <- plot_ly()

temp_data = sales_data|> filter(Year == 2020)
temp_data$Month <- factor(temp_data$Month, levels = temp_data[['Month']])
temp_data <- temp_data |> 
  mutate(
    norm_forg_sales = log(Foreigners_Sales),
    norm_total_sales = log(Total)
  )

graph_four |> 
  add_trace(
    data = temp_data,
    x = ~Month,
    y = ~norm_forg_sales,
    name = '2020 Forg House Sales',
    type = 'scatter', mode = 'lines+markers',
    line = list(width = 3)) -> graph_four

graph_four |> 
  add_trace(
    data = temp_data,
    x = ~Month,
    y = ~norm_total_sales,
    name = '2020 Total House Sales',
    type = 'scatter', 
    mode = 'lines+markers',
    line = list(width = 3)) -> graph_four

graph_four

#####

#####  Graph for the " Is there a corralation between total house sales and foregins sales perct #####  

sales_data |> 
  group_by(Year)|> 
  filter(Year < 2023)|>
  dplyr::summarise(
    total_sales = log(sum(Total), 14),
    percentage = log2(sum(Percentage))
  )|>
    ggplot(aes(x= Year)) + 
    geom_line(aes(y = percentage), col = 'blue', linewidth=1.5) +
    geom_point(aes(y = percentage), col = 'red') + 
    geom_line(aes(y = total_sales), col = 'red', linewidth=1.5) +
    geom_point(aes(y = total_sales), col = 'blue')


#####  

#####  Pie chart for city distrubition on sales #####  
temp_data <- province_data_long_version |> group_by(City) |> dplyr::summarise(sales = `mean`(Value))

graph_six <- plot_ly(
                labels = temp_data$City, 
                values = temp_data$sales, 
                type = 'pie')

graph_six <- graph_four |> 
  layout(title = 'House Sales Based City ',
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

graph_six

##### 

##### Bar chart for seasonal interest most solded city #####

province_data_long_version |> 
  group_by( City, Season ) |>
  dplyr::summarise(
    sales = mean(Value))|>
  filter((City == 'İstanbul') | (City == 'Antalya') | (City == 'Ankara')) -> temp_data

temp_data$Season <- factor(temp_data$Season, levels = c('Winter','Spring','Summer','Autumn'))

graph_seven <- plot_ly()

for (city_val in temp_data$City|>unique()){
  
  temp_data_two = temp_data|>filter(City == city_val)
  
  graph_seven |> 
    add_trace( 
      data = temp_data_two, 
      x = ~Season,
      y = ~sales,
      name = city_val) -> graph_seven
}

graph_seven |>
layout(
  barmode = 'group'
  ) -> graph_seven

graph_seven 
  
#####  

#####  Tree Map for5 Sales depending on Season #####  

temp_data <-  sales_data |> 
                group_by(Year,Season)|> 
                  dplyr::summarise( foreigners_sales = sum( Foreigners_Sales ) ) |>  
                filter(Year < 2023)

treemap(
  temp_data, index = c('Year','Season'), vSize = 'foreigners_sales', type = "index",
  border.col = c("orange","white"),
  title = "Seasonal distribution  of the house sales",
  fontsize.labels = c(12, 10), 
  bg.labels=c("transparent"),
  fontcolor.labels = c('white', 'orange'),
  align.labels =  list( c('left', 'top'), c('right','bottom') ),
  inflate.labels =  F,
        )
#####  

#####  Turkye Map Graph Season #####  

geojson <- rjson::fromJSON(file='MapJson/tr-cities.json')

map_data <- merge(turkey_map, province_data_long_version, by.x = 'name', by.y = 'City', all.x= TRUE)

  # Optional for lego grid ::::  map_data <- replace(map_data, is.na(map_data), 0)

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


#####  

#####  Turkye Map Heat Graph  #####  

##### Old Code ##### 
# map_raw_data <- province_data_long_version |> group_by(City) |> dplyr::summarise(sales = mean(Value))
# 
# map_data <- merge(turkey_map, map_raw_data, by.x = 'name', by.y = 'City', all.x= TRUE)
# 
# # Optional for lego grid ::::  map_data <- replace(map_data, is.na(map_data), 0)
# 
# custom_theme <- theme_void()+
#   theme(
#     plot.margin = margin(1,1,10,1,"pt"),
#     plot.background = element_rect(fill="#001219",color=NA),
#     legend.position = "bottom",
#     legend.title = element_text(hjust=0.5,color="white",face="bold"),
#     legend.text = element_text(color="white"),
#     plot.title = element_text(color = "white"),
#     plot.subtitle = element_text(color = "white")
#   )
# 
# 
# 
# ggplot(map_data, aes(fill = sales)) +
#   geom_sf() +
#   labs(title = "Seasonal Distribution of House Sales in Turkish Cities",
#        subtitle = "Color represents the most dominant season of sales",
#        fill = "Season Color Scale") +
#   guides(fill=guide_legend( nrow=1, title.position="top", label.position="bottom" )) +
#   custom_theme
#####

##### New Code ##### 

geojson <- rjson::fromJSON(file='MapJson/tr-cities.json')
map_raw_data  <- province_data_long_version |> group_by(City) |> dplyr::summarise(sales = mean(Value))

map_data <- merge(turkey_map, map_raw_data, by.x = 'name', by.y = 'City', all.x= TRUE)
map_data <- replace(map_data, is.na(map_data), 1)
map_data |> mutate(City = name, norm_sales = log(sales) )-> map_data
map_data |> dplyr::select(-c(geometry,number,name)) -> map_data

geo_feature <- list(
  fitbounds = "locations",
  visible = FALSE
)
map_two <- plot_ly() 
map_two |> add_trace(
  type = "choropleth",
  geojson = geojson,
  locations = map_data$City,
  z = map_data$norm_sales,
  colorscale = "a",
  featureidkey = "properties.name"
) -> map_two
map_two |> layout( geo = geo_feature ) -> map_two

map_two |> colorbar(title = "House Sales") -> map_two

map_two |> layout( title = "Average house sales heat map of Turkey"
) -> map_two

map_two

#####  

#####  World Map Graph #####  

geojson <- rjson::fromJSON(file='MapJson/world-map.json')

map_raw_data <- nationality_data_long_version |> 
  group_by(Country) |> dplyr::summarise( Sales = mean(Value) )

map_data <- merge(world_map, map_raw_data, by.x = 'name', by.y = 'Country', all.x= TRUE)
map_data <- replace(map_data, is.na(map_data), 1)

map_data |> mutate(Country = name, Norm_sales = log(Sales) )-> map_data
map_data |> dplyr::select(-c(geometry,id,name)) -> map_data

geo_feature <- list(
  fitbounds = "locations",
  visible = FALSE
)
map_three <- plot_ly() 
map_three |> add_trace(
  type = "choropleth",
  geojson = geojson,
  locations = map_data$Country,
  z = map_data$Norm_sales,
  colorscale = "a",
  featureidkey = "properties.name"
) -> map_three
map_three |> layout( geo = geo_feature ) -> map_three

map_three |> colorbar(title = "House Sales") -> map_three

map_three |> layout( title = "Average house sales heat map of Turkey"
) -> map_three

map_three

#####  

#####  Nationality Inspectation ##### 
nationality_data |> 
  group_by( Country ) |>
  dplyr::summarise(
    Total_sales = sum(Total)
  ) -> temp_data 

temp_data <- temp_data[!(temp_data$Country == "Other countries"), ]
temp_data <- temp_data[order(temp_data$Total_sales,decreasing = TRUE),]
temp_data$Country <- factor(temp_data$Country, levels = temp_data[['Country']])

plot_ly(
  data = temp_data,
  x = ~Country,
  y = ~Total_sales,
  type = 'bar', 
  marker = list(
    colorscale = list(
      c(0, max( temp_data$Total_sales) ), 
      c("lawngreen", "red")
    ),
    colorbar = list( title = "House Sold Decade" ),
    color = ~Total_sales
  )
) -> graph_eight 

graph_eight

#####  

##### Top 3 For Sales Historically #####


temp_data <- nationality_data |> filter((Country == 'Iraq') | (Country == 'Russia') | (Country == 'Iran'))
temp_data <- temp_data |> mutate(norm_total = log(Total))

graph_nine <- plot_ly()

for (nat_val in temp_data$Country |> unique()){
  
  filter_data <- temp_data|> filter(Country == nat_val)
  
  graph_nine |> 
    add_trace(
      data = filter_data,
      x = ~Year,
      y = ~Total,
      name = paste(as.character(nat_val),'House Sales'),
      type = 'scatter', mode = 'lines+markers',
      line = list(width = 3)) -> graph_nine 
  
  
}

graph_nine

#####


########### DUMMY SECTION ########### 



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























