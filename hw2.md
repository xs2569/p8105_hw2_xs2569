p8105_hw2_xs2569
================
Xun Sun
2024-09-26

# 1\] problem 1 - NYC Transit data

### 1.1 read and clean

``` r
subway_df = 
  read_csv("./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |> 
  mutate( 
  entry_logic = ifelse(entry == "YES", TRUE, ifelse(entry == "NO", FALSE, NA)),
  route_count = 11 - rowSums(is.na(across(starts_with("route"), .names = "route")))) |> 
  select(line, station_name, station_latitude, station_longitude, route_count, entry_logic, vending, entrance_type, ada)
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
view(subway_df)
summary(subway_df)
```

    ##      line           station_name       station_latitude station_longitude
    ##  Length:1868        Length:1868        Min.   :40.58    Min.   :-74.03   
    ##  Class :character   Class :character   1st Qu.:40.69    1st Qu.:-73.99   
    ##  Mode  :character   Mode  :character   Median :40.73    Median :-73.96   
    ##                                        Mean   :40.73    Mean   :-73.94   
    ##                                        3rd Qu.:40.77    3rd Qu.:-73.91   
    ##                                        Max.   :40.90    Max.   :-73.76   
    ##   route_count     entry_logic       vending          entrance_type     
    ##  Min.   : 1.000   Mode :logical   Length:1868        Length:1868       
    ##  1st Qu.: 1.000   FALSE:115       Class :character   Class :character  
    ##  Median : 2.000   TRUE :1753      Mode  :character   Mode  :character  
    ##  Mean   : 2.286                                                        
    ##  3rd Qu.: 3.000                                                        
    ##  Max.   :11.000                                                        
    ##     ada         
    ##  Mode :logical  
    ##  FALSE:1400     
    ##  TRUE :468      
    ##                 
    ##                 
    ## 

##### This data frame :subway_df) now contains 9 variables as follows:

- **line** (`Character`): Indicates the subway line.
- **station_name** (`Character`): The name of the subway station.
- **station_latitude** (`Numeric`): The latitude coordinate of the
  station. Values range from 40.58 to 40.90, with a median at 40.73.
- **station_longitude** (`Numeric`): The longitude coordinate of the
  station. Values range from -74.03 to -73.76, with a median at -73.96.
- **route_count** (`Numeric`): The number of routes available at each
  station (calculated). On average, stations have about 2.29 routes
  available, ranging from 1 to 11.
- **entry_logic** (`Logical`): A logical indicator of whether there is
  an entry (`TRUE` if there is an entry, `FALSE` otherwise). 468
  stations have entries marked as `TRUE`, while 1400 are marked as
  `FALSE`.
- **vending** (`Character`): Information about vending availability at
  the station. The variable contains descriptive data about vending
  services.
- **entrance_type** (`Character`): Type of entrance. This variable
  describes the type of entrance each station has.
- **ada** (`Logical`): Logical indicator for whether the station is ADA
  accessible (`TRUE` if accessible, `FALSE` otherwise). 468 stations are
  ADA accessible, while 1400 are not.

Describe data cleaning steps: 1. **Read Data**: Import the dataset,
treating specific strings as NA values. 2. **Clean Column Names**:
Standardize column names for consistency. 3. **Calculate Entry Logic**:
Create a binary indicator for entry presence. 4. **Determine Route
Count**: Compute the number of non-missing route entries per station. 5.
**Select Columns**: Keep only the relevant columns.

The dimension of the resulting dataset: The resulting data set contains
9 columns and 1868 rows.

After the steps above, the data can still be cleaner.

### 1.2 answer questions

##### 1.2.1 How many distinct stations are there?

``` r
n_distinct(data.frame(x = subway_df$line, y = subway_df$station_name))
```

    ## [1] 465

``` r
unique_subway_df <- distinct(subway_df, line, station_name, .keep_all = TRUE)
view(unique_subway_df)
```

##### 1.2.2 How many stations are ADA compliant?

``` r
sum(unique_subway_df$ada == TRUE, na.rm = TRUE)
```

    ## [1] 84

##### 1.2.3 What proportion of station entrances / exits without vending allow entrance?

``` r
A <- sum(subway_df$vending == "NO", na.rm = TRUE)
B <- sum(subway_df$vending == "NO" & subway_df$entry_logic == TRUE, na.rm = TRUE)
 B / A
```

    ## [1] 0.3770492

# 2\] Problem 2 - Mr. Trash Wheel sheet

##### 2.1 Read and clean the Mr. Trash Wheel sheet

``` r
MrTrash_df = 
  read_excel("./data/202309_Trash_Wheel_Collection_Data.xlsx", 
             na = c("NA", "", "."),
             sheet = "Mr. Trash Wheel",
             skip = 1) |>
  janitor::clean_names() |> 
  filter(!is.na(dumpster)) |>
  mutate(sports_balls = as.integer(round(sports_balls)),
         year = as.double(year)) |>
  mutate(trash_wheel = "Mr. Trash Wheel") |>
  select(-x15, -x16)  # without this line the data will contain x15,x16 with all na

view(MrTrash_df)
summary(MrTrash_df)
```

    ##     dumpster        month                year     
    ##  Min.   :  1.0   Length:584         Min.   :2014  
    ##  1st Qu.:146.8   Class :character   1st Qu.:2016  
    ##  Median :292.5   Mode  :character   Median :2018  
    ##  Mean   :292.5                      Mean   :2018  
    ##  3rd Qu.:438.2                      3rd Qu.:2020  
    ##  Max.   :584.0                      Max.   :2023  
    ##       date                         weight_tons    volume_cubic_yards
    ##  Min.   :1900-01-20 00:00:00.00   Min.   :0.780   Min.   : 7.0      
    ##  1st Qu.:2016-07-28 18:00:00.00   1st Qu.:2.720   1st Qu.:15.0      
    ##  Median :2018-09-01 00:00:00.00   Median :3.195   Median :15.0      
    ##  Mean   :2018-07-23 03:36:59.18   Mean   :3.211   Mean   :15.3      
    ##  3rd Qu.:2020-11-17 12:00:00.00   3rd Qu.:3.730   3rd Qu.:15.0      
    ##  Max.   :2023-06-29 00:00:00.00   Max.   :5.620   Max.   :20.0      
    ##  plastic_bottles  polystyrene   cigarette_butts  glass_bottles   
    ##  Min.   : 210    Min.   :  48   Min.   :   900   Min.   :  0.00  
    ##  1st Qu.:1000    1st Qu.: 555   1st Qu.:  3900   1st Qu.: 10.00  
    ##  Median :1900    Median :1160   Median :  6500   Median : 18.00  
    ##  Mean   :1979    Mean   :1558   Mean   : 19833   Mean   : 21.62  
    ##  3rd Qu.:2780    3rd Qu.:2400   3rd Qu.: 24000   3rd Qu.: 30.00  
    ##  Max.   :5960    Max.   :6540   Max.   :310000   Max.   :110.00  
    ##   plastic_bags       wrappers     sports_balls   homes_powered  
    ##  Min.   :  24.0   Min.   : 180   Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.: 290.0   1st Qu.: 750   1st Qu.: 6.00   1st Qu.:40.62  
    ##  Median : 635.0   Median :1100   Median :11.00   Median :51.50  
    ##  Mean   : 916.8   Mean   :1416   Mean   :13.17   Mean   :47.28  
    ##  3rd Qu.:1242.0   3rd Qu.:1980   3rd Qu.:18.25   3rd Qu.:60.33  
    ##  Max.   :3750.0   Max.   :5085   Max.   :56.00   Max.   :93.67  
    ##  trash_wheel       
    ##  Length:584        
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

##### 2.2 Read and clean Professor Trash Wheel and Gwynnda Sheet

``` r
ProfTrash_df = 
  read_excel("./data/202309_Trash_Wheel_Collection_Data.xlsx", 
             na = c("NA", "", "."),
             sheet = "Professor Trash Wheel",
             skip = 1) |>
  janitor::clean_names() |> 
  filter(!is.na(dumpster)) |>
  mutate(trash_wheel = "Professor Trash Wheel", sports_balls = 0,
         year = as.double(year))

view(ProfTrash_df)
```

``` r
GwynTrash_df = 
  read_excel("./data/202309_Trash_Wheel_Collection_Data.xlsx", 
             na = c("NA", "", "."),
             sheet = "Gwynnda Trash Wheel",
             skip = 1) |>
  janitor::clean_names() |> 
  filter(!is.na(dumpster)) |>
  mutate(trash_wheel = "Gwynnda", sports_balls = 0,
         year = as.double(year))

view(GwynTrash_df)
```

##### 2.3 Combine

``` r
Combined_df <- bind_rows(MrTrash_df, ProfTrash_df, GwynTrash_df)
view(Combined_df)
summary(Combined_df)
```

    ##     dumpster      month                year     
    ##  Min.   :  1   Length:845         Min.   :2014  
    ##  1st Qu.: 71   Class :character   1st Qu.:2017  
    ##  Median :162   Mode  :character   Median :2019  
    ##  Mean   :223                      Mean   :2019  
    ##  3rd Qu.:373                      3rd Qu.:2021  
    ##  Max.   :584                      Max.   :2023  
    ##                                                 
    ##       date                         weight_tons    volume_cubic_yards
    ##  Min.   :1900-01-20 00:00:00.00   Min.   :0.610   Min.   : 5.00     
    ##  1st Qu.:2017-06-21 00:00:00.00   1st Qu.:2.490   1st Qu.:15.00     
    ##  Median :2019-10-25 00:00:00.00   Median :3.070   Median :15.00     
    ##  Mean   :2019-06-08 04:53:06.75   Mean   :3.009   Mean   :15.13     
    ##  3rd Qu.:2021-11-04 00:00:00.00   3rd Qu.:3.540   3rd Qu.:15.00     
    ##  Max.   :2023-06-30 00:00:00.00   Max.   :5.620   Max.   :20.00     
    ##                                                                     
    ##  plastic_bottles  polystyrene    cigarette_butts  glass_bottles   
    ##  Min.   :   0    Min.   :    0   Min.   :     0   Min.   :  0.00  
    ##  1st Qu.:1000    1st Qu.:  280   1st Qu.:  3200   1st Qu.: 10.00  
    ##  Median :1980    Median :  950   Median :  5500   Median : 18.00  
    ##  Mean   :2296    Mean   : 1631   Mean   : 15592   Mean   : 20.89  
    ##  3rd Qu.:2900    3rd Qu.: 2400   3rd Qu.: 16000   3rd Qu.: 28.00  
    ##  Max.   :9830    Max.   :11528   Max.   :310000   Max.   :110.00  
    ##  NA's   :1       NA's   :1       NA's   :1        NA's   :156     
    ##   plastic_bags      wrappers      sports_balls    homes_powered  
    ##  Min.   :    0   Min.   :  180   Min.   : 0.000   Min.   : 0.00  
    ##  1st Qu.:  280   1st Qu.:  840   1st Qu.: 0.000   1st Qu.:37.83  
    ##  Median :  680   Median : 1380   Median : 6.000   Median :49.50  
    ##  Mean   : 1082   Mean   : 2330   Mean   : 9.102   Mean   :45.87  
    ##  3rd Qu.: 1400   3rd Qu.: 2635   3rd Qu.:14.000   3rd Qu.:57.50  
    ##  Max.   :13450   Max.   :20100   Max.   :56.000   Max.   :93.67  
    ##  NA's   :1       NA's   :118                      NA's   :2      
    ##  trash_wheel       
    ##  Length:845        
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

##### 2.4 Summary

This dataset provides a detailed record of waste collection activities
by three entities: Mr. Trash Wheel, Professor Trash Wheel, and Gwynnda.
It consists of 845 observations, each capturing a month’s worth of waste
collection data from as early as 2014 to as recent as 2023.

Key variables in the dataset include `dumpster`, which identifies the
dumpster number; `month` and `year`, which are character strings
indicating the time of collection; and `date`, which provides a precise
timestamp. The `weight_tons` variable records the weight of the waste
collected in tons, while `volume_cubic_yards` indicates the volume in
cubic yards. The dataset also tracks the quantity of various waste
materials such as `plastic_bottles`, `polystyrene`, `cigarette_butts`,
`glass_bottles`, `plastic_bags`, and `wrappers`. Additionally, it
records the number of `sports_balls` collected and estimates the number
of `homes_powered` by the energy equivalent of the waste.

For example, the minimum and maximum weights of trash collected are 0.61
tons and 5.62 tons, respectively. The median number of `plastic_bottles`
collected is 1980, and the mean number of `cigarette_butts` is
1.5592427^{4}.
