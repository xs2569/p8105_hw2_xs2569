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
count(distinct(subway_df, line, station_name))
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   465

``` r
unique_subway_df <- distinct(subway_df, line, station_name, .keep_all = TRUE)
view(unique_subway_df)
```

##### 1.2.2 How many stations are ADA compliant?

``` r
count(unique_subway_df, ada == TRUE)
```

    ## # A tibble: 2 × 2
    ##   `ada == TRUE`     n
    ##   <lgl>         <int>
    ## 1 FALSE           381
    ## 2 TRUE             84

##### 1.2.3 What proportion of station entrances / exits without vending allow entrance?

``` r
A = count(subway_df, vending == "NO")
B = count(subway_df, vending == "NO" & entry_logic == TRUE)

B/A
```

    ##   vending == "NO" & entry_logic == TRUE         n
    ## 1                                   NaN 1.0676558
    ## 2                                     1 0.3770492

\#Problem 2
