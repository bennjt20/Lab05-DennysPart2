Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
John Bennett
Feb 22, 2022

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

Q: How many Denny’s locations are there in Alaska? A: There are 3
locations in AK.

``` r
dennys_ak <- dennys %>%
  filter(state == "AK")
nrow(dennys_ak)
```

    ## [1] 3

### Exercise 2

Q: How many La Quinta locations are there in Alaska? A: There are 2
locations in AK.

``` r
laquinta_ak <- laquinta %>%
  filter(state == "AK")
nrow(laquinta_ak)
```

    ## [1] 2

### Exercise 3

Q: How many pairings are there between all Denny’s and all La Quinta
locations in Alaska, i.e., how many distances do we need to calculate
between the locations of these establishments in Alaska? A: 11

``` r
dennys_laquinta_ak <- full_join(dennys_ak, laquinta_ak, by = "state")
dennys_laquinta_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # … with 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4.1

Q: How many observations are in the joined dennys_laquinta_ak data
frame? A: There are 11 observations.

``` r
ncol(dennys_laquinta_ak)
```

    ## [1] 11

### Exercise 4.2

Q: What are the names of the variables in this data frame? A: Address.x,
City.x, State.x, zip.x, longitude.x, latitude.x

### Exercise 5

Q: What function from the tidyverse do we use the add a new variable to
a data frame while keeping the existing variables? A: The mutate
function will help us to add a new variable while keeping the existing
variable.

### Exercise 6 Create “distance” variable

Q: Calculate the distances between all pairs of Denny’s and La Quinta
locations and save this variable as “distance”. Make sure to save this
variable in THE “dn_lq_ak” data frame so that you can use it later…. A:
\[Note: This is the point where I tried some creative problem solving…\]

\`\`{r Ex6DistanceVar} dennys_laquinta_ak \<- dennys_laquinta_ak %>%
mutate(distancevar = case_when(longitude.x == -149.8767 \~ long1,
latitude.x == 61.1953 \~ lat1, longitude.x == -149.8090 \~ long2,
latitude.x == 61.2097 \~ lat2, longitude.x == -147.7600 \~ long3,
latitude.x == 64.8366 \~ lat3))



    dennys_laquinta_ak <- dennys_laquinta_ak %>%
    summary(dennys) %>%
    glimpse(dennys)

    ```r
    ?dennys

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

Note: After manually entering the latitude and longitude below, the
function did not appear to produce an actual distance in km…

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = -149.8767 * pi / 180
  lat1  = 61.1953  * pi / 180
  long2 = -149.8090 * pi / 180
  lat2  = 61.2097  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((61.2097 - 61.1953)/2)^2 + cos(61.1953) * cos(61.2097) * sin((-149.8090 - -149.8767)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

These are my attempts to research online and derive a formula to help
create the “distance” variable.

``` r
my_points <- matrix(c(81.25798, 73.81277,    # Create longitude/latitude matrix
                      14.91254, 18.18145),
                    nrow = 2)
colnames(my_points) <- c("longitude", "latitude")
rownames(my_points) <- c("point_1", "point_2")
my_points                                    # Print longitude/latitude matrix
```

    ##         longitude latitude
    ## point_1  81.25798 14.91254
    ## point_2  73.81277 18.18145

\`\`{r Ex6GeoPack} install.packages(“geosphere”) # Install & load
geosphere library(“geosphere”)


    ``{r Ex6DistHav}
    my_dist <- distHaversine(my_points)          # Calculate Haversine distance
    my_dist                                      # Print Haversine distance

### Exercise 7

    [Note: Have to have "distance" variable, created in Ex6 above] Calculate the minimum distance between a Denny’s and La Quinta for each Denny’s location. To do so we group by Denny’s locations and calculate a new variable that stores the information for the minimum distance.

\`\`{r Ex7} dennys_laquinta_ak_mindist \<- dennys_laquinta_ak %>%
group_by(address.x) %>% summarize(closest = min(distance)) \`\`\`

### Exercise 8

\[Note: I ran into a major brick wall on Exercise #6, which forced me to
take a pause at this point and submit what I have, in the interest of
time\]

Describe the distribution of the distances Denny’s and the nearest La
Quinta locations in Alaska. Also include an appripriate visualization
and relevant summary statistics.

### Exercise 9

Repeat the same analysis for North Carolina: (i) filter Denny’s and La
Quinta Data Frames for NC, (ii) join these data frames to get a complete
list of all possible pairings, (iii) calculate the distances between all
possible pairings of Denny’s and La Quinta in NC, (iv) find the minimum
distance between each Denny’s and La Quinta location, (v) visualize and
describe the distribution of these shortest distances using appropriate
summary statistics.

### Exercise 10

Repeat the same analysis for Texas.

### Exercise 11

Repeat the same analysis for a state of your choosing, different than
the ones we covered so far.

### Exercise 12

Among the states you examined, where is Mitch Hedberg’s joke most likely
to hold true? Explain your reasoning.
