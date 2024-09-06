```{r, message = FALSE, echo = FALSE}
# **Libraries:**
library(tidycensus)
library(sp)
library(sf)
library(tidyverse)
library(ggspatial)
library(spdep)
library(tmap)
library(spatialreg)
library(gstat) 
library(spatstat)
library(raster)
library(kableExtra)
```


## **Introduction**

Portland, Oregon? What is so special about this place? Well, other than the fact that Z is from there, not much?... We knew we wanted to show and share the statistical knowledge we learned in this month by looking at crime statistics of a city and if the location of those crimes were affected by other variables of the area. We chose to look at the year 2020, a year of numerous challenges and events that left a lasting impact on the world; for the United States, we have the start of the covid-19 pandemic, lock downs, social distancing, Black Lives Matter protests, and the presidential election. We should be mindful of these events when looking at our crime data. 

A little bit about the data that we used, there were a lot of joins used and *some* data manipulation, but the boundary lines we decided to use for the city of Portland are based off the decennial census tracts. From the glossary of Census.gov, "Census tracts are small, relatively permanent statistical subdivisions of a county.' that, "...generally have a population size between 1,200 and 8,000 people, with an optimum size of 4,000 people." For our tracts, there resides 5000 people in one. We also pulled income data from the ACS, which is the american community survey. It's goal is to keep a rough estimate of the population between each decennial census since with the ACS they don't go to everybody's door like the decenial census does. It also does not follow constitutional law made for the decennial census so it can include more information and it does not count toward congressional seats.

Our crime data is from the City of Portland itself, in the year 2020, within our boundaries, there were a total of 52,644 crimes reported. Our overarching goal was to look at the relationship between crimes and location across the city.


```{r, message = FALSE, echo = FALSE}
#Load in Crime Data:** (remember at end cite data)
crime <- read_csv("CrimeData-2020.csv") 
#Working with Crime Data:
crime_sf <- crime |>
  drop_na(OpenDataX, OpenDataY) |>
  st_as_sf(coords = c("OpenDataX", "OpenDataY"), remove = FALSE) |>
  st_set_crs(2913)

crime_sf <- crime_sf |>
  drop_na(Neighborhood)
```


```{r, message = FALSE, echo = FALSE, include = FALSE}
# **Load in Portland Shape File:** (remember at end cite data)

portland_neighborhoods <- st_read("Neighborhood_Boundaries.shp")
# Neighborhood boundaries shp

portland_neighborhoods_new <- portland_neighborhoods |>
  st_union() |>
  st_as_sf()
```


```{r, message = FALSE, echo = FALSE, include = FALSE}
# **Load in Census Data:**

desired_vars = c(
        all = "P2_001N", # All Residents
        hisp = "P2_002N", # Hispanic
        white = "P2_005N", # White
        baa = "P2_006N", # Black or African American
        amin = "P2_007N", # Native American(American Indian in data)
        asian = "P2_008N", # Asian
        nhopi = "P2_009N", # Native Hawaiian or Pacific Islander
        other = "P2_010N", # Some Other Race
        multi = "P2_011N" # Two or More Races
       )
```

```{r, message = FALSE, echo = FALSE, include = FALSE}
census_or_2020 = get_decennial(
  geometry = "TRUE",
  geography = "tract",
  state = "OR",
  county = c("Multnomah", "Clackamas", "Washington"),
  variables = desired_vars, 
  year = 2020,
  sumfile = "pl"
)

#pivot wider for tidy census data
census_or_2020 <- census_or_2020 |>
  pivot_wider(names_from = "variable", values_from = "value")
```


```{r, message = FALSE, echo = FALSE, include = FALSE}
# **Filtering Census Data to only include tracts in Portland**

census_or_2020 <- st_transform(census_or_2020, crs = 2913)
portland_neighborhoods_new <- st_transform(portland_neighborhoods_new, crs = 2913)

census_within_portland_sf <- st_filter(census_or_2020, portland_neighborhoods_new)
```



```{r, message = FALSE, echo = FALSE}
# **Counting crimes type within tracts**

# Find crimes within tracts
crime_in_tract <- st_join(crime_sf, census_within_portland_sf, join = st_within)
# count number of crimes
crime_tract_count <- as_tibble(crime_in_tract) %>%
  count(NAME, GEOID, OffenseType) %>%
  group_by(NAME) %>%
  slice_max(n)

#portland crime type counts with dropped na values sf
pctc_sf <- left_join(census_within_portland_sf, crime_tract_count) |> 
  drop_na(n)

```


```{r, message = FALSE, echo = FALSE}
# **Counting crime categories within tracts**

# count number of crimes
crime_tract_count2 <- as_tibble(crime_in_tract) %>%
  count(NAME, GEOID, OffenseCategory) %>%
  group_by(NAME) %>%
  slice_max(n)

#portland crime category counts with dropped na values sf
pccc_sf <- left_join(census_within_portland_sf, crime_tract_count2) |> 
  drop_na(n)
```

## **Most Reported Type of Crime by Census Tract**
```{r, out.width= "100%", echo = FALSE}
ggplot(pctc_sf) +
  geom_sf(aes(fill = OffenseType)) +
  labs(
    title = "Most Reported Type of Crime 
by Census Tract for the City of Portland",
    fill = "Crime Type"
  ) +
  scale_fill_brewer(palette = "Set3")
```

This was a graph we created showing the most reported type of crime per census tract, we can see that, the most common is theft from motor vehicle, which is theft of articles from a motor vehicle, whether locked or unlocked. There doesn't seem to be a lot of variety for the tracts, the most reported type of crime is related to *theft*.

```{r, , message = FALSE, echo = FALSE}
pctc_sf |>
  as_tibble() |>
  dplyr::select(NAME, OffenseType, n) |>
  slice_max(n, n = 10) |>
    kable() |>
  kable_styling()
```

A table for the top ten tracts with the most crimes committed. We can also categorize crime types into larger categories. 

```{r, out.width= "100%", echo = FALSE}
ggplot(pccc_sf) +
  geom_sf(aes(fill = OffenseCategory)) +
  labs(
    title = "Most Reported Category of Crime by Census Tract 
for the City of Portland",
    fill = "Crime Categories"
  ) +
  scale_fill_brewer(palette = "Set3")
```

From this graph we can see that Larceny Offenses are the most reported category of crime, these offenses consist of pocket-picking, purse-snatching, shoplifting, theft from building, theft from coin-op machine or device, theft from motor vehicle, theft of motor vehicle parts, and all other larceny. 



```{r, message = FALSE, echo = FALSE}
# **ACS INCOME DATA NOTE REMEMBER TO MENTION THIS IS NOT DECENNIAL DATA**

or_income <- get_acs(geography = "tract", 
              variables = c(medincome = "B19013_001"), 
              state = "OR", 
              year = 2020)

or_income <- or_income |>
  pivot_wider(names_from = "variable", values_from = "estimate")
```


```{r, , message = FALSE, echo = FALSE}
# **joining income data and census data by geoid**

census_within_portland_sf <-
  left_join(census_within_portland_sf, or_income) 
```


```{r, out.width= "100%", echo = FALSE}
# Areal Map of Larceny Offenses per Census Tract

ggplot(pccc_sf) +
  geom_sf(aes(fill = n)) +
  labs(
    title = "Number of Larceny Offenses per Census Tract",
    fill = "Number of Larceny Offenses"
  ) +
  scale_fill_gradient2()
```

Looking further into just larceny offenses per census tract, we counted the number of larceny crimes and plotted it down in our tracts, at first glance it is apparent that there is spatial autocorrelation of these offenses because we can see that colors on the scale that are close to each other stay by each other. It doesn't look like the colors could be randomly generated on the map, but we cannot say this for sure until we calculate Moran's I.


```{r, message = FALSE, echo = FALSE}
# creating neighbors for portland tracts 

# Create neigbors with na value dropped
pccc_nsf <- pccc_sf |> 
  filter(GEOID != "41051005701") |>
  filter(GEOID != "41051007300") |>
  filter(GEOID != "41051980000")

pccc_nb <- poly2nb(pccc_nsf, queen = FALSE) 
 # Create neighbor weights
pccc_nbw <- nb2listw(pccc_nb, style = "W", zero.policy = TRUE)
```


```{r, echo = FALSE}
# **Moran's I for Larceny Offenses**

moran.mc(pccc_nsf$n, pccc_nbw, nsim = 499)
```

**Our Test**

$H_O$: No spatial autocorrelation, I is close to 0

$H_A$: Spatial autocorrelation, I $\neq$ 0. 

Calculating Moran's I for larceny offenses in using Monte Carlo's simulations we get a Moran's I of .37878 which is positive and moderately strong. We also got a p-value of 0.002. We can reject the null hypothesis and conclude that it is statistically significant that larceny offenses in Portland, Oregon is spatially autocorrelated. Which means a tracts number of larceny offenses reported is similar to its surrounding tracts.


```{r, echo = FALSE, out.width= "100%"}
# SECTION 2
ggplot(census_within_portland_sf) +
  geom_sf(aes(fill = medincome)) +
  labs(
    title = "Median Income per Census Tract",
    fill = "Median Income (US Dollars)"
  ) +
  scale_fill_gradient2()
```

The ACS lacked median income data for three of the census tracts in Portland, which is why in our analysis of Larceny Offenses we also did not include these three tracts. Looking at this map we can also see that there is spatial autocorrelation for the median income of Portland, Oregon, but again, we cannot say this for sure until we calculate Moran's I.

```{r, message = FALSE, echo = FALSE}
# **Moran's I for median income**
pccc_sf <- left_join(census_within_portland_sf, crime_tract_count2) |> 
  drop_na(n)

pccc_nsf <- pccc_sf |> 
  filter(GEOID != "41051005701") |>
  filter(GEOID != "41051007300") |>
  filter(GEOID != "41051980000")

```

```{r, echo = FALSE}
moran.mc(pccc_nsf$medincome, pccc_nbw, nsim = 499)
```


**Our Test (Again):**

$H_O$: No spatial autocorrelation, I is close to 0

$H_A$: Spatial autocorrelation, I $\neq$ 0. 

Calculating Moran's I for median income in using Monte Carlo's simulations we get a Moran's I of .52218 which is positive and strong. We also got a p-value of 0.002. We can reject the null hypothesis and conclude that it is statistically significant that median income in Portland, Oregon is spatially autocorrelated. Which means a tracts median income reported is similar to its surrounding tracts.


```{r, message = FALSE, echo = FALSE, include=FALSE}
## **Moran's after accounting for income

regression_ci <- left_join(pccc_nsf, or_income)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, out.width="100%", results="hide" }
ggplot(regression_ci) + 
  geom_point(aes(medincome, n)) +
  geom_smooth(aes(medincome, n), method = lm, se = FALSE) +
  labs(
    title = "Median Income vs. Number of Larceny Offenses",
    x = "Median Income (US Dollars)",
    y = "Number of Larceny Offenses"
  )
```

From this graph we can see a slight negative relationship between median income and larceny offenses so, unlike what some people might expect, as median income goes up, theft slightly goes down. However, it is worth noting that this trend may be effected by the slight leverage point for the downtown area on the left side of the graph.

```{r, message = FALSE, echo = FALSE}
lm1 <- lm(n ~ medincome, data= regression_ci)
regression_ci$residual <- residuals(lm1)
```

```{r, echo = FALSE}
plot(lm1, 1)
summary(lm1)
```

The residual plot is reletively flat around the center suggesting linearity. The data appers to be somewhat normal considering there are a large number of points and they are also relatively centered around zero suggesting equal variance. A linear moder can be used, in particular, we will use the simple linear regression (SAR) model to account for the spatial aspect of the data.

```{r, echo = FALSE}
sarlm1 <- lagsarlm(n ~ medincome, data = regression_ci, listw = pccc_nbw)
summary(sarlm1)
```

Using a lagged linear model detrended with median income to measure the auto spatial correlation of residuals, a Rho value of 0.487 suggests a positive moderate correlation. A p-value of 0.02 after running Monte Carlo simulations indicates statistical significance. 

## **Tract Point Pattern**

For this section, we zoomed in on the census tract that includes the Downtown area of Portland.

The grid layout of the area also produced a regular pattern in the offences.

```{r, message = FALSE, echo = FALSE, warning = FALSE}
ct106.2 <- census_within_portland_sf |> 
  filter(GEOID == "41051010602")

ct106.2_points <- st_filter(crime_sf, ct106.2)

larceny_points <- ct106.2_points |> 
  filter(OffenseCategory == "Larceny Offenses")

vandalism_points <- ct106.2_points |> 
  filter(OffenseCategory == "Vandalism")

arson_points <- ct106.2_points |> 
  filter(OffenseCategory == "Arson")

```

```{r, echo = FALSE, message = FALSE, results = "hide"}
ggplot(ct106.2) +
  geom_sf() +
  annotation_map_tile() +
  geom_sf(data = larceny_points) +
  labs(
    title = "Census Tract 106.2 Larceny Offences",
  )
```

```{r, message = FALSE, echo = FALSE, warning = FALSE}
dt_larceny_ppp <- as.ppp(st_coordinates(larceny_points), W = ct106.2)
dt_vandalism_ppp <- as.ppp(st_coordinates(vandalism_points), W = ct106.2)
dt_arson_ppp <- as.ppp(st_coordinates(arson_points), W = ct106.2)
```


```{r, echo = FALSE, message = FALSE, include = FALSE}
# **Turning the specific census track to a PPP object** all crimes in dt tract
ct106.2ppp <- as.ppp(st_coordinates(ct106.2_points), W = ct106.2)

plot(ct106.2ppp)
```


However, from the density plot, we can clearly see areas of high and low intensity. The discrepancy between the center and other areas of Downtown exhibits an inhomogeneous process.

```{r, echo = FALSE, warning = FALSE, results = "hide", out.width= "100%"}
plot(density(ct106.2ppp), main = "Offenses per mile squared in Downtown")
points(ct106.2ppp, col = "white")
```

Our group was curious as to how some of the crime locations in 2020 were distributed in downtown Portland. In order to do this, we created a ppp object for the crime locations of different types in the census tract that covers the downtown area. We then performed the following K function analysis on a few different crimes to see how they were distributed. 

We used the inhomogenous K function for the analysis since there is a river included on the east side of this tract and to absoluletly nobodys surprise, crimes aren't really happening on this flowing body of water. This is why the crime data for downtown looks homogenous except for that section on the east side containing the river. Since there is this inconsistency in crime locations throughout the census tract, the data is inhomogenous prompting us to use that type of K function for the analysis.

The inhomogeneous K function was used for analysing the point pattern of each of larceny, vandalism, and arson offences at multiple scales.

## **K Function Analysis for Larceny, Vandalism, and Arson PPP Objects**
```{r, echo = FALSE, include = FALSE, results = "hide"}
dt_larceny_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot()
```

The first type of crime we examined in the downtown area was larceny because this was the most common crime in downtown Portland along with the majority of the census tracts in the City of Portland. The k function starts out above the envelope for a smaller radius from a given crime while for a larger radius, the k function for larceny in downtown Portland goes below the envelope. This means that on a small scale, the larceny crimes are clustered likely clustering at building locations while on a large scale, the larceny crimes are distributed with regularity. These patterns result in regularly spaced clusters of closely spaced offences. This means these clusters of points are distributed with regularity and that is likely due to the angular shape of the street grid in downtown Portland where each street intersects each other at a 90 degree angle so the buildings are distributed with regularity.

```{r,  out.width= "100%", echo = FALSE, incldue = FALSE, results = "hide"}
par(mfrow=c(1,2))
dt_vandalism_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot(main = "Inhomogeneous K for Larceny")

dt_arson_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot(main = "Inhomogeneous K for Arson")
```

## **K Function Analysis for Arson PPP object**

The next type of crime we examined in the downtown area was vandalism because this was the next most common crime in downtown Portland. We also examined arson since we though that was a more interesting crime type and may have had potential to have a different looking K function. Due to the regularity of buildings across this region due to the street grid though, we did not see very different k functions at all for these crimes where the function once again started above the envelope and finished below. 

They aren't completely identical though. The vandalism point pattern exhibits similar behavior to larceny offences. However, its empirical function is closer to the theoretical function than larceny offences. This suggests that although it follows the same trend, it is weaker.

The arson offences have stronger repulsion at greater distances than either offences. This is seen through the larger difference between the empirical function and simulation envelope.

```{r, echo = FALSE, results = "hide", warning = FALSE}
par(mfrow=c(1,1))
dt_df <- st_coordinates(ct106.2_points) |> 
  bind_cols(as_tibble(ct106.2_points)) |> 
  dplyr::select(X, Y, OffenseCategory) |> 
  mutate(OffenseCategory = as.factor(OffenseCategory))

dt_ppp <- as.ppp(dt_df, W = ct106.2) 

plot(dt_ppp, main = "Downtown PPP")
```

```{r, echo = FALSE, results = "hide"}
dt_ppp |> envelope(Kcross.inhom, i = "Larceny Offenses", j = "Arson", nsim = 5) |> 
  plot(main = "Inhomogeneous K Cross Between Larceny and Arson Offences")
```

This Kcross function is almost entirely under the envelope suggesting at all scales the locations of arson and larceny crimes repel or in other words are distributed with regularity when combined at all scales. There is strong repulsion upward of a radius of 200, there are significantly less offences with a closest neighbor than expected by the null landscape.

## **Closure**
Overall, although all this analysis was fun and interesting, we are unable to conclude causation for any analysis we've done. It's also worth noting that there were limitations in our analysis where at many time we ran into the issue of having missing values for a few census tracts or a census tract not having complete information on income. 

## **References**

Census.gov Glossary
Bureau, U. C. (2022, April 11). Glossary. Census.gov. https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_13 

Neighborhood boundaries data:
City of Portland. (2024). Portland Neighborhood Boundaries Open Data Shapefile. PortlandMaps Open Data. Retrieved from https://gis-pdx.opendata.arcgis.com/datasets/1e95d9b9076742ed9a71de0535ac255c/explore

Portland crime data:
City of Portland. (2020). Portland Police Bureau 2020 Open Data. Tableau Public. Retrieved from https://public.tableau.com/app/profile/portlandpolicebureau/viz/New_Monthly_Neighborhood/MonthlyOffenseTotals


Tidycensus data:
Walker, K., & Herman, M. (2023). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames (Version 1.5) [Computer software]. Retrieved from https://walker-data.com/tidycensus/

## **Demographics of Census Tracts**

```{r, message = FALSE, echo = FALSE}
pccc_sf <- pccc_sf |>
  mutate(hisp_per = hisp/all,
         white_per = white/all,
         baa_per = baa/all,
         amin_per = amin/all,
         asian_per = asian/all,
         nhopi_per = nhopi/all,
         other_per = other/all,
         multi_per = multi/all
        )
 

```

```{r, echo = FALSE, out.width= "100%"}
 ggplot(pccc_sf) +
  geom_sf(aes(fill = white_per)) +
  labs(
    title = "Percentage of White Individuals per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
```

```{r, echo = FALSE, message = FALSE}
pccc_nb2 <- poly2nb(pccc_sf, queen = FALSE) 
 # Create neighbor weights
pccc_nbw2 <- nb2listw(pccc_nb2, style = "W", zero.policy = TRUE)
```

```{r, echo = FALSE}
moran.mc(pccc_sf$white_per, pccc_nbw2, nsim = 499)
```

**Our Test**

$H_O$: No spatial autocorrelation, I is close to 0

$H_A$: Spatial autocorrelation, I $\neq$ 0. 

Calculating Moran's I for the percentage of white individuals in using Monte Carlo's simulations we get a Moran's I of 0.7077 which is positive and very strong. We also got a p-value of 0.002. We can reject the null hypothesis and conclude that it is statistically significant that the percentage of white individuals in Portland, Oregon is spatially autocorrelated.

**More Graphs**
```{r, echo = FALSE, out.width= "100%"}
 ggplot(pccc_sf) +
  geom_sf(aes(fill = baa_per)) +
  labs(
    title = "Percentage of Black or African American Individuals
per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
```

```{r, echo = FALSE, out.width= "100%"}
 ggplot(pccc_sf) +
  geom_sf(aes(fill = hisp_per)) +
  labs(
    title = "Percentage of Hispanic Individuals per 
Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
```

```{r, echo = FALSE, out.width= "100%"}
 ggplot(pccc_sf) +
  geom_sf(aes(fill = asian_per)) +
  labs(
    title = "Percentage of Asian Individuals per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
```
