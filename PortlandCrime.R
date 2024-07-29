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

#Load in Crime Data:** (remember at end cite data)
crime <- read_csv("CrimeData-2020.csv") 
#Working with Crime Data:
crime_sf <- crime |>
  drop_na(OpenDataX, OpenDataY) |>
  st_as_sf(coords = c("OpenDataX", "OpenDataY"), remove = FALSE) |>
  st_set_crs(2913)

crime_sf <- crime_sf |>
  drop_na(Neighborhood)

# **Load in Portland Shape File:** (remember at end cite data)

portland_neighborhoods <- st_read("Neighborhood_Boundaries.shp")
# Neighborhood boundaries shp

portland_neighborhoods_new <- portland_neighborhoods |>
  st_union() |>
  st_as_sf()

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

# **Filtering Census Data to only include tracts in Portland**

census_or_2020 <- st_transform(census_or_2020, crs = 2913)
portland_neighborhoods_new <- st_transform(portland_neighborhoods_new, crs = 2913)

census_within_portland_sf <- st_filter(census_or_2020, portland_neighborhoods_new)

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

# **Counting crime categories within tracts**

# count number of crimes
crime_tract_count2 <- as_tibble(crime_in_tract) %>%
  count(NAME, GEOID, OffenseCategory) %>%
  group_by(NAME) %>%
  slice_max(n)

#portland crime category counts with dropped na values sf
pccc_sf <- left_join(census_within_portland_sf, crime_tract_count2) |> 
  drop_na(n)

# **Most Reported Type of Crime by Census Tract**
ggplot(pctc_sf) +
  geom_sf(aes(fill = OffenseType)) +
  labs(
    title = "Most Reported Type of Crime 
by Census Tract for the City of Portland",
    fill = "Crime Type"
  ) +
  scale_fill_brewer(palette = "Set3")

pctc_sf |>
  as_tibble() |>
  dplyr::select(NAME, OffenseType, n) |>
  slice_max(n, n = 10) |>
    kable() |>
  kable_styling()

#table for the top ten tracts with the most crimes committed. 
ggplot(pccc_sf) +
  geom_sf(aes(fill = OffenseCategory)) +
  labs(
    title = "Most Reported Category of Crime by Census Tract 
for the City of Portland",
    fill = "Crime Categories"
  ) +
  scale_fill_brewer(palette = "Set3")

# **ACS INCOME DATA NOTE REMEMBER TO MENTION THIS IS NOT DECENNIAL DATA**

or_income <- get_acs(geography = "tract", 
              variables = c(medincome = "B19013_001"), 
              state = "OR", 
              year = 2020)

or_income <- or_income |>
  pivot_wider(names_from = "variable", values_from = "estimate")

# **joining income data and census data by geoid**

census_within_portland_sf <-
  left_join(census_within_portland_sf, or_income) 

# Areal Map of Larceny Offenses per Census Tract

ggplot(pccc_sf) +
  geom_sf(aes(fill = n)) +
  labs(
    title = "Number of Larceny Offenses per Census Tract",
    fill = "Number of Larceny Offenses"
  ) +
  scale_fill_gradient2()

# creating neighbors for portland tracts 

# Create neigbors with na value dropped
pccc_nsf <- pccc_sf |> 
  filter(GEOID != "41051005701") |>
  filter(GEOID != "41051007300") |>
  filter(GEOID != "41051980000")

pccc_nb <- poly2nb(pccc_nsf, queen = FALSE) 
 # Create neighbor weights
pccc_nbw <- nb2listw(pccc_nb, style = "W", zero.policy = TRUE)

# **Moran's I for Larceny Offenses**

moran.mc(pccc_nsf$n, pccc_nbw, nsim = 499)

```{r, echo = FALSE, out.width= "100%"}
# SECTION 2
ggplot(census_within_portland_sf) +
  geom_sf(aes(fill = medincome)) +
  labs(
    title = "Median Income per Census Tract",
    fill = "Median Income (US Dollars)"
  ) +
  scale_fill_gradient2()

# **Moran's I for median income**
pccc_sf <- left_join(census_within_portland_sf, crime_tract_count2) |> 
  drop_na(n)

pccc_nsf <- pccc_sf |> 
  filter(GEOID != "41051005701") |>
  filter(GEOID != "41051007300") |>
  filter(GEOID != "41051980000")
moran.mc(pccc_nsf$medincome, pccc_nbw, nsim = 499)

## **Moran's after accounting for income

regression_ci <- left_join(pccc_nsf, or_income)

ggplot(regression_ci) + 
  geom_point(aes(medincome, n)) +
  geom_smooth(aes(medincome, n), method = lm, se = FALSE) +
  labs(
    title = "Median Income vs. Number of Larceny Offenses",
    x = "Median Income (US Dollars)",
    y = "Number of Larceny Offenses"
  )

sarlm1 <- lagsarlm(n ~ medincome, data = regression_ci, listw = pccc_nbw)
summary(sarlm1)

# tract point pattern 
ct106.2 <- census_within_portland_sf |> 
  filter(GEOID == "41051010602")

ct106.2_points <- st_filter(crime_sf, ct106.2)

larceny_points <- ct106.2_points |> 
  filter(OffenseCategory == "Larceny Offenses")

vandalism_points <- ct106.2_points |> 
  filter(OffenseCategory == "Vandalism")

arson_points <- ct106.2_points |> 
  filter(OffenseCategory == "Arson")

ggplot(ct106.2) +
  geom_sf() +
  annotation_map_tile() +
  geom_sf(data = larceny_points) +
  labs(
    title = "Census Tract 106.2 Larceny Offences",
  )

dt_larceny_ppp <- as.ppp(st_coordinates(larceny_points), W = ct106.2)
dt_vandalism_ppp <- as.ppp(st_coordinates(vandalism_points), W = ct106.2)
dt_arson_ppp <- as.ppp(st_coordinates(arson_points), W = ct106.2)

# **Turning the specific census track to a PPP object** all crimes in dt tract
ct106.2ppp <- as.ppp(st_coordinates(ct106.2_points), W = ct106.2)

plot(ct106.2ppp)

plot(density(ct106.2ppp), main = "Offenses per mile squared in Downtown")
points(ct106.2ppp, col = "white")

# **K Function Analysis for Larceny, Vandalism, and Arson PPP Objects**
dt_larceny_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot()

par(mfrow=c(1,2))
dt_vandalism_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot(main = "Inhomogeneous K for Larceny")

dt_arson_ppp |>
  envelope(Kinhom, nsim = 5) |>
  plot(main = "Inhomogeneous K for Arson")

par(mfrow=c(1,1))
dt_df <- st_coordinates(ct106.2_points) |> 
  bind_cols(as_tibble(ct106.2_points)) |> 
  dplyr::select(X, Y, OffenseCategory) |> 
  mutate(OffenseCategory = as.factor(OffenseCategory))

dt_ppp <- as.ppp(dt_df, W = ct106.2) 

plot(dt_ppp, main = "Downtown PPP")

dt_ppp |> envelope(Kcross.inhom, i = "Larceny Offenses", j = "Arson", nsim = 5) |> 
  plot(main = "Inhomogeneous K Cross Between Larceny and Arson Offences")

# **Demographics of Census Tracts**
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

 ggplot(pccc_sf) +
  geom_sf(aes(fill = white_per)) +
  labs(
    title = "Percentage of White Individuals per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
pccc_nb2 <- poly2nb(pccc_sf, queen = FALSE) 
 # Create neighbor weights
pccc_nbw2 <- nb2listw(pccc_nb2, style = "W", zero.policy = TRUE)
moran.mc(pccc_sf$white_per, pccc_nbw2, nsim = 499)

 ggplot(pccc_sf) +
  geom_sf(aes(fill = baa_per)) +
  labs(
    title = "Percentage of Black or African American Individuals
per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()

 ggplot(pccc_sf) +
  geom_sf(aes(fill = hisp_per)) +
  labs(
    title = "Percentage of Hispanic Individuals per 
Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()

 ggplot(pccc_sf) +
  geom_sf(aes(fill = asian_per)) +
  labs(
    title = "Percentage of Asian Individuals per Census Tract",
    fill = "Percentage"
  ) +
  scale_fill_gradient2()
