library(tidyverse)
library(arrow)
library(sf)
library(sfarrow)
library(mapview)
library(rmapshaper)
library(mapboxapi)


#dev parameters
overwrite_flag  <- TRUE


#parameters
username <- R.utils::System$getUsername()

#Files to read in
shapefile_folder <- paste0(
  "C:/Users/",
  username,
  "/Purpose Analytics/Data - Documents/Elections_Canada/Shapefiles 2023 Representation Order_2024-09-11/"
)

census_profile_folder <-  paste0(
  'C:/Users/',
  username,
  '/Purpose Analytics/Data - Documents/Statistics_Canada/2021_Census_Profile_FED2023_98-401-X2021029_2025-01-28/'
)
census_profile_file <-  paste0(census_profile_folder,
                               '98-401-X2021029_English_CSV_data.csv')
census_metadata_file <- paste0(census_profile_folder, '98-401-X2021029_English_meta.txt')

aligned_vectors_file <-  paste0(
  'C:/Users/',
  username,
  '/Purpose Analytics/Data - Documents/Statistics_Canada/2021_Census_Profile_FED2023_98-401-X2021029_2025-01-28/Metadata_Characteristic_Number_to_cancensus_vector_number_alignment_selected.xlsx'
)


#These are generated in 03-vector-values.R
vectors_many <- readRDS(here::here("data-raw", "intermediary", "vectors_many.rds"))
vectors_few  <- readRDS(here::here("data-raw", "intermediary", "vectors_few.rds"))
language_at_home_vector <- readRDS(here::here("data-raw", "intermediary", "language_at_home_vectors.rds"))
ethnic_cultural_origin_vector  <- readRDS(here::here(
  "data-raw",
  "intermediary",
  "ethnic_cultural_origin_vectors.rds"
))

all_desired_vectors <- bind_rows(vectors_many , vectors_few) |>  mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_"))) |> arrange(vector_number)

#Read in Census Profile. Location on your system may vary.
census_profile_raw <- readr::read_csv(census_profile_file)

#Prepare Census Metadata
census_metadata_raw <-   readr::read_lines(census_metadata_file)

cm_first_row <- which(census_metadata_raw == 'Member')

census_metadata <- census_metadata_raw[cm_first_row + 1:length(census_metadata_raw)]     |> as_tibble() |>
  separate_wider_delim(
    cols = value,
    delim = '\t',
    names = c('vector_number_metadata', 'characteristic'),
    too_few = 'align_start'
  )

ftn_row <- which(census_metadata$vector_number_metadata == "Footnotes")

census_metadata <- census_metadata |>  slice(1:ftn_row - 1) |>  filter(!is.na(characteristic))  |>
  mutate(characteristic = str_replace_all(characteristic, '\xa0', ' ') |>  str_trim()) |>
  mutate(vector_number_metadata = as.numeric(vector_number_metadata)) |>
  mutate(characteristic = str_replace_all(characteristic, "\\s*\\(\\d+\\)", ""))

####################
#Align to cancensus CA21 vector numbers

aligned_vector_numbers <- readxl::read_excel(aligned_vectors_file) |>
  select(metadata_row, metadata_name, vector_number) |>
  mutate(metadata_name  = str_replace_all(metadata_name, "\\s*\\(\\d+\\)", "")) |>
  mutate(metadata_name = str_remove(metadata_name, ' - 100% data')) |>
  mutate(metadata_name = str_remove(metadata_name, ' - 25% sample data')) |>
  mutate(metadata_name = str_remove(metadata_name, '^Total - ')) |>
  mutate(metadata_name = str_trim(metadata_name)) |>
  filter(!is.na(vector_number))


#TO CHECK FOR MISALIGNMENTS
if (FALSE) {
  full_join_em <-  all_desired_vectors |>  full_join(aligned_vector_numbers)

  full_join_em |>  filter(is.na(vector))
  full_join_em |>  filter(is.na(metadata_row))




  all_desired_vectors |>  left_join(aligned_vector_numbers) |>
    select(label, metadata_name) |>
    mutate(label = str_remove(label, '^Total - ')) |>
    mutate(label = str_remove(label, ' - 100% data')) |>
    filter(!label == metadata_name)
}

alignment_numbers_only <- aligned_vector_numbers |>  select(metadata_row, vector_number)



metadata_to_keep <- census_metadata |>
  left_join(aligned_vector_numbers,
            join_by(vector_number_metadata == metadata_row)) |>
  filter(!is.na(vector_number)) |>
  select(vector_number_metadata) |>  pull()

#### census profile keep parts
census_profile <- census_profile_raw |>
  filter(CHARACTERISTIC_ID %in% metadata_to_keep)   |>
  filter(GEO_LEVEL == 'Federal electoral district (2023 Representation Order)') |>
  left_join(alignment_numbers_only ,
            join_by(CHARACTERISTIC_ID == metadata_row)) |>
  left_join(all_desired_vectors) |>
  mutate(value = if_else(type == 'Total', C1_COUNT_TOTAL, NA)) |>
  filter(!is.na(value))


rm(census_profile_raw)


#######vector values
#SEE: arrow::read_parquet('inst/extdata/ct_values/id=00/part-0.parquet')
census_profile |>
  select(DGUID, vector, value) |>
  rename(geo_uid = DGUID) |>
  mutate(id =  str_sub(geo_uid, 11, 13)) |>
  group_by(id) |>
  write_dataset(here::here("inst", "extdata", "riding2023_values"))

#####################


###################
# read in shapefiless
#SEE: load('data-raw/intermediary/csd_before_simplify.rds')


geo_names_from_census_profile <- census_profile |>  select(DGUID, ALT_GEO_CODE) |>  distinct() |>
  mutate(ALT_GEO_CODE = as.numeric(ALT_GEO_CODE))

fed2023 <- sf::read_sf(paste0(shapefile_folder, "/FED_CA_2023_EN.shp")) |>
  rename(geo_uid  = FED_NUM, region_name = ED_NAMEE) |>
  mutate(pr_uid = str_sub(geo_uid, 1, 2)) |>
  left_join(geo_names_from_census_profile,
            join_by(geo_uid == ALT_GEO_CODE)) |>
  select(-geo_uid) |>
  rename(geo_uid  = DGUID)


needed_columns <- c(
  "geo_uid",
  "pr_uid",
  "region_name",
  "population",
  "households",
  "area_sq_km",
  "population_density",
  "geometry"
)


#Calculate areas

canada_sf <- sf::read_sf('C:/Users/DanielSimeone/Purpose Analytics/Data - Documents/Statistics_Canada/2021_Census_Boundary_Province_lpr_000b21a_20250206/lpr_000b21a_e.shp')

canada_sf_summarized <- summarize(canada_sf)
rm(canada_sf)

ridings_intersected <- fed2023|> st_intersection(canada_sf_summarized)

rm(canada_sf_summarized)

ridings_geometry_types <- map(ridings_intersected$geometry,function(x){
  sf::st_geometry_type(x)
})


ridings_geom_type_df <- tibble(geo_type =c(unlist(ridings_geometry_types))) |>
  rowid_to_column() |>
  filter(geo_type =='GEOMETRYCOLLECTION')


geom_collection_rows <- ridings_intersected |>
  slice(ridings_geom_type_df$rowid)


# Loop through each feature in the collection
for (i in seq(1, nrow(geom_collection_rows))) {
  print(i)
  # Extract the geometry
  geom <- st_geometry(geom_collection_rows |>  slice(i) )

  # Check if the geometry is a GEOMETRYCOLLECTION
  if (st_geometry_type(geom[[1]]) == "GEOMETRYCOLLECTION") {
    # Extract the individual geometries
    components <- st_collection_extract(geom[[1]], "POLYGON")

    # Combine the polygons into a MULTIPOLYGON
    geom_multipolygons <- st_multipolygon((components))

    # Add the MULTIPOLYGON to the list
    geom_collection_rows$geometry[[i]] <- geom_multipolygons
  }
}


walk(geom_collection_rows$geo_uid,
    function(x){
ridings_intersected <<- ridings_intersected |>
  mutate(geometry = if_else(geo_uid ==x, geom_collection_rows |>  filter(geo_uid ==x) |>  select(geometry) |>  pull(), geometry))
      }
)

#########################simplify shapes
fed_size <- object.size(ridings_intersected)

fed2023 <- ridings_intersected %>%
  split(.$geo_uid) %>%
  map_dfr(function(feature) {
    pts <- npts(feature)

    if (pts > 10000) {
      ms_simplify(feature, keep = 0.1, keep_shapes = TRUE)
    } else if (pts > 5000) {
      ms_simplify(feature, keep = 0.3, keep_shapes = TRUE)
    } else if (pts > 500) {
      ms_simplify(feature, keep = 0.5, keep_shapes = TRUE)
    } else {
      feature
    }
  })

rm(geom_collection_rows)
rm(ridings_intersected)
#make valid

fed2023  <- fed2023 %>%
  st_make_valid()

# Size after:
fed2023_simplified_size <- object.size(fed2023)

as.numeric(fed2023_simplified_size) / as.numeric(fed_size)


fed2023 <- fed2023 |>
  mutate(SHAPE_AREA = sf::st_area(geometry,))




land_area_of_ridings <- tibble(geo_uid =  fed2023$geo_uid,
                               riding_name = fed2023$region_name,
                               area  = fed2023$SHAPE_AREA) |>
                                               mutate(area_sq_km = as.numeric(area/1000000)) |>
  select(geo_uid, area_sq_km)






################
###Making the riding2023 file for 'data'
population_households <- census_profile |>
  filter(CHARACTERISTIC_ID %in% c('8', '100')) |>
  select(DGUID, CHARACTERISTIC_ID, C1_COUNT_TOTAL) |>
  pivot_wider(names_from = CHARACTERISTIC_ID, values_from = C1_COUNT_TOTAL) |>
  rename('population' = '8',
         'households' =  '100',
         'geo_uid' = 'DGUID')



riding2023 <- census_profile |>  select(DGUID, GEO_NAME) |>  distinct() |>
  rename('geo_uid' = 'DGUID') |>
  left_join(land_area_of_ridings) |>
  mutate(
    pr_uid = str_sub(geo_uid, 10, 11)
  ) |>
  select(any_of(
    c(
      "geo_uid"  ,
      "pr_uid",
      "population",
      "households",
      "area_sq_km",
      "population_density"
    )
  )) |>  left_join(population_households) |>
  mutate(population_density = population/area_sq_km)

class(riding2023) <- 'data.frame'
usethis::use_data(riding2023, overwrite = overwrite_flag)
rm(census_profile)

fed2023 <- fed2023  |> left_join(riding2023)

################################
#Riding Population density quintitles

 load('data/csd_quantiles_text.rda')
 load('data/csd_population_density_quantiles.rda')
 riding2023_population_density_quintiles <- csd_population_density_quantiles
 riding2023_quantiles_text <- csd_quantiles_text
usethis::use_data(riding2023_population_density_quintiles)
usethis::use_data(riding2023_quantiles_text)
rm(csd_population_density_quantiles)
rm(csd_quantiles_text)
###############a



############### Arrow file for geograpy in app
# Write arrow dataset, partitioned by province, for getting geometry / boundary export in app
fed_geometry <- fed2023 %>%
  select(geo_uid, pr_uid)

dir <- "inst/extdata/riding2023_values/"
if (dir.exists(dir)) {
  fs::dir_delete(dir)
}
fs::dir_create(dir)


fed_geometry %>%
  group_by(pr_uid) %>%
  write_sf_dataset(dir, format = "parquet", hive_style = FALSE)

# Create formatted version of values, round original population density
fed2023_upload <- fed2023 %>%
  mutate(
    across(c(population, households), .fns = list(fmt = scales::comma)),
    across(
      c(area_sq_km, population_density),
      .fns = list(fmt = ~ scales::comma(.x, accuracy = 0.1))
    ),
    population_density = round(population_density, digits = 1)
  )

# Remove original values (except population density)
fed2023_upload <- fed2023_upload %>%
  select(-population, -households, -area_sq_km)

# Now to upload to mapbox

fed2023_upload <- fed2023_upload %>%
  select(-pr_uid)

# Optimizing as per recommendations in https://docs.mapbox.com/help/troubleshooting/uploads/#troubleshooting
# Since the processing takes >1 hour, it times out

# Reproject to Web Mercator (EPSG:3857)
# If not in this format, then Mapbox will reproject on upload, which takes time and can contribute to timing out

fed2023_upload <- fed2023_upload %>%
  st_transform(3857)


# Upload
if(FALSE){
upload_tiles(
  input = fed2023_upload,
  username = "purposeanalytics",
  tileset_id = "2023_ridings",
  tileset_name = "2023_census_csd",
  multipart = TRUE
)
}

riding2023 <- fed2023 %>%
  st_set_geometry(NULL) %>%
  select(all_of(c("geo_uid",            "population",         "households",         "area_sq_km",         "population_density")))

usethis::use_data(riding2023, overwrite = TRUE)

