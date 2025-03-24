library(tidyverse)
library(arrow)
library(sf)
library(sfarrow)
library(mapview)
library(rmapshaper)
library(mapboxapi)
library(cancensus)


#dev parameters
overwrite_flag  <- TRUE
re_run_map_procedure <- FALSE

#parameters
username <- R.utils::System$getUsername()

#Files to read in
if(re_run_map_procedure){

canada_sf <- get_census(dataset="CA21", regions = list(C = "1"), level="CSD", geo_format = "sf") |>
  select(-everything()) |>
  summarize() |>  st_make_valid()


federal_ridings_sf <- read_sf(paste0(Sys.getenv("USERPROFILE"), r"(\Purpose Analytics\Data - Documents\Elections_Canada\Shapefiles 2023 Representation Order_2024-09-11\FED_CA_2023_EN.shp)"))

federal_ridings_clipped_sf <- federal_ridings_sf |>
  st_transform(4326) |>
  st_intersection(canada_sf) |>
  mutate(area_sq_km = st_area(geometry) |> units::set_units("km^2") |> as.numeric())

rm(canada_sf)
rm(federal_ridings_sf)

saveRDS(federal_ridings_clipped_sf, here::here("data-raw/intermediary/", "federal_ridings_clipped_sf.rds"))
} else{
federal_ridings_clipped_sf <- readRDS(here::here("data-raw/intermediary/federal_ridings_clipped_sf.rds"))
}


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



all_desired_vectors <- censusaggregatorapp::vectors |>   mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_")))


#Read in Census Profile. Location on your system may vary.
census_profile_raw <- readr::read_csv(census_profile_file)


##Fix encoding issues with riding names
riding_names <- census_profile_raw |>  select(DGUID,GEO_NAME)|>  distinct()
riding_names$GEO_NAME <- iconv(riding_names$GEO_NAME, from = "latin1", to = "UTF-8")
riding_names$GEO_NAME <- gsub("\u0092", "'", riding_names$GEO_NAME)
riding_names <- riding_names |>  mutate(GEO_NAME = str_replace_all(GEO_NAME, "--", "—"))

census_profile_raw <- census_profile_raw |>  select(-GEO_NAME) |>  left_join(riding_names )  |>
  select(CENSUS_YEAR, DGUID,ALT_GEO_CODE ,GEO_LEVEL , GEO_NAME, everything())



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
if(FALSE){
#from prepare_data.R
mvi <-   missing_vector_info |>  separate_wider_delim(cols = vector, delim = "_", names = c("prefix", "census_num", "vec_num"), cols_remove = FALSE)

ct_csd_vectors <- bind_rows(
readRDS("data-raw/intermediary/vectors_few.rds"),
readRDS("data-raw/intermediary/vectors_many.rds")
) |>  separate_wider_delim(cols = vector, delim = "_", names = c("prefix", "census_num", "vec_num"), cols_remove = FALSE )


ct_csd_vectors |> filter(vector %in% mvi$vector)

federal_ridings_clipped_sf |>  st_drop_geometry()

}




alignment_numbers_only <- aligned_vector_numbers |>  select(metadata_row, vector_number)



metadata_to_keep <- census_metadata |>
  left_join(aligned_vector_numbers,
            join_by(vector_number_metadata == metadata_row)) |>
  filter(!is.na(vector_number)) |>
  select(vector_number_metadata) |>  pull()

#### census profile keep parts
census_profile <-
  census_profile_raw |>
  filter(CHARACTERISTIC_ID %in% metadata_to_keep)   |>
  filter(GEO_LEVEL == 'Federal electoral district (2023 Representation Order)') |>
  left_join(alignment_numbers_only ,
            join_by(CHARACTERISTIC_ID == metadata_row)) |>
  left_join(all_desired_vectors)




#########TO ADD in the 2021 Population
add_in_population_2021 <- census_profile |>
                           filter(CHARACTERISTIC_ID %in% c(1,8)) |>
                           group_by(DGUID) |>
                           arrange(DGUID, desc(CHARACTERISTIC_ID)) |>
                           fill(C1_COUNT_TOTAL, .direction = 'down') |> ungroup()

census_profile <- census_profile |>  filter(!CHARACTERISTIC_ID %in% c(1,8))

census_profile <- bind_rows(census_profile, add_in_population_2021) |>
  mutate(value = if_else(type == 'Total'| CHARACTERISTIC_ID ==29  , C1_COUNT_TOTAL, NA))



### add in land area
riding_areas <- federal_ridings_clipped_sf |>  st_drop_geometry() |> select(FED_NUM, SHAPE_AREA) |>
  mutate(land_area= SHAPE_AREA/1e6) |>
  select(FED_NUM, land_area) |>
  mutate(FED_NUM = as.character(FED_NUM))


land_area_rows <- census_profile |> filter( CHARACTERISTIC_ID  ==7) |>
  left_join(riding_areas, join_by(ALT_GEO_CODE == FED_NUM)) |>
  select(-value) |>
  rename(value = land_area )


census_profile <- census_profile |>  filter(!CHARACTERISTIC_ID ==7)

census_profile <- bind_rows(census_profile, land_area_rows)



### add in population density
pop_density <-  census_profile |> filter(CHARACTERISTIC_ID %in%c(7,1)) |>
   select(DGUID, CHARACTERISTIC_ID, value) |>
   pivot_wider(names_from = CHARACTERISTIC_ID,  values_from = value) |>
   mutate(pop_density = `1`/`7`) |>  select(DGUID, pop_density)

census_profile <- census_profile |>  left_join(pop_density) |>
  mutate(value = if_else(CHARACTERISTIC_ID ==6, pop_density, value)) |>
  select(-pop_density)

#### Add in grouped age cohort vectors
#generated in 01-vectors.R
age_cohort_vectors <- readRDS( here::here("data-raw", "intermediary", "age_cohort_vectors.rds")) |>
 mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_")))

age_cohort_data <- census_profile  |>  filter(vector_number %in% age_cohort_vectors$vector_number) |>
  select(CHARACTERISTIC_ID, CHARACTERISTIC_NAME, value, vector_number, DGUID, details) |>
  left_join(age_cohort_vectors) |>
  group_by(DGUID,group) |>
  summarise(value = sum(value, na.rm = TRUE),
            details = paste(label, collapse = ';')
            ) |>  ungroup() |>
  mutate(details = paste('CA 2021 Census; 100% data; Total - Age;', details)) |>
  rename(vector = group)


age_cohort_profile_form <- census_profile  |>  filter(vector_number %in% age_cohort_vectors$vector_number) |>
  select(CENSUS_YEAR, DGUID, GEO_LEVEL, GEO_NAME, highest_parent_vector, type, units, aggregation, label_short,vector_number) |>  filter(vector_number %in% age_cohort_vectors$vector_number) |>
  select(-vector_number) |>
  distinct() |>
  filter(!is.na(highest_parent_vector)) |>
  mutate(label_short = 'age_cohort') |>  left_join(age_cohort_data)

census_profile <- census_profile  |> bind_rows(age_cohort_profile_form)
####
### add in income buckets

income_vectors_grouped<- readRDS('data-raw/intermediary/income_vectors_grouped.rds') |>
  mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_")))


income_buckets_profile_form <- census_profile  |>  filter(vector_number %in% income_vectors_grouped$vector_number) |>
  select(CHARACTERISTIC_ID, CHARACTERISTIC_NAME, value, vector_number, DGUID,label, details) |>
  left_join(income_vectors_grouped) |>
  group_by(DGUID,new_vector) |>
  summarise(value = sum(value, na.rm = TRUE),
            details = paste(label, collapse = ';')
  ) |>  ungroup() |>
  mutate(details = paste('CA 2021 Census; 100% data; Income; Household income; Household total income groups in 2020 for private households;', details)) |>
  rename(vector = new_vector) |>
  mutate(label_short = 'income_buckets')

census_profile <- census_profile  |> bind_rows(income_buckets_profile_form)


#################
###educational attainment

educational_attainment_vectors <- readRDS(here::here("data-raw", "intermediary", "educational_attainment_vectors_grouped.rds")) |>
  mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_")))


educational_attainment_profile_form <- census_profile  |>  filter(vector_number %in% educational_attainment_vectors$vector_number) |>
  mutate(value = C1_COUNT_TOTAL) |>
  select(CHARACTERISTIC_ID, CHARACTERISTIC_NAME, value, vector_number, DGUID,label, details) |>
  left_join(educational_attainment_vectors) |>
  group_by(DGUID,new_vector) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') |>
  rename(vector = new_vector) |>
  mutate(label_short= 'educational_attainment')

census_profile <- census_profile |> filter(!vector_number %in% educational_attainment_vectors$vector_number)

census_profile <- census_profile  |> bind_rows(educational_attainment_profile_form)


##################################
##################################

rm(census_profile_raw)


#######vector values
#SEE: arrow::read_parquet('inst/extdata/ct_values/id=00/part-0.parquet')
fs::dir_delete("inst/extdata/ridings_values/")
census_profile |>
  select(DGUID, vector, value) |>
  rename(geo_uid = DGUID) |>
  mutate(id =  str_sub(geo_uid, 11, 13)) |>
  group_by(id) |>
  write_dataset(here::here("inst", "extdata", "ridings_values"))

#####################


###################
# read in shapefiless
#SEE: load('data-raw/intermediary/csd_before_simplify.rds')


geo_names_from_census_profile <- census_profile |>  select(DGUID, ALT_GEO_CODE) |>  distinct() |>
  mutate(ALT_GEO_CODE = as.numeric(ALT_GEO_CODE))

fed2023_unsimplified <- federal_ridings_clipped_sf |>
  rename(geo_uid  = FED_NUM, region_name = ED_NAMEE) |>
  mutate(pr_uid = str_sub(geo_uid, 1, 2)) |>
  left_join(geo_names_from_census_profile,
            join_by(geo_uid == ALT_GEO_CODE)) |>
  select(-geo_uid) |>
  rename(geo_uid  = DGUID) |>
  select(-SHAPE_AREA, -REP_ORDER, -SHAPE_LEN, -ED_NAMEF)


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

##STILL NEED THESE
setdiff(needed_columns, names(fed2023_unsimplified))

# THIS MAY BE NEEDED IF ANY OF THE RIDINGS END UP AS TYPE GEOMETRYCOLLECTION
#
# ridings_geometry_types <- map(fed2023$geometry,function(x){
#   sf::st_geometry_type(x)
# })
#
#
# ridings_geom_type_df <- tibble(geo_type =c(unlist(ridings_geometry_types))) |>
#   rowid_to_column() |>
#   filter(geo_type =='GEOMETRYCOLLECTION')
#
#
# geom_collection_rows <- ridings_intersected |>
#   slice(ridings_geom_type_df$rowid)
#
#
# # Loop through each feature in the collection
# for (i in seq(1, nrow(geom_collection_rows))) {
#   print(i)
#   # Extract the geometry
#   geom <- st_geometry(geom_collection_rows |>  slice(i) )
#
#   # Check if the geometry is a GEOMETRYCOLLECTION
#   if (st_geometry_type(geom[[1]]) == "GEOMETRYCOLLECTION") {
#     # Extract the individual geometries
#     components <- st_collection_extract(geom[[1]], "POLYGON")
#
#     # Combine the polygons into a MULTIPOLYGON
#     geom_multipolygons <- st_multipolygon((components))
#
#     # Add the MULTIPOLYGON to the list
#     geom_collection_rows$geometry[[i]] <- geom_multipolygons
#   }
# }
#
#
# walk(geom_collection_rows$geo_uid,
#     function(x){
# ridings_intersected <<- ridings_intersected |>
#   mutate(geometry = if_else(geo_uid ==x, geom_collection_rows |>  filter(geo_uid ==x) |>  select(geometry) |>  pull(), geometry))
#       }
# )
#rm(geom_collection_rows)

#########################simplify shapes
fed_size <- object.size(fed2023_unsimplified)


fed2023_unsimplified <- fed2023_unsimplified |>
  mutate(prov = str_sub(geo_uid, 10,11)) |>
  mutate(prov_group = case_when(
                              prov %in% c('10', '24') ~ 'qc_nl',
                              prov == '11' ~ 'pe',
                              prov %in% c('12', '13') ~'ns_nb',
                              prov == '35' ~ 'on',
                              prov =='46' ~'mb',
                              prov =='47' ~ 'sk',
                              prov =='48' ~ 'ab',
                              prov =='59' ~'bc',
                              prov %in% c('61', '62', '60') ~'terr',
                              .default = NA)
  )



fed2023_split <-  fed2023_unsimplified %>% split(.$prov_group)

map(fed2023_split, function(feature) {
  pts <- npts(feature)
  rlog::log_info(paste("Processing", unique(feature$prov_group), "Num points", pts))
  pts
})


fed2023 <-  map(fed2023_split, function(feature) {
    pts <- npts(feature)
    rlog::log_info(paste("Processing", unique(feature$prov_group), "Num points", pts))

    if(pts > 1.7e6){
      ms_simplify(feature , keep = .10, keep_shapes = TRUE)
    } else if(pts > 1000000){
      ms_simplify(feature , keep = .15, keep_shapes = TRUE)
    } else if(pts > 450000){
      ms_simplify(feature , keep = .4, keep_shapes = TRUE)
    } else if (pts > 200000){
      ms_simplify(feature , keep = .6, keep_shapes = TRUE)
    }else {
     feature }
}
) |> list_rbind() |>  sf::st_as_sf()


#make valid
fed2023  <- fed2023 %>%
  st_make_valid()

#rm(fed2023_unsimplified)

# Size after:
fed2023_simplified_size <- object.size(fed2023);fed2023_simplified_size

as.numeric(fed2023_simplified_size) / as.numeric(fed_size)


################
###Making the riding2023 file for 'data'
population_households <- census_profile |>
  filter(CHARACTERISTIC_ID %in% c('8', '100')) |>
  select(DGUID, CHARACTERISTIC_ID, C1_COUNT_TOTAL) |>
  pivot_wider(names_from = CHARACTERISTIC_ID, values_from = C1_COUNT_TOTAL) |>
  rename('population' = '8',
         'households' =  '100',
         'geo_uid' = 'DGUID')


land_area_of_ridings <- fed2023 |>  st_drop_geometry() |> select(geo_uid, area_sq_km)


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
ridings <- riding2023
rm(census_profile)

fed2023 <- fed2023  |> left_join(ridings)

################################
#Riding Population density quintitles

 load('data/csd_quantiles_text.rda')
 load('data/csd_population_density_quantiles.rda')
 ridings_population_density_quintiles <- csd_population_density_quantiles
 ridings_quantiles_text <- csd_quantiles_text
usethis::use_data(ridings_population_density_quintiles, overwrite = TRUE)
usethis::use_data(ridings_quantiles_text, overwrite = TRUE)
rm(csd_population_density_quantiles)
rm(csd_quantiles_text)
###############a



############### Arrow file for geograpy in app
# Write arrow dataset, partitioned by province, for getting geometry / boundary export in app
fed_geometry <- fed2023 %>%
  select(geo_uid, pr_uid)

dir <- "inst/extdata/ridings/"
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
  st_transform(3857) |>
  select(-prov, -prov_group)


#test_upload <- fed2023_upload |>  filter(prov_group =='pe') |>select(-prov, -prov_group)


# Upload
if(FALSE){
  usethis::edit_r_environ(scope = 'project')
  readRenviron('.Renviron')
access_token <-   Sys.getenv('MAPBOX_SECRET_TOKEN')

upload_tiles(
  input = fed2023_upload,
  access_token = access_token,
  username = "purposeanalytics",
  tileset_id = "2021_ridings",
  tileset_name = "2021_census_ridings",
  multipart=TRUE
)

}

riding <- fed2023 %>%
  st_set_geometry(NULL) %>%
  select(all_of(c("geo_uid",            "population",         "households",         "area_sq_km",         "population_density")))

usethis::use_data(ridings, overwrite = TRUE)


if(FALSE){
 sf::st_write(fed2023, dsn = "../ridingmap/Federal_Ridings.gpkg", layer = 'simplified_fed_ridings')

 #sf::st_write(fed2023, "../ridingmap/Federal_Ridings.shp")
 #sf::st_write(fed2023_unsimplified, "../ridingmap/Federal_Ridings.shp")

}


