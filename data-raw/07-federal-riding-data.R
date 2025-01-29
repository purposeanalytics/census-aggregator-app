library(tidyverse)
library(arrow)

#dev parameters
overwrite_flag  <- TRUE


#parameters
username <- R.utils::System$getUsername()

#Files to read in
shapefile_folder <-paste("C:/Users/", username, "/Purpose Analytics/Data - Documents/Elections_Canada/Shapefiles 2023 Representation Order_2024-09-11/")

census_profile_folder <-  paste0( 'C:/Users/',username, '/Purpose Analytics/Data - Documents/Statistics_Canada/2021_Census_Profile_FED2023_98-401-X2021029_2025-01-28/')
census_profile_file <-  paste0( census_profile_folder, '98-401-X2021029_English_CSV_data.csv' )
census_metadata_file <- paste0(census_profile_folder,  '98-401-X2021029_English_meta.txt')


#These are generated in 03-vector-values.R
vectors_many <- readRDS(here::here("data-raw", "intermediary", "vectors_many.rds"))
vectors_few  <- readRDS(here::here("data-raw", "intermediary", "vectors_few.rds"))
language_at_home_vector <- readRDS(here::here("data-raw", "intermediary", "language_at_home_vectors.rds"))
ethnic_cultural_origin_vector  <- readRDS(here::here( "data-raw", "intermediary", "ethnic_cultural_origin_vectors.rds" ))

all_desired_vectors <- bind_rows(vectors_many , vectors_few) |>  mutate(vector_number = as.numeric(str_remove(vector, "v_CA21_"))) |> arrange(vector_number)

#Read in Census Profile. Location on your system may vary.
census_profile_raw <- readr::read_csv(census_profile_file )

#Prepare Census Metadata
census_metadata_raw <-   readr::read_lines(census_metadata_file )

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
  mutate(characteristic = str_replace_all(characteristic,  "\\s*\\(\\d+\\)", ""))

####################
#Align to cancensus CA21 vector numbers

aligned_vector_numbers <- readxl::read_excel( paste0( 'C:/Users/', R.utils::System$getUsername(), '/Purpose Analytics/Data - Documents/Statistics_Canada/2021_Census_Profile_FED2023_98-401-X2021029_2025-01-28/Metadata_Characteristic_Number_to_cancensus_vector_number_alignment_selected.xlsx')) |>
  select(metadata_row, metadata_name, vector_number) |>
  mutate(metadata_name  = str_replace_all(metadata_name, "\\s*\\(\\d+\\)","")) |>
  mutate(metadata_name = str_remove(metadata_name, ' - 100% data')) |>
  mutate(metadata_name = str_remove(metadata_name, ' - 25% sample data')) |>
  mutate(metadata_name = str_remove(metadata_name, '^Total - ')) |>
  mutate(metadata_name = str_trim(metadata_name)) |>
  filter(!is.na(vector_number))


#TO CHECK FOR MISALIGNMENTS
if(FALSE){
all_desired_vectors |>  left_join(aligned_vector_numbers) |>
  select(label, metadata_name) |>
  mutate(label = str_remove(label, '^Total - ')) |>
  mutate(label = str_remove(label, ' - 100% data')) |>
  filter(!label == metadata_name)
}

alignment_numbers_only <- aligned_vector_numbers |>  select(metadata_row, vector_number)



metadata_to_keep <- census_metadata |>
  left_join(aligned_vector_numbers, join_by( vector_number_metadata == metadata_row)) |>
  filter(!is.na(vector_number)) |>
  select(vector_number_metadata) |>  pull()

#### census profile keep parts
census_profile <- census_profile_raw |>
  filter(CHARACTERISTIC_ID %in% metadata_to_keep)   |>
  filter(GEO_LEVEL =='Federal electoral district (2023 Representation Order)') |>
  left_join(alignment_numbers_only , join_by(CHARACTERISTIC_ID == metadata_row)) |>
  left_join(all_desired_vectors) |>
  mutate(value = if_else(type =='Total', C1_COUNT_TOTAL,NA)
  ) |>
  filter(!is.na(value))





#######vector values
#SEE: arrow::read_parquet('inst/extdata/ct_values/id=00/part-0.parquet')
census_profile |>
  select(DGUID, vector, value) |>
  rename(geo_uid = DGUID) |>
  mutate(id =  str_sub(geo_uid, 11,13)) |>
  group_by(id) |>
  write_dataset(here::here("inst", "extdata", "riding2023_values"))

#####################


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
  mutate( pr_uid = str_sub(geo_uid, 10,11),
          area_sq_km  =NA_real_,
          population_density = NA_real_) |>
  select(any_of(c("geo_uid"  ,
                  "pr_uid",
                  "population",
                  "households",
                  "area_sq_km",
                  "population_density"
  ) ) ) |>  left_join(population_households)

class(riding2023) <- 'data.frame'
usethis::use_data(riding2023, overwrite = overwrite_flag)


#TODO - areas_sq_km and population_density

################################
#Riding Population density quintitles
#TODO


#usethis::use_data(riding2023_population_density_quintiles)
#usethis::use_data(riding2023_quantiles_text)
#SEE: load('data/csd_quantiles_text.rda')
#     load('data/csd_population_density_quantiles.rda')
###############a


###################
#TODO
# read in shapefiles

shapefile_folder









#TODO simplify shapes

#csd_size <- object.size(csd)
#csd <- csd %>%
# split(.$geo_uid) %>%
# map_dfr(function(feature) {
# pts <- npts(feature)
#
#   if (pts > 10000) {
#      ms_simplify(feature, keep = 0.1, keep_shapes = TRUE)
#    } else if (pts > 5000) {
#      ms_simplify(feature, keep = 0.3, keep_shapes = TRUE)
#    } else if (pts > 500) {
#      ms_simplify(feature, keep = 0.5, keep_shapes = TRUE)
#    } else {
#      feature
#    }
#  })


#make valid
#st_make_valid()

#csd <- csd %>%
#  st_make_valid()

# Size after:
#csd_simplified_size <- object.size(csd)

#as.numeric(csd_simplified_size) / as.numeric(csd_size)





####################



############### Arrow file for geograpy in app
#TODO
#ct_geometry %>%
#  group_by(cd_uid) %>%
#  write_sf_dataset(dir,
#                   format = "parquet",
#                   hive_style = FALSE
 # )
#

#Simple feature collection with 6100 features and 2 fields
#Geometry type: MULTIPOLYGON
#Dimension:     XY
#Bounding box:  xmin: -124.6993 ymin: 41.90973 xmax: -52.61938 ymax: 60.00006
#Geodetic CRS:  WGS 84
#First 10 features:
#    geo_uid cd_uid                       geometry
#1  0010001.00   1001 MULTIPOLYGON (((-52.7279 47...
#2  0010002.00   1001 MULTIPOLYGON (((-52.7182 47...
#3  0010003.01   1001 MULTIPOLYGON (((-52.73801 4...
#4  0010003.02   1001 MULTIPOLYGON (((-52.74572 4.

######################
#And to upload to mapbox:
# TODO
#ct_upload <- ct_upload %>%
# select(-cd_uid)
#
#pload_tiles(
# input = ct_upload,
# username = "purposeanalytics",
# tileset_id = "2021_ct",
# tileset_name = "2021_census_ct",
# multipart = TRUE
#

##########






