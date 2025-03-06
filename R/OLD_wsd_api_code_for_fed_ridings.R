if(FALSE){


  #############################################################
  ################ Federal 2023 Rep Order Values
  #####################################################
  fed_version <- '2.0'

  if(FALSE){

    #NEW ONE
    metadata <- get_statcan_wds_metadata("2021","FED",version=fed_version)
    saveRDS(metadata,  here::here("data-raw", "intermediary", "metadata.rds"))

    #OLD ONE
    metadata <- readRDS(here::here("data-raw", "intermediary", "metadata.rds"))
  }

  characteristics <- metadata |> filter(`Codelist en`=="Characteristic") |>
    select(where(~ !all(is.na(.)))) |>
    select(ID, en, `Parent ID`) |>
    rename(CHARACTERISTIC_NAME = en,
           CHARACTERISTIC_PARENT_ID =`Parent ID` ) |>
    mutate(CHARACTERISTIC_NAME = str_remove(CHARACTERISTIC_NAME, "^Total - "))

  topics <- metadata |> filter(`Codelist en`=="Topic") |>
    select(where(~ !all(is.na(.)))) |>
    select(ID, en) |>
    rename(TOPIC_NAME = en)


  dguids <- metadata |> filter(`Codelist ID`=="CL_GEO_FED") |> pull(ID)

  if(FALSE){
    ##NEW ONE
    all_wds_data_for_federal_districts_raw <- get_statcan_wds_data(DGUIDs= dguids, version = fed_version, language = 'en', refresh = FALSE)
    saveRDS(all_wds_data_for_federal_districts_raw, here::here("data-raw", "intermediary", "all_wds_data_for_federal_districts.rds"))


    #OLD ONE
    all_wds_data_for_federal_districts_raw <-readRDS( here::here("data-raw", "intermediary", "all_wds_data_for_federal_districts.rds"))
  }


  ##Vectors to keep
  #Ethnic/cultural origins and language at home removed
  vectors_fed_to_keep <- bind_rows(vectors_few, vectors_many) |>
    dplyr::filter(!label_short %in% c("ethnic_cultural_origin", "language_at_home"))



  vector_relationships <- vectors_fed_to_keep |>  select(highest_parent_vector, parent_vector, vector)

  root_vectors <- vector_relationships |>  filter(is.na(parent_vector)) |>  select(vector) |>  rename(root_vector = vector)
  leaf_vectors <- vector_relationships |>  filter(!is.na(parent_vector))


  vector_tree <- root_vectors |>
    left_join(leaf_vectors |>  select(parent_vector, vector), join_by(root_vector == parent_vector)) |>  rename(second_level_vector  = vector) |>
    left_join(leaf_vectors |>  select(parent_vector, vector), join_by(second_level_vector == parent_vector)) |>  rename(third_level_vector  = vector) |>
    left_join(leaf_vectors |>  select(parent_vector, vector), join_by(third_level_vector == parent_vector)) |>  rename(fourth_level_vector  = vector)  |>
    left_join(leaf_vectors |>  select(parent_vector, vector), join_by(fourth_level_vector == parent_vector)) |>  rename(fifth_level_vector  = vector) |>
    select(where(~ !all(is.na(.))))


  vector_labels <- vectors_fed_to_keep |> select(vector, label)

  vector_tree <- vector_tree |>
    left_join(vector_labels, join_by(root_vector == vector)) |>  rename(root_label  =label) |>
    left_join(vector_labels, join_by(second_level_vector == vector)) |>  rename(second_level_label  =label) |>
    left_join(vector_labels, join_by(third_level_vector == vector)) |>  rename(third_level_label  =label) |>
    mutate(leaf_vector = if_else(!is.na(third_level_vector), third_level_vector,
                                 if_else(!is.na(second_level_vector), second_level_vector,
                                         root_vector)
    ))

  census_vector_list <- list_census_vectors(dataset)[, c("vector", "type", "label","details")]

  vector_tree <- vector_tree |>  left_join(census_vector_list,join_by(leaf_vector ==vector))
  vector_tree$details <- gsub("^(.*)Census; |100% data; ","",vector_tree$details)


  all_wds_data_for_federal_districts_with_metadata <- all_wds_data_for_federal_districts_raw |>
    filter(OBS_VALUE  !="" & OBS_VALUE !="" & OBS_VALUE!=0) |>
    select(-CHARACTERISTIC_NAME) |>
    left_join(characteristics, join_by(CHARACTERISTIC ==
                                         ID)) |>
    left_join(topics, by = join_by(TOPIC == ID)) |>
    select( CHARACTERISTIC, CHARACTERISTIC_NAME, TOPIC_NAME, GENDER, OBS_VALUE, starts_with('GEO'), CHARACTERISTIC_PARENT_ID ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    left_join(
      characteristics |>
        rename(
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME = CHARACTERISTIC_NAME,
          CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID  = CHARACTERISTIC_PARENT_ID
        ),
      join_by(CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_ID == ID)
    ) |>
    #  select(where(~ !all(is.na(.)))) |>
    select(OBS_VALUE,GENDER,GEO_DESC, GEO_NAME, ends_with('NAME'), CHARACTERISTIC, ends_with("ID")) |>
    mutate(across(everything(), ~ ifelse(is.na(.), "", .))) |>
    mutate(query  =paste(
      CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_PARENT_NAME,
      CHARACTERISTIC_PARENT_NAME,
      CHARACTERISTIC_NAME,
      sep = '; '
    )) |>
    mutate(query = str_remove(query, "^(; )+")) |>
    mutate(query = paste(TOPIC_NAME, query, sep = '; '))



  all_wds_data_for_federal_districts_with_metadata |> select(-OBS_VALUE, -starts_with("GEO")) |>  distinct()  |>  filter(if_any(everything(), ~ str_detect(., 'Knowledge'))) |> View()



  twnety_wds <- all_wds_data_for_federal_districts_with_metadata  |>
    slice_sample(n = 20) |>
    mutate(gender_type = case_match(GENDER,
                                    "1" ~ 'total',
                                    '2' ~ 'male',
                                    '3' ~ 'female' )) |>
    select(query,OBS_VALUE, GEO_DESC, GEO_NAME, , TOPIC_NAME, gender_type)






  za <- pmap(
    list(twnety_wds$query, twnety_wds$gender_type)
    , function(x, y) {
      find_census_vectors(dataset = 'CA21',
                          query =  x,
                          type = y,
                          query_type = 'semantic'
      ) |>  mutate(query = x, gender = y)
    })



}
