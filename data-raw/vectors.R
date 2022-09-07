# Vectors and breakdowns for app

library(dplyr)
library(censusaggregate)
library(cancensus)
library(purrr)
library(stringr)

vectors <- tibble::tribble(
  ~vector, ~vector_label, ~vector_label_short,
  "v_CA16_401", "Population, 2016", "population_2016",
  "v_CA16_402", "Population, 2011", "population_2011",
  "v_CA16_407", "Land area in square kilometres", "area",
  "v_CA16_1", "Total - Age", "age",
  "v_CA16_418", "Private households by household size", "household_size",
  "v_CA16_484", "Total number of census families in private households - 100% data", "family_type",
  "v_CA16_504", "Total - Private households by household type - 100% data", "household_type",
  "v_CA16_512", "Total - Knowledge of official languages for the total population excluding institutional residents - 100% data", "knowledge_of_english",
  "v_CA16_1355", "Total - Language spoken most often at home for the total population excluding institutional residents - 100% data", "top_10_languages",
  "v_CA16_2525", "In low income based on the Low-income measure, after tax (LIM-AT)", "lim_at",
  "v_CA16_3405", "Total - Immigrant status and period of immigration for the population in private households - 25% sample data", "immigrant_status",
  "v_CA16_3852", "Total - Aboriginal identity for the population in private households - 25% sample data", "aboriginal_identity",
  "v_CA16_3954", "Total - Visible minority for the population in private households - 25% sample data", "visible_minority",
  "v_CA16_3999", "Total - Ethnic origin for the population in private households - 25% sample data", "ethnic_origin",
  "v_CA16_4836", "Total - Private households by tenure - 25% sample data", "household_tenure",
  "v_CA16_4886", "Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data", "unaffordable_housing",
  "v_CA16_5051", "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data", "educational_attainment"
)

# List census vectors and their children
vectors_and_children <- vectors[["vector"]] %>%
  purrr::set_names() %>%
  purrr::map_dfr(cancensus::child_census_vectors,
    keep_parent = TRUE,
    .id = "highest_parent_vector"
  )

# Filter for specific children/breakdowns
# NA means keep all children, otherwise just keep the ones in the list

# To get age cohort breakdowns:
# cancensus::child_census_vectors("v_CA16_1", keep_parent = TRUE) %>%
#   filter(str_detect(label, "to") | str_detect(label, "and over"), units == "Number") %>%
#   mutate(label_derived = str_replace(label, "years and over", "to NA years")) %>%
#   separate(label_derived, into = c("min", "max"), sep = " to ", convert = TRUE) %>%
#   mutate(
#     max = str_remove(max, " years"), max = as.numeric(max),
#     gap = max - min,
#     group = case_when(
#       min == 0 & max == 14 ~ "0 to 14",
#       min >= 15 & max <= 24 & gap == 4 ~ "15 to 24",
#       min >= 25 & max <= 44 & gap == 4 ~ "25 to 44",
#       min >= 45 & max <= 64 & gap == 4 ~ "45 to 65",
#       min >= 65 & max <= 84 & gap == 4 ~ "65 to 84",
#       min == 85 & is.na(max) ~ "85+"
#     )
#   ) %>%
#   filter(!is.na(group)) %>%
#   select(vector, group)

# To get official language spoken at home breakdowns
# cancensus::child_census_vectors("v_CA16_1355", leaves_only = TRUE) %>%
#   pull(vector)

# Same for ethnic origin
# cancensus::child_census_vectors("v_CA16_3999", leaves_only = TRUE) %>%
#   pull(vector)

breakdown_vectors <- list(
  "v_CA16_401" = NA,
  "v_CA16_418" = c("v_CA16_419", "v_CA16_420", "v_CA16_421", "v_CA16_422", "v_CA16_423"),
  "v_CA16_3954" = c(
    "v_CA16_3957", "v_CA16_3960", "v_CA16_3963", "v_CA16_3966",
    "v_CA16_3969", "v_CA16_3972", "v_CA16_3975", "v_CA16_3978", "v_CA16_3981",
    "v_CA16_3984", "v_CA16_3987", "v_CA16_3990", "v_CA16_3993",
    "v_CA16_3996"
  ),
  "v_CA16_4836" = c("v_CA16_4837", "v_CA16_4838", "v_CA16_4839"),
  "v_CA16_4886" = "v_CA16_4888",
  "v_CA16_1" = c(
    # 5 year breakdown
    "v_CA16_7", "v_CA16_25", "v_CA16_43", "v_CA16_64", "v_CA16_82",
    "v_CA16_100", "v_CA16_118", "v_CA16_136", "v_CA16_154", "v_CA16_172",
    "v_CA16_190", "v_CA16_208", "v_CA16_226", "v_CA16_247", "v_CA16_265",
    "v_CA16_283", "v_CA16_301", "v_CA16_322", "v_CA16_340", "v_CA16_358", "v_CA16_376",
    # Age cohorts
    "v_CA16_4", "v_CA16_64", "v_CA16_82", "v_CA16_100", "v_CA16_118",
    "v_CA16_136", "v_CA16_154", "v_CA16_172", "v_CA16_190", "v_CA16_208",
    "v_CA16_226", "v_CA16_247", "v_CA16_265", "v_CA16_283", "v_CA16_301",
    "v_CA16_319"
  ),
  "v_CA16_512" = c("v_CA16_515", "v_CA16_521"),
  "v_CA16_3405" = c("v_CA16_3411", "v_CA16_3432"),
  "v_CA16_3852" = c("v_CA16_3855"),
  "v_CA16_5051" = c("v_CA16_5054", "v_CA16_5057", "v_CA16_5060"),
  "v_CA16_484" = c("v_CA16_485", "v_CA16_488", "v_CA16_489"),
  "v_CA16_504" = c("v_CA16_505", "v_CA16_508", "v_CA16_509"),
  "v_CA16_402" = NA,
  "v_CA16_407" = NA,
  "v_CA16_1355" = c(
    "v_CA16_2150", "v_CA16_2153", "v_CA16_2156", "v_CA16_2159",
    "v_CA16_1364", "v_CA16_1367", "v_CA16_1508", "v_CA16_1538", "v_CA16_1541",
    "v_CA16_1586", "v_CA16_1619", "v_CA16_1766", "v_CA16_1964", "v_CA16_1973",
    "v_CA16_2144", "v_CA16_1379", "v_CA16_1439", "v_CA16_1505", "v_CA16_1514",
    "v_CA16_1517", "v_CA16_1520", "v_CA16_1526", "v_CA16_1529", "v_CA16_1532",
    "v_CA16_1535", "v_CA16_1547", "v_CA16_1550", "v_CA16_1553", "v_CA16_1556",
    "v_CA16_1559", "v_CA16_1562", "v_CA16_1565", "v_CA16_1568", "v_CA16_1571",
    "v_CA16_1577", "v_CA16_1580", "v_CA16_1583", "v_CA16_1592", "v_CA16_1595",
    "v_CA16_1598", "v_CA16_1604", "v_CA16_1607", "v_CA16_1610", "v_CA16_1613",
    "v_CA16_1616", "v_CA16_1682", "v_CA16_1688", "v_CA16_1691", "v_CA16_1694",
    "v_CA16_1700", "v_CA16_1703", "v_CA16_1706", "v_CA16_1709", "v_CA16_1712",
    "v_CA16_1715", "v_CA16_1718", "v_CA16_1721", "v_CA16_1724", "v_CA16_1727",
    "v_CA16_1730", "v_CA16_1733", "v_CA16_1739", "v_CA16_1742", "v_CA16_1745",
    "v_CA16_1751", "v_CA16_1754", "v_CA16_1757", "v_CA16_1760", "v_CA16_1763",
    "v_CA16_1772", "v_CA16_1775", "v_CA16_1883", "v_CA16_1970", "v_CA16_1979",
    "v_CA16_1985", "v_CA16_1988", "v_CA16_1991", "v_CA16_1994", "v_CA16_1997",
    "v_CA16_2000", "v_CA16_2003", "v_CA16_2006", "v_CA16_2009", "v_CA16_2012",
    "v_CA16_2015", "v_CA16_2018", "v_CA16_2021", "v_CA16_2024", "v_CA16_2027",
    "v_CA16_2030", "v_CA16_2036", "v_CA16_2039", "v_CA16_2045", "v_CA16_2048",
    "v_CA16_2051", "v_CA16_2102", "v_CA16_2105", "v_CA16_2108", "v_CA16_2114",
    "v_CA16_2117", "v_CA16_2120", "v_CA16_2123", "v_CA16_2126", "v_CA16_2132",
    "v_CA16_2135", "v_CA16_2138", "v_CA16_2141", "v_CA16_1385", "v_CA16_1388",
    "v_CA16_1391", "v_CA16_1394", "v_CA16_1397", "v_CA16_1400", "v_CA16_1403",
    "v_CA16_1406", "v_CA16_1409", "v_CA16_1412", "v_CA16_1418", "v_CA16_1421",
    "v_CA16_1427", "v_CA16_1430", "v_CA16_1433", "v_CA16_1436", "v_CA16_1448",
    "v_CA16_1451", "v_CA16_1454", "v_CA16_1457", "v_CA16_1460", "v_CA16_1463",
    "v_CA16_1466", "v_CA16_1469", "v_CA16_1472", "v_CA16_1631", "v_CA16_1634",
    "v_CA16_1640", "v_CA16_1643", "v_CA16_1646", "v_CA16_1649", "v_CA16_1655",
    "v_CA16_1658", "v_CA16_1661", "v_CA16_1664", "v_CA16_1667", "v_CA16_1670",
    "v_CA16_1673", "v_CA16_1676", "v_CA16_1679", "v_CA16_1838", "v_CA16_1841",
    "v_CA16_1844", "v_CA16_1850", "v_CA16_1853", "v_CA16_1856", "v_CA16_1859",
    "v_CA16_1862", "v_CA16_1865", "v_CA16_1868", "v_CA16_1871", "v_CA16_1874",
    "v_CA16_1877", "v_CA16_1880", "v_CA16_1940", "v_CA16_1946", "v_CA16_1949",
    "v_CA16_1952", "v_CA16_1955", "v_CA16_1958", "v_CA16_1961", "v_CA16_2060",
    "v_CA16_2063", "v_CA16_2066", "v_CA16_2069", "v_CA16_2072", "v_CA16_2075",
    "v_CA16_2078", "v_CA16_2081", "v_CA16_2087", "v_CA16_2090", "v_CA16_2093",
    "v_CA16_2096", "v_CA16_1478", "v_CA16_1481", "v_CA16_1484", "v_CA16_1490",
    "v_CA16_1493", "v_CA16_1499", "v_CA16_1502", "v_CA16_1784", "v_CA16_1787",
    "v_CA16_1793", "v_CA16_1796", "v_CA16_1799", "v_CA16_1802", "v_CA16_1805",
    "v_CA16_1808", "v_CA16_1811", "v_CA16_1814", "v_CA16_1817", "v_CA16_1820",
    "v_CA16_1823", "v_CA16_1826", "v_CA16_1829", "v_CA16_1832", "v_CA16_1892",
    "v_CA16_1895", "v_CA16_1898", "v_CA16_1901", "v_CA16_1904", "v_CA16_1907",
    "v_CA16_1910", "v_CA16_1913", "v_CA16_1916", "v_CA16_1919", "v_CA16_1922",
    "v_CA16_1925", "v_CA16_1931", "v_CA16_1934", "v_CA16_1937"
  ),
  "v_CA16_2525" = NA,
  "v_CA16_3999" = c(
    "v_CA16_4005", "v_CA16_4008", "v_CA16_4011", "v_CA16_4017",
    "v_CA16_4020", "v_CA16_4023", "v_CA16_4026", "v_CA16_4029", "v_CA16_4032",
    "v_CA16_4035", "v_CA16_4038", "v_CA16_4041", "v_CA16_4269", "v_CA16_4272",
    "v_CA16_4275", "v_CA16_4278", "v_CA16_4281", "v_CA16_4284", "v_CA16_4287",
    "v_CA16_4290", "v_CA16_4293", "v_CA16_4296", "v_CA16_4299", "v_CA16_4302",
    "v_CA16_4305", "v_CA16_4308", "v_CA16_4311", "v_CA16_4314", "v_CA16_4317",
    "v_CA16_4320", "v_CA16_4323", "v_CA16_4326", "v_CA16_4332", "v_CA16_4335",
    "v_CA16_4338", "v_CA16_4341", "v_CA16_4344", "v_CA16_4347", "v_CA16_4350",
    "v_CA16_4353", "v_CA16_4356", "v_CA16_4359", "v_CA16_4362", "v_CA16_4365",
    "v_CA16_4368", "v_CA16_4371", "v_CA16_4374", "v_CA16_4377", "v_CA16_4380",
    "v_CA16_4383", "v_CA16_4386", "v_CA16_4389", "v_CA16_4392", "v_CA16_4395",
    "v_CA16_4398", "v_CA16_4401", "v_CA16_4809", "v_CA16_4812", "v_CA16_4050",
    "v_CA16_4053", "v_CA16_4056", "v_CA16_4059", "v_CA16_4062", "v_CA16_4065",
    "v_CA16_4068", "v_CA16_4071", "v_CA16_4077", "v_CA16_4080", "v_CA16_4083",
    "v_CA16_4086", "v_CA16_4092", "v_CA16_4095", "v_CA16_4098", "v_CA16_4101",
    "v_CA16_4104", "v_CA16_4107", "v_CA16_4110", "v_CA16_4113", "v_CA16_4116",
    "v_CA16_4119", "v_CA16_4125", "v_CA16_4128", "v_CA16_4131", "v_CA16_4134",
    "v_CA16_4137", "v_CA16_4140", "v_CA16_4146", "v_CA16_4149", "v_CA16_4152",
    "v_CA16_4155", "v_CA16_4158", "v_CA16_4161", "v_CA16_4164", "v_CA16_4167",
    "v_CA16_4170", "v_CA16_4173", "v_CA16_4176", "v_CA16_4179", "v_CA16_4182",
    "v_CA16_4185", "v_CA16_4188", "v_CA16_4194", "v_CA16_4197", "v_CA16_4200",
    "v_CA16_4203", "v_CA16_4206", "v_CA16_4209", "v_CA16_4212", "v_CA16_4215",
    "v_CA16_4218", "v_CA16_4221", "v_CA16_4224", "v_CA16_4227", "v_CA16_4230",
    "v_CA16_4233", "v_CA16_4236", "v_CA16_4239", "v_CA16_4242", "v_CA16_4245",
    "v_CA16_4251", "v_CA16_4254", "v_CA16_4257", "v_CA16_4260", "v_CA16_4263",
    "v_CA16_4410", "v_CA16_4413", "v_CA16_4416", "v_CA16_4419", "v_CA16_4422",
    "v_CA16_4425", "v_CA16_4428", "v_CA16_4431", "v_CA16_4434", "v_CA16_4437",
    "v_CA16_4440", "v_CA16_4443", "v_CA16_4446", "v_CA16_4449", "v_CA16_4452",
    "v_CA16_4455", "v_CA16_4458", "v_CA16_4461", "v_CA16_4464", "v_CA16_4467",
    "v_CA16_4470", "v_CA16_4473", "v_CA16_4476", "v_CA16_4479", "v_CA16_4482",
    "v_CA16_4485", "v_CA16_4488", "v_CA16_4494", "v_CA16_4497", "v_CA16_4500",
    "v_CA16_4503", "v_CA16_4506", "v_CA16_4509", "v_CA16_4512", "v_CA16_4515",
    "v_CA16_4518", "v_CA16_4521", "v_CA16_4524", "v_CA16_4530", "v_CA16_4533",
    "v_CA16_4536", "v_CA16_4539", "v_CA16_4542", "v_CA16_4545", "v_CA16_4548",
    "v_CA16_4551", "v_CA16_4554", "v_CA16_4557", "v_CA16_4560", "v_CA16_4563",
    "v_CA16_4566", "v_CA16_4569", "v_CA16_4572", "v_CA16_4575", "v_CA16_4578",
    "v_CA16_4581", "v_CA16_4584", "v_CA16_4587", "v_CA16_4590", "v_CA16_4593",
    "v_CA16_4596", "v_CA16_4602", "v_CA16_4605", "v_CA16_4614", "v_CA16_4617",
    "v_CA16_4620", "v_CA16_4623", "v_CA16_4626", "v_CA16_4629", "v_CA16_4632",
    "v_CA16_4635", "v_CA16_4638", "v_CA16_4641", "v_CA16_4644", "v_CA16_4647",
    "v_CA16_4650", "v_CA16_4653", "v_CA16_4656", "v_CA16_4659", "v_CA16_4662",
    "v_CA16_4665", "v_CA16_4668", "v_CA16_4671", "v_CA16_4674", "v_CA16_4677",
    "v_CA16_4680", "v_CA16_4683", "v_CA16_4686", "v_CA16_4689", "v_CA16_4692",
    "v_CA16_4695", "v_CA16_4701", "v_CA16_4704", "v_CA16_4707", "v_CA16_4710",
    "v_CA16_4713", "v_CA16_4716", "v_CA16_4719", "v_CA16_4722", "v_CA16_4725",
    "v_CA16_4728", "v_CA16_4731", "v_CA16_4734", "v_CA16_4737", "v_CA16_4740",
    "v_CA16_4746", "v_CA16_4749", "v_CA16_4752", "v_CA16_4755", "v_CA16_4758",
    "v_CA16_4761", "v_CA16_4764", "v_CA16_4767", "v_CA16_4770", "v_CA16_4773",
    "v_CA16_4776", "v_CA16_4779", "v_CA16_4782", "v_CA16_4785", "v_CA16_4788",
    "v_CA16_4791", "v_CA16_4794", "v_CA16_4797", "v_CA16_4803", "v_CA16_4818",
    "v_CA16_4821", "v_CA16_4824", "v_CA16_4827", "v_CA16_4830", "v_CA16_4833"
  )
)

filter_breakdown_vectors <- function(data, vector) {
  vector_breakdown_vectors <- breakdown_vectors[[vector]]

  if (!all(is.na(vector_breakdown_vectors))) {
    data <- data %>%
      filter(
        vector %in% c(vector_breakdown_vectors, !!vector)
      )
  } else {
    data <- data %>%
      filter(
        vector == !!vector
      )
  }

  data
}

vectors_and_children <- map_dfr(
  vectors[["vector"]],
  function(vector) {
    vectors_and_children %>%
      filter_breakdown_vectors(vector)
  }
)

# Save vectors
vectors <- vectors_and_children %>%
  left_join(vectors, by = c("vector", "label" = "vector_label"))

usethis::use_data(vectors, overwrite = TRUE)

# For now, limit to vectors that have.... <= 25 children
vectors_and_children_few <- vectors_and_children %>%
  add_count(highest_parent_vector) %>%
  filter(n <= 25) %>%
  select(-n)

# Get data for ALL CSDs?

csd_data <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CSD",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

ct_data <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

# Many
vectors_and_children_many <- vectors_and_children %>%
  add_count(highest_parent_vector) %>%
  filter(n > 25) %>%
  select(-n)

# Get data for ALL CSDs?

csd_data_many <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CSD",
  vectors = vectors_and_children_many[["vector"]], labels = "short"
)

ct_data_many <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = head(vectors_and_children_many[["vector"]], 200), labels = "short"
)

ct_data_many_2 <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = vectors_and_children_many[["vector"]][201:474], labels = "short"
)

# Pivot and combine
pivot_census_data <- function(data) {
  data %>%
    dplyr::select(
      geo_uid = .data$GeoUID,
      dplyr::starts_with("v_CA16_")
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("v_CA16_"), names_to = "vector")
}

csd_values <- bind_rows(
  csd_data %>%
    pivot_census_data(),
  csd_data_many %>%
    pivot_census_data()
) %>%
  filter(!is.na(value)) %>%
  filter(value != 0)

ct_values <- bind_rows(
  ct_data %>%
    pivot_census_data(),
  ct_data_many %>%
    pivot_census_data(),
  ct_data_many_2 %>%
    pivot_census_data()
) %>%
  filter(!is.na(value)) %>%
  filter(value != 0)

# Save via arrow, partitioned by first 2 characters of geo_uid

library(arrow)
csd_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "csd_values"))

ct_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "ct_values"))
