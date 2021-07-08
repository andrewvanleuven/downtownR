info_clean <- function(dataframe) {
  data.table::fread(dataframe) %>%
    janitor::clean_names() %>%
    dplyr::select(id = 35,
           company = 1,
           address = 2,
           city = 3,
           state = 4,
           zip = 5,
           size_code = 10,
           naics = 14,
           cal_yr = 26,
           jobs = 28,
           biz_code = 30,
           latitude = 47,
           longitude = 48,
           match_code = 49,
           cty_fips = 53) %>%
    dplyr::mutate(id = as.numeric(id),
           company = as.character(company),
           address = as.character(address),
           city = as.character(city),
           state = as.character(state),
           zip = as.numeric(zip),
           size_code = as.character(size_code),
           naics = as.numeric(naics),
           cal_yr = as.numeric(cal_yr),
           jobs = as.numeric(jobs),
           biz_code = as.character(biz_code),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           match_code = as.character(match_code),
           cty_fips = as.numeric(cty_fips))
}

trim_census <- function(x) {
  x %>% mutate_if(is.character, str_replace_all, pattern = " city", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " CDP", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " village", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " borough", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " town", replacement = "") %>% 
    mutate_if(is.character, str_trim)
}