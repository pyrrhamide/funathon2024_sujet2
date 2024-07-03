import_data <- function(df){
  arrow::read_parquet(df) %>% 
    rename_with(tolower) %>% 
    mutate(anmois = as.character(anmois),
           an = str_sub(anmois, 1, 4),
           mois = str_remove(str_sub(anmois, -2), "^0"))
}
