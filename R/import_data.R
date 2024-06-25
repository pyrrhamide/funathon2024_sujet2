import_data <- function(df){
  read_csv2(df,
            col_types = cols(ANMOIS = col_character())) %>% 
    # à partir de ANMOIS, créer an et mois
    mutate(an = str_sub(ANMOIS, 1, 4),
           mois = str_remove(str_sub(ANMOIS, -2), "^0")) %>% 
    rename_with(tolower)
}
