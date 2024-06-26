summary_stat_airport <- function(df, in_an, in_mois){
  df %>% 
    filter(an == in_an, mois == in_mois) %>% 
    select(apt, apt_nom_clean2, starts_with("apt_pax_"), trafic) %>% 
    arrange(-trafic)
}
