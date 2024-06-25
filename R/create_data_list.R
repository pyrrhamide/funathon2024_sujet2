# transformer ce bout de code en une fonction create_data_list prenant un argument source_file et renvoyant cette liste 
create_data_list <- function(source_file) yaml::read_yaml(source_file)
