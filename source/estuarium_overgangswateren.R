mi_data %>% filter(jaar > 2009) %>% filter(type %in% c("Mlz", "O2zout", "O1o", "O1brak")) %>% mapview
