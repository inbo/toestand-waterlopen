# Installeer gratia als je dit nog niet hebt: install.packages("gratia")
library(gratia)

# Haal één specifiek model uit je geneste dataframe (bijv. de beek-groep voor mmif)
model_voorbeeld <- mi_models %>%
  filter(subset == "nat_sv_kempen", index == "mmif") %>%
  pull(model) %>%
  .[[1]]

# Optie A: De standaard mgcv validatie
# Dit geeft output in je console (over de k-waarde) en basis R-plots
gam.check(model_voorbeeld)

# Optie B: De moderne gratia validatie (Aanrader!)
# Dit genereert een overzichtelijk panel met ggplot2 grafieken (QQ-plot, residuen, etc.)
appraise(model_voorbeeld)
