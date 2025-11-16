prioritering <- read_excel("data/ruw/overstorten/prioritering/prioritering.xlsx") %>%
  mutate(
    owl = str_replace(waterlichaam_code, "^(A0_G_|A0_)", "")
  )

