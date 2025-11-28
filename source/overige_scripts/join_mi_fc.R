test2 <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  select(meetplaats, monsternamedatum) %>%
  left_join(.,
            fc_selectie,
            by = c("meetplaats"), suffix = c("", "_fc")) %>%
  filter(

{
  days_before <- as.numeric(difftime(monsternamedatum, monsternamedatum_fc, units = "days"))
  days_before < 90 &
  days_before > -15

}
) %>%
  group_by(meetplaats, monsternamedatum) %>% #dubbele samples uitmiddelen
  summarise(
    across(
      where(is.numeric), \(x) mean(x, na.rm = TRUE) # enkel numerische kolommen om de mean te pakken
    ), # voor niet numerische waarden gewoon de eerste string nemen om te behouden
    across(
      where(is.factor) | where(is.character),
      ~ first(.)
    ),
    .groups = "drop" # Drop the grouping at the end
  )
test2 <- test2[order(test2$monsternamedatum),]

vis_miss(test2)

sum(is.na(test2$p_t))


