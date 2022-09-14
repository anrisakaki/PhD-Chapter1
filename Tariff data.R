########################
# CLEANING TARIFF DATA #
########################

# Tariff data was downloaded from Brian McCaig's website: https://sites.google.com/site/briandmccaig/notes-on-vhlsss

tariff <- tariff %>%
  select(isic2, col2_ave_all, mfn_ave_all)

tariff$isic2 <- as.factor(tariff$isic2)