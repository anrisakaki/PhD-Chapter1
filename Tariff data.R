########################
# CLEANING TARIFF DATA #
########################

# Tariff data was downloaded from Brian McCaig's website: https://sites.google.com/site/briandmccaig/notes-on-vhlsss

tariff <- tariff %>%
  select(isic2, col2_ave_all, mfn_ave_all) %>% 
  mutate(isic2 = as.factor(isic2),
         Change_rate = col2_ave_all - mfn_ave_all)

tariff$Industry <-  forcats::fct_recode(tariff$isic2, 
                                        "Agriculture and livestock cultivation" = "1",
                                        "Forestry" = "2",
                                        "Aquaculture" = "5",
                                        "Coal Mining" = "10",
                                        "Oil and Gas" = "11",
                                        "Non-metal mining" = "12",
                                        "Metal ores" = "13",
                                        "Other Mining" = "14",
                                        "Food and Beverages" = "15",
                                        "Tobacco" = "16",
                                        "Textiles" = "17",
                                        "Wearing apparel" = "18",
                                        "Leather processing" = "19",
                                        "Wood processing" = "20",
                                        "Paper and paper products" = "21",
                                        "Printing and publishing" = "22",
                                        "Crude Oil" = "23",
                                        "Chemical products processing" = "24",
                                        "Plastic and rubber" = "25",
                                        "Non-metal mineral products" = "26",
                                        "Metal processing" = "27",
                                        "Metal products" = "28",
                                        "Equipment and Machinery" = "29",
                                        "Office, accounting and computing machinery" = "30",
                                        "Electronic and electric equipment" = "31",
                                        "Radio, television and communication equipment and apparatus" = "32",
                                        "Medical equipment" = "33",
                                        "Motor equipment and parts" = "34",
                                        "Other transport equipment" = "35",
                                        "Furniture"  = "36",                                      
                                        "Electricity" = "40",
                                        "Business support services" = "74",
                                        "Waste management" = "92",
                                        "Other Services" = "93")
# Bar chart for tariff cuts 
tariff <- tariff %>% 
  arrange(desc(Change_rate)) %>% 
  mutate(Change_rate = Change_rate*100)

ggplot(dplyr::filter(tariff, Change_rate > 0.1), aes(reorder(Industry, Change_rate), Change_rate)) +
  geom_col() +
  labs(x = "Industry",
       y= "Tariff Cut (Percentage Points)") +
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()
ggsave("industrybarchart.png")