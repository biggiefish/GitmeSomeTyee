library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)

tx <- pdf_text("C:/Users/evogt/Downloads/Tyee-Club-Roster-for-Year-Book-August-3-2019.pdf")
tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)




tx4 <- tx3 %>% as.data.frame() %>%
                  mutate(across(where(is.character), ~ na_if(., ""))) %>%
                  drop_na() %>%
                  filter(!str_detect(V1, "Last Name")) %>%
                  filter(!str_detect(.[[1]], "Tyee Club Roster for Year Book August 3, 2019")) %>%
                  filter(!str_detect(.[[1]], "Page ")) %>%
                  mutate(fullname = paste(.[[1]], ",", .[[2]] ),
                         origin   = paste(.[[3]], ",", .[[4]])) %>%
                  rename("LastName"  = "V1",
                         "FirstName" = "V2",
                         "City"      = "V3",
                         "Prov"      = "V4",
                         "DateWt"    = "V5") %>%
                  separate(DateWt, c("day","month","year","lb", "oz")) %>%
                  mutate(month = as.numeric(match(month,month.abb)),
                         year  = as.numeric(year),
                         day   = as.numeric(day),
                         lb    = as.numeric(lb),
                         oz    = as.numeric(oz),
                         oz    = ifelse(is.na(oz),0,oz),
                         oz    = ifelse(oz>0,oz/16,0),
                         weight = lb+oz)

write.csv(tx4, "C:/Users/evogt/Downloads/TyeeClubRoster2019.csv")






str(tx4)
                  