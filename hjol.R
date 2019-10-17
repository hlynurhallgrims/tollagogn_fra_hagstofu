library(tidyverse)
library(pxweb)
library(lubridate)

# Hér er get kall á hagstofuna út frá json fyrirspurninni innflutningur_hjol.json
px_data <- pxweb_get(url = "http://px.hagstofa.is//pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/03_inntollskra/UTA03803.px",
                     query = "fyrirspurnir/innflutningur_hjol.json")

hjol <- px_data$data

view(hjol)

#Komum gögnunum í gagnaramma, a.k.a. "tibblu"
tibbla <- tibble(
  key = map(hjol, "key"), # Búum til list columns inni í tibblunni
  values = map(hjol, "values")
) %>% 
  mutate(tollskrarnumer = map_chr(.x = key, ~pluck(.x[[1]])), #Búum til nýja dálka út frá innihaldi listcolumns
         land = map_chr(.x = key, ~pluck(.x[[2]])),
         timabil = map_chr(.x = key, ~pluck(.x[[3]])),
         eining = map_chr(.x = key, ~pluck(.x[[4]])),
         gildi = map_chr(.x = values, ~pluck(.x[[1]])),
         gildi = as.numeric(gildi)) %>% 
  select(-key, -values) %>% # Fjarlægjum list columns eftir að hafa sett allt í sinn eigin dálk
  separate(timabil, into = c("ar", "man"), sep = "M") %>% #Brjótum 'tímabil' dálkinn með sína "2017M01" strengi, niður í tvo nýja dálka
  mutate_at(.vars = vars(man, ar), #Tökum þessa nýju dálka
            .funs = as.numeric) # og gerum þá að numeric-dálkum

# Heildarinnflutningur óháð landi
heild_inn <- tibbla %>% 
  select(-land) %>% # Í raun óþarfi út af næstu línu, en You get the point
  group_by(tollskrarnumer, ar, man, eining) %>% 
  summarize(gildi = sum(gildi))

heild_inn

# Eitthvað random graf fyrir CIF og FOB út frá tollskrárnúmerum sem innihalda "87116"
heild_inn %>% 
  filter(eining %in% c("cif", "fob"),
         str_detect(string = tollskrarnumer, pattern = "87116")) %>%
  mutate(dags = make_date(year = ar,
                          month = man,
                          day = 1)) %>% 
  ggplot(aes(x = dags, y = gildi, color = eining)) +
  geom_line() +
  facet_grid(rows = vars(tollskrarnumer)) 
##facet_grid(cols = vars(tollskrarnumer)) #eða facetta í raðir, frekar en dálka
