# get size ranges from NEFSC food habits data

# object is called `allfh`
load(url("https://github.com/Laurels1/Condition/raw/master/data/allfh.RData"))

sizedefs <- allfh %>% 
  select(pdcomnam, sizecat, pdlen) %>% 
  group_by(pdcomnam, sizecat) %>% 
  summarize(minlen = min(pdlen), maxlen = max(pdlen))

write_csv(sizedefs, here("fhdat/sizedefs.csv"))