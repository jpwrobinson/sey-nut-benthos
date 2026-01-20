source('01_load.R')

meta<-read.csv('data/benthic_elements_meta_Jan2026.csv')

# fix units to mg/kg and match benthic names
# μg/g = mg/kg
# ppm = mg / kg
meta<-meta %>% 
  mutate(value = ifelse(unit %in% c('mg/100 g', 'mg/100g'), value/10, value),
         benthic_type = recode(taxa, 'Turf' = 'Turf Algae',
                                      'Sediment Organic Matter' = 'Sediment',
                                      'Soft corals' = 'Soft Coral'),
         id = paste(element, taxa, ref)) %>% 
  filter(element %in% c('Fe', 'Zn'))


ggplot(meta, aes(benthic_type, value)) + 
  geom_point(data = meta %>% filter(stat == 'mean')) +
  geom_point(data = dat %>% filter(element %in% c('Fe', 'Zn')), col='red') +
  geom_line(aes(group = id)) +
  facet_wrap(~element, scales='free')
