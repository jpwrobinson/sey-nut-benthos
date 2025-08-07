library(tidyverse)
library(readxl)
library(janitor)
theme_set(theme_classic())

# load data, clean headers, and pivot into long form for easy ggplotting
dat<-read_excel('data/ICP-OES_Mg-Kg Calculation_GF_Analysis_JR.xls') %>% 
  clean_names() %>% 
  pivot_longer(-c(sample_no, sample_name, site, site_abb, benthic_type, benthic_abb, notes),
               names_to = 'element', values_to = 'value')

# inspecting clear outliers in histograms
ggplot(dat, aes(value)) + geom_histogram() + 
  facet_grid(~element, scales='free')

# obvious outliers - contamination?

# Calcium - looks fine
dat %>% filter(element=='ca_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

# Iron
dat %>% filter(element=='fe_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

dat %>% filter(element=='fe_mg_kg' & value > 4000) # 8,267 iron mg/kg

# Potassium: clear set of very large values, and most samples 
dat %>% filter(element=='k_mg_kg' &  value < 20000) %>% 
  ggplot() + geom_histogram(aes(value))

# Brown macroalgae - suggests this is taxa specific, not an outlier
dat %>% filter(element=='k_mg_kg' & value > 15000) # n = 20.
dat %>% filter(element=='k_mg_kg' & value < 0) # one false read?

# Magnesium - looks fine
dat %>% filter(element=='mg_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

# Sodium - possible outliers but nothing severe
dat %>% filter(element=='na_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

# Phosphorus - possible outliers above 4000
dat %>% filter(element=='p_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

dat %>% filter(element=='p_mg_kg' & value > 4000)

# Zinc - 6 macroalgae samples 0.5-1x greater than rest of sample set
dat %>% filter(element=='zn_mg_kg') %>% 
  ggplot() + geom_histogram(aes(value))

# drop two outliers
dat<-dat %>% mutate(
  value = ifelse(element == 'fe_mg_kg' & value > 8000, NA, value),
  value = ifelse(element=='k_mg_kg' & value < 0, NA, value))

# plot by taxa and site
pdf(file = 'fig/observed_element_conc_taxa.pdf', height=7, width=12)
ggplot(dat, aes(benthic_abb, value)) +
  geom_boxplot() + facet_grid(element ~ benthic_type, scales='free')
dev.off()

