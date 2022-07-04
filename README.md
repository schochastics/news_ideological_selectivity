
# Analyzing audience networks

Material to replicate xxxx

A folder named `data` needs to be added with the following structure:

```
data  
|-domain_lists.rds  
|-domain_lists_v2.rds  
|-news_types.rds  
|-party_families.xlsx  
|-political_URLs.rds  
|-survey_data_r.rds  
|-tracking  
  |-data_parsed
    |-merged-parsed-tracking_de_v1.csv  
    |-merged-parsed-tracking_fr.csv  
    |-merged-parsed-tracking_uk.csv  
    |-merged-parsed-tracking_es.csv  
    |-merged-parsed-tracking_it.csv  
    |-merged-parsed-tracking_us.csv  
```
All other required folders are created at runtime.
The following scripts are included:

- `helpers.R`: auxiliary functions used in other scripts
- `00_process_data.R`: processes the raw data
- `01_create_networks.R`: creates the pmi and other backbone audience networks
- `02_analyze_networks.R`: analyses the networks, e.g. comparing density 
- `03_analyze_data.R`: analyses the processed data, e.g. segregation scores
