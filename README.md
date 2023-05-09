
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
  |-data_raw
```

These are the raw data files that cannot be shared.

All other required folders are created at runtime.
The following scripts are included:

- `helpers.R`: auxiliary functions used in other scripts
- `00_process_data_raw.R`: processes the raw data
- `04_paper_analyses.R`: main analysis script

Use `renv::restore()` to install required R package with the correct version