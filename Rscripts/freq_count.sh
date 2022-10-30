#!/bin/bash

xsv select type ../processed_data/desktop/de.csv | xsv frequency > ../processed_data/stats/de_type_freq_visits.csv
xsv select type ../processed_data/desktop/es.csv | xsv frequency > ../processed_data/stats/es_type_freq_visits.csv
xsv select type ../processed_data/desktop/fr.csv | xsv frequency > ../processed_data/stats/fr_type_freq_visits.csv
xsv select type ../processed_data/desktop/it.csv | xsv frequency > ../processed_data/stats/it_type_freq_visits.csv
xsv select type ../processed_data/desktop/uk.csv | xsv frequency > ../processed_data/stats/uk_type_freq_visits.csv
xsv select type ../processed_data/desktop/us.csv | xsv frequency > ../processed_data/stats/us_type_freq_visits.csv

xsv select panelist_id ../processed_data/desktop/de.csv | sort | uniq -c | wc -l > ../processed_data/stats/de_uniq_panelists.csv
xsv select panelist_id ../processed_data/desktop/es.csv | sort | uniq -c | wc -l > ../processed_data/stats/es_uniq_panelists.csv
xsv select panelist_id ../processed_data/desktop/fr.csv | sort | uniq -c | wc -l > ../processed_data/stats/fr_uniq_panelists.csv
xsv select panelist_id ../processed_data/desktop/it.csv | sort | uniq -c | wc -l > ../processed_data/stats/it_uniq_panelists.csv
xsv select panelist_id ../processed_data/desktop/uk.csv | sort | uniq -c | wc -l > ../processed_data/stats/uk_uniq_panelists.csv
xsv select panelist_id ../processed_data/desktop/us.csv | sort | uniq -c | wc -l > ../processed_data/stats/us_uniq_panelists.csv

xsv search -s type 'news' ../processed_data/desktop/de.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/de_uniq_panelists_news.csv
xsv search -s type 'news' ../processed_data/desktop/es.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/es_uniq_panelists_news.csv
xsv search -s type 'news' ../processed_data/desktop/fr.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/fr_uniq_panelists_news.csv
xsv search -s type 'news' ../processed_data/desktop/it.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/it_uniq_panelists_news.csv
xsv search -s type 'news' ../processed_data/desktop/uk.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/uk_uniq_panelists_news.csv
xsv search -s type 'news' ../processed_data/desktop/us.csv | xsv select panelist_id | sort | uniq -c | wc -l > ../processed_data/stats/us_uniq_panelists_news.csv
