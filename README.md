# README

Replication materials for the article "The Process of Revolutionary Protest: Development and Democracy in the Tunisian Revolution"

## Main directory:

01_eh_analysis.do: Stata .do file for event history analysis of protest data

02_AB_survey_analysis.do: Stata .do file for survey analysis of Arab Barometer data

03_democ_tweets_prepare.R: R script to transliterate tweet data

04_democ_tweets_ts.R: R script to analyze and plot democracy tweets over time

05_get_user_profiles.R: R script to retrieve user profiles of tweeting accounts

06_plot_geo_users.R: R script to infer and plot location of tweeting accounts

07_map_protest.R: R script to plot protest and demographic information on map of Tunisia

08_democ_tweets_ts_robust.R: R script to analyze and plot democracy tweets over time for Tunisia users only

09_news_analysis.R: R script to analyze and plot democracy words in Tunisian news media over time

10_revdata.R: R script to collect and code other revolutionary episodes from secondary datasets

## Directory structure:

```{r}
├── 01_eh_analysis.do
├── 02_AB_survey_analysis.do
├── 03_democ_tweets_prepare.R
├── 04_democ_tweets_ts.R
├── 05_get_user_profiles.R
├── 06_plot_geo_users.R
├── 07_map_protest.R
├── 08_democ_tweets_ts_robust.R
├── 09_news_analysis.R
├── 10_revdata.R
├── README.md
├── data
│   ├── analysis
│   │   ├── tunehdata.dta
│   │   ├── tunfd.csv
│   │   └── tunisia_29.dta
│   ├── output
│   │   ├── countrycountsdf.tex
│   │   ├── demwrds.csv
│   │   ├── demwrdsdf.tex
│   │   ├── hellbern_protests.csv
│   │   ├── hellbern_protests_aut.csv
│   │   ├── latlongs.rds
│   │   ├── nightlights.dta
│   │   ├── nightlights_grcn.dta
│   │   ├── nws_corpus.rds
│   │   ├── reversegeo.rds
│   │   ├── stage1.dta
│   │   ├── stage2.dta
│   │   ├── stage3.dta
│   │   ├── tables
│   │   │   ├── ab1.doc
│   │   │   ├── ab1.tex
│   │   │   ├── ab1.txt
│   │   │   ├── ab2.doc
│   │   │   ├── ab2.tex
│   │   │   ├── ab2.txt
│   │   │   ├── corrtex.tex
│   │   │   ├── eh1.doc
│   │   │   ├── eh1.tex
│   │   │   ├── eh1.txt
│   │   │   ├── eh2.doc
│   │   │   ├── eh2.tex
│   │   │   ├── eh2.txt
│   │   │   ├── ehnl.doc
│   │   │   ├── ehnl.tex
│   │   │   └── ehnl.txt
│   │   ├── topwords_coded.csv
│   │   ├── tundatnls.csv
│   │   ├── tunisia_delegations_nightlights_GRCN.xlsx
│   │   ├── tunisia_delegations_nightlights_VIIRS.xlsx
│   │   ├── tunisia_shp_delegations1.dta
│   │   ├── tunusers.rds
│   │   ├── userids.rds
│   │   ├── userprofs.rds
│   │   └── vdem_beissdem.csv
│   ├── plots
│   │   ├── demnws.png
│   │   ├── demnws_combined.png
│   │   ├── demnws_keyness.png
│   │   ├── dempred.png
│   │   ├── dempredt.png
│   │   ├── demtwts.png
│   │   ├── demtwts_combined.png
│   │   ├── demtwts_combined_robust.png
│   │   ├── demtwts_keyness.png
│   │   ├── demtwts_keyness_robust.png
│   │   ├── demtwts_robust.png
│   │   ├── diffmap.png
│   │   ├── diffmapidr.png
│   │   ├── gcrn.png
│   │   ├── mlp.png
│   │   ├── rpqm1.png
│   │   ├── rpqm2.png
│   │   ├── rpqm3.png
│   │   ├── rpqm4.png
│   │   ├── rpqmnl.png
│   │   ├── rpsm1.png
│   │   ├── rpsm2.png
│   │   ├── rpsm3.png
│   │   ├── rpsm4.png
│   │   ├── rpsmnl.png
│   │   ├── twtmapstat.png
│   │   └── viirs.png
│   └── raw
│       ├── adbii_merged_data_file_english_final_0.dta
│       ├── nlrast.png
│       └── shapefiles
│           ├── delegations.cpg
│           ├── delegations.dbf
│           ├── delegations.shp
│           ├── delegations.shx
│           ├── tunhex1.dbf
│           ├── tunhex1.shp
│           └── tunhex1.shx
└── prp_PoP.Rproj
```