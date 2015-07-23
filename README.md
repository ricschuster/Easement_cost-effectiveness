# Effects of disputes and reasement violations on the cost-effectiveness of land conservation

[![DOI](https://zenodo.org/badge/15744/yeronimo/Easement_cost-effectiveness.svg)](https://zenodo.org/badge/latestdoi/15744/yeronimo/Easement_cost-effectiveness)

This repository includes the data and files for the example analysis of the following paper published in PeerJ:

Schuster and Arcese (2015) Effects of disputes and easement violations on the cost-effectiveness of land conservation. PeerJ x:xxxx

Please be advised that if you re-run our analysis you will get (slightly) different results, because we randomized the order of the property costs due to a restrictive licence share agreement, that does not allow to share the property cost data with others. The analysis steps will be identical though.

File description:

**00.Marxan.batch.v3.r**: this is the batch function to run Marxan that was used to create the land purchase scenarios for this analysis. The input data for these are not included in this repository, but have been documented in a previous paper:
Schuster R, Martin TG, Arcese P (2014) Bird Community Conservation and Carbon Offsets in Western North America. PLoS ONE 9(6): e99292. doi: 10.1371/journal.pone.0099292 
http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0099292

**Covenants_Acqu_1000_runs_red.RData**: has the workspace holding the cleaned results from running 00.Marxan.batch.v3.r. This workspace represents the basis for all further analysis.

**04.Covenant.analysis.all.runs.RM.const.high.2.final.r**: This represents a script for the main analysis, in this case for the high dispute rate scenario. Files low and medium are almost identical and essentially only chance the dispute rate.

**Polygon_level_Area_Carbon_Biodiv_values_cost_randomized.csv**: Input file required for 04 scripts. It has information on the size, randomized monetary value and biodiversity features for each property used for the easement analysis.

**05.Covenant.results.for.paper_PE_DE_alternate.R**: Script used to calculate most numerical results presented in the paper.

**06.combine.plot.ggplot2.ribbon.Fig2-4_PE_DE_alternate.R**: R script for creating the figures used in the PeerJ paper.
