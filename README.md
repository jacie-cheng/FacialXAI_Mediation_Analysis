# FacialXAI_Mediation_Analysis
This repository contains the code and data for the supplementary section of the paper "Evaluation of a Deep Learning and XAI based Facial Phenotyping Tool for Genetic Syndromes: A Clinical User Study"

## How to reproduce analysis
The code was tested with Python 3.12 and R 4.5.0

```
├── output/*
├── raw_*.csv: edited Qualtrics output
├── ai_cohort_bootstrapping.r
├── xai_cohort_bootstrapping.r
├── xai_presence_bootstrapping.r
```

Bootstrap resampling with replacement can take ~5-10 minutes
```
Rscript ai_cohort_bootstrapping.r
Rscript xai_cohort_bootstrapping.r
Rscript xai_presence_bootstrapping.r
```
