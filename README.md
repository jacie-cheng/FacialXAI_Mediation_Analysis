# FacialXAI_Mediation_Analysis
This GitHub performs the mediation analysis to understand how users behave when exposed to XAI components such as saliency maps. Please see our paper "Evaluation of a Deep Learning and XAI based Facial Phenotyping Tool for Genetic Syndromes: A Clinical User Study" [here on medrxiv](https://www.medrxiv.org/content/10.1101/2025.06.08.25328588v1). 

## How to reproduce analysis
The code was tested with Python 3.12 and R 4.5.0

```
├── output/*
├── raw_*.csv: edited Qualtrics output
├── ai_cohort_bootstrapping.r
├── xai_cohort_bootstrapping.r
├── xai_presence_bootstrapping.r
```

Run R scripts ai_cohort_bootstrapping.r, xai_cohort_bootstrapping.r, xai_presence_bootstrapping.r to perform mediation analysis for Supplementary Diagrams 4, 5, and 6, respectively. 
