# FacialXAI_Mediation_Analysis
This GitHub performs the mediation analysis to understand how users behave when exposed to XAI components such as saliency maps. Please see our paper "Evaluation of a Deep Learning and XAI based Facial Phenotyping Tool for Genetic Syndromes: A Clinical User Study" [here on medrxiv](https://www.medrxiv.org/content/10.1101/2025.06.08.25328588v1). 

## How to reproduce analysis
The code was tested with R 4.5.0

```
├── output/*
├── ai_correct_final_ai_cohort_bootstrapping.r
├── ai_correct_final_xai_cohort_bootstrapping.r
├── ai_correct_final_xai_presence_bootstrapping.r
├── final_ai_cohort_bootstrapping.r
├── final_xai_cohort_bootstrapping.r
├── final_xai_presence_bootstrapping.r
├── raw_*.csv: edited Qualtrics output
```

Run R scripts [ai_cohort_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/final_ai_cohort_bootstrapping.r), [xai_cohort_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/final_xai_cohort_bootstrapping.r), [xai_presence_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/final_ai_presence_bootstrapping.r) to perform mediation analysis for Supplementary Figures 6A, 6B, and 6C and create Supplementary Tables 4, 5, and 6. 

Run R scripts [ai_correct_ai_cohort_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/ai_correct_final_ai_cohort_bootstrapping.r), ai_correct_[xai_cohort_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/ai_correct_final_xai_cohort_bootstrapping.r), ai_correct_[xai_presence_bootstrapping.r](https://github.com/jacie-cheng/FacialXAI_Mediation_Analysis/blob/main/ai_correct_final_ai_presence_bootstrapping.r) to perform mediation analysis for Supplementary Figures 6A, 6B, and 6C and create Supplementary Tables 8, 9, and 10, where dataset is conditioned on AI-correct only and user-hard images. 

All output files can be found in 'output' folder.
