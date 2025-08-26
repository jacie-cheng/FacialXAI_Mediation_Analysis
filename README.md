# FacialXAI_Mediation_Analysis
This repository contains the code and data for the supplementary section of the paper "Evaluation of a Deep Learning and XAI based Facial Phenotyping Tool for Genetic Syndromes: A Clinical User Study"

## How to reproduce analysis
The code was tested with Python 3.12 and R 4.5.0

```
├── input_data/
│   ├── raw_*.csv: edited Qualtrics output
├── output/*
├── ai_cohort_bootstrapping.r
├── xai_cohort_bootstrapping.r
├── xai_presence_bootstrapping.r
```

Raw Qualtric outputs are difficult to read, thus the data is extracted and saved in a more readable format
```python
python reformat_data_ai_only.py
python reformat_data_xai.py
python reformat_data_xai_presence.py
```
Bootstrap resampling with replacement can take ~5-10 minutes
```
Rscript ai_cohort_bootstrapping.r
Rscript xai_cohort_bootstrapping.r
Rscript xai_presence_bootstrapping.r
```
