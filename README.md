# BIG_Communities

Data and code for the manuscript **“Identity‑by‑descent captures shared environmental factors at biobank scale.”**  
Pre‑print: <https://doi.org/10.1101/2025.05.03.652048>

This project is part of the **Biorepository, Informatics & Genomics (BIG) initiative at the University of Tennessee Health Science Center**.  
Project home: <https://uthsc.edu/cbmi/big/>

## What’s in this repository?
- **aggregated data & scripts** to reproduce the analysis at biobank scale. Identity‑by‑descent (IBD) clustering.  
  - four continent‑scale genetic communities, and  
  - seventeen sub‑communities that share both genetic architecture and distinct environmental exposures.  
  - A **Streamlit dashboard** (`subcommunity_explorer_app.py`) to interactively explore genetic, environmental and health patterns at sub‑community resolution.

## Quick start
```bash
## Quick start
# activate your conda environment (or create one)
conda activate myenv

# install dependencies
pip install streamlit pandas numpy

# run the dashboard
streamlit run subcommunity_explorer_app.py
```

<img width="1576" height="935" alt="BIG_dashboard" src="https://github.com/user-attachments/assets/8959c0f8-7a93-49a0-8f59-ddf362dab50a" />

Note: Final dashboard will be deployed in a web, at this stage the data are for illustrative purposes

fmarsico@uthsc.edu
