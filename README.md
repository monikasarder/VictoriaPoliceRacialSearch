# VicPol Stop and Search Dataset

[📥 **Download Cleaned Dataset (XLSX)**](VicPol%20Search%20Data%20Clean/VicPol%20Search%20Data%20Clean.xlsx?raw=true)
 

## About the Dataset

This repository contains my cleaned and enriched version of Victoria Police stop-and-search data — over 50,000 **warrantless searches** conducted in 2018, 2019, 2022, and 2023. The dataset has been processed in R to improve data quality, standardise formats, and add geographic location fields. It is designed to support public, academic, and community stakeholders in assessing patterns of policing and the potential presence of racial profiling in Victoria.

The **original raw data** was obtained under the Freedom of Information Act by the [Racial Profiling Data Monitoring Project](https://www.racialprofilingresearch.org/) (previously hosted by the Centre Against Racial Profiling) and is made available there “free for use by the public.”

## Purpose of this Repository

This repository was developed to support public interest analysis and community-led accountability by providing:

- A cleaned and structured dataset of all warrantless police searches  
- An overview of the data structure, coverage, and key limitations  
- A focused analysis of **drug searches conducted on foot by uniform police**, assessing racial disparities using established 'hit rate' methodology  

## Repository Contents

- **VicPol Racial Profiling Data.xlsx** – Cleaned dataset of all warrantless searches across all police units  
- **Data Overview** – Summary of variables, search types, and coverage limitations  
- **Data Analysis** – Subset analysis of drug searches by uniform police, including hit rates by race and location  

---

## Data Notes

### Search Types Included

This dataset covers **warrantless searches**, where police are required by law to form a **reasonable suspicion** that the person is carrying a **prohibited item** before conducting the search. The dataset includes the following categories:

- **Drugs** – Section 82 of the *Drugs, Poisons and Controlled Substances Act 1981*  
- **Volatile Substances** – For people under 18, under Sections 60E and 60F of the same Act  
- **Weapons/Dangerous Articles** – Section 10 of the *Control of Weapons Act 1990*  
- **Firearms** – Section 149 of the *Firearms Act 1996*  
- **Graffiti Implements** – Section 13 of the *Graffiti Prevention Act*  

Approximately 90% of the records relate to **drug-related searches**.

### Racial Appearance Categories

Racial appearance is recorded based on the officer’s **perception** at the time of the search, using a fixed set of predefined categories. These do not reflect the individual’s self-identified race or ethnicity.

### Missing Data

Although racial appearance has been a required field since 2019, **42% of all search records (22,117 entries)** are missing this information. In some cases, officers selected “Other” when uncertain.

### Limitations in Middle Eastern Categorisation

Since 2020, Victoria Police have combined "Mediterranean" and "Middle Eastern" categories into **"Mediterranean/Middle Eastern"**, meaning outcomes for people perceived to be of Middle Eastern or 'Arab' appearance are not visible in the data.

## Interpreting Search Locations

The dataset includes police station-level location data for searches conducted by **uniform police** only. Location data is not available for specialist units (e.g., Highway Patrol, CIU, SOCIT), as these units often operate across multiple suburbs or LGAs within a broader Police Division.  

The included analysis focuses specifically on **foot-based drug searches conducted by uniform officers**, where station-level location data is most reliable.

---
## License

This repository contains both **code** and **data**, licensed under separate terms:

### 🔹 Code
All code in this repository is © 2025 Monika Sarder and licensed under the [MIT License](https://opensource.org/licenses/MIT).  
You are free to use, modify, and distribute the code with proper attribution.

### 🔹 Data
The **cleaned and enriched dataset** is © 2025 Monika Sarder, created by processing, restructuring, and augmenting the FOI dataset originally obtained by the [Racial Profiling Data Monitoring Project](https://www.racialprofilingresearch.org/) from Victoria Police.  

You are free to:  

- **Share** — copy and redistribute the material in any medium or format  
- **Adapt** — remix, transform, and build upon the material **for non-commercial purposes only**, with attribution to both Monika Sarder (for the cleaned dataset) and the Racial Profiling Data Monitoring Project (for the original FOI data).

**Commercial use is prohibited without written permission.**  
For commercial licensing, please contact: monikasarder@gmail.com
