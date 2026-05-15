# VicPol Stop and Search Dataset and Racial Profiling Analysis

[📥 Download full cleaned dataset (XLSX)](https://raw.githubusercontent.com/monikasarder/VictoriaPoliceRacialSearch/main/VicPol%20Search%20Data%20Clean/VicPol%20Search%20Data%20Clean.xlsx)

## About the Dataset

This repository contains:

- A cleaned version of Victoria Police stop-and-search data (50,000+ **warrantless searches** from 2018, 2019, 2022, 2023, 2024), processed in R to improve quality, standardise formats, and add geographic fields.  
- A racial profiling analysis of **drug searches of pedestrians by uniform police in Melbourne**, comparing outcomes for white- and African-appearing people using hit-rate methods.  

The original raw data was obtained under FOI by the [Racial Profiling Data Monitoring Project](https://www.racialprofilingresearch.org/) and is made publicly available there.

## Purpose of this Repository

This repository was developed to support public interest analysis and community-led accountability by providing:

- A cleaned and structured dataset of all warrantless police searches  
- An overview of the data structure, coverage, and key limitations  
- A focused analysis of **drug searches conducted on foot by uniform police**, assessing racial disparities using established 'hit rate' methodology  

## Repository Contents

- **[VicPol Racial Profiling Data.xlsx](https://raw.githubusercontent.com/monikasarder/VictoriaPoliceRacialSearch/main/VicPol%20Search%20Data%20Clean/VicPol%20Search%20Data%20Clean.xlsx)**  
  Cleaned and enriched dataset of all warrantless searches conducted by Victoria Police.

- **[R-code-cleaning/](R-code-cleaning/)**  
  R scripts used for cleaning, restructuring, and augmenting the FOI dataset.

- **[Data-summary-and-profiling-analysis/](Data-summary-and-profiling-analysis/)**  
  Folder containing all R Markdown analysis files, rendered outputs, and supporting materials:
  - **01 VicPol search data overview.Rmd** – Overview of all Victorian warrantless search data (50,000+ records).  
  - **02 Racial profiling – analysis and findings.Rmd** – Racial profiling analysis of drug searches of white and African-appearing pedestrians by uniform officers in Melbourne (7,534 records).  
  - **03 Missing data - pedestrian uniform drug and weapons searches Melbourne.Rmd** – Missingness of race and age information in drug and weapons searches of pedestrians carried out by uniform officers in Melbourne.

--- 

## Data Notes

### Search Types Included

The full dataset covers **warrantless searches** conducted under various Victorian laws, where police must form a **reasonable suspicion** that the person is carrying a **prohibited item** before searching.  

Some search powers apply **anywhere in public**, while others only apply in **specified locations or circumstances**.

---

#### **Anywhere in public (with reasonable suspicion)**

- **Drugs** – [Section 82 of the *Drugs, Poisons and Controlled Substances Act 1981*](https://classic.austlii.edu.au/au/legis/vic/consol_act/dpacsa1981422/s82.html)  
  *Allows searches in any public place if the officer reasonably suspects the person possesses a controlled drug.*

- **Weapons/Dangerous Articles** – [Section 10 of the *Control of Weapons Act 1990*](https://classic.austlii.edu.au/au/legis/vic/consol_act/cowa1990217/s10.html)  
  *Allows searches in any public place if the officer reasonably suspects the person possesses a controlled weapon, dangerous article, or prohibited weapon.*

---

#### **Specified circumstances only (with reasonable suspicion)**

- **Volatile Substances** – [Sections 60E & 60F of the *Drugs, Poisons and Controlled Substances Act 1981*](https://classic.austlii.edu.au/au/legis/vic/consol_act/dpacsa1981422/)  
  *Applies to people under 18 suspected of possessing or inhaling volatile substances (or possessing items used to inhale them), or to anyone suspected of supplying them to a child.*

- **Firearms** – [Section 112Q of the *Firearms Act 1996*](https://classic.austlii.edu.au/au/legis/vic/consol_act/fa1996102/s112q.html)  
  *Applies only to persons subject to a Firearm Prohibition Order (FPO) — allows searches of the person, their vehicle, or premises without a warrant.*

- **Graffiti Implements** – [Section 13 of the *Graffiti Prevention Act 2007*](https://classic.austlii.edu.au/au/legis/vic/consol_act/gpa2007217/s13.html)  
  *Applies where a person (aged 14+) is in a prescribed area such as public transport property or trespassing, and is suspected of possessing a prescribed graffiti implement.*

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
