## About the Dataset

This dataset summarises over 50,000 searches conducted by Victoria Police in 2018, 2019, 2022, and 2023. It was compiled as part of the [**The Racial Profiling Data Monitoring Project**](https://khaki-pepper-b3rf.squarespace.com/), a project of the [Centre Against Racial Profiling](https://www.centreagainstracialprofiling.au/). 

## Purpose of This Repository

This repository was developed to support stakeholders working on police accountability by providing:

- Access to cleaned and raw police search data  
- R code to transform and analyse the data  
- Additional contextual information such as police station locations and Victoria Police’s organisational hierarchy  

The aim is to enable users to turn raw data into actionable insights about racial profiling in their communities.

## Contents of the Repository

- **VicPol Racial Profiling Data.xlsx** – the cleaned dataset  
- **R Code** – scripts for data cleaning and transformation  
- **Primary Datasets** – raw search data from Victoria Police  
- **Secondary Datasets** – supplementary data, including station location information and intermediate outputs from the cleaning process  
- **Data and Hit Rate Overview** – a preliminary analysis in RMarkdown format  

---

## Notes on the Data

### Search Types Included

The dataset covers five types of **warrantless searches**, where police must have *reasonable grounds* to suspect the person is carrying a prohibited item. Nearly **90% of these searches are drug-related**.

The search categories include:

- **Drugs** – under section 82 of the *Drugs, Poisons and Controlled Substances Act 1981*  
- **Volatile substances (inhalants)** – for persons under 18, under sections 60E and 60F of the same Act  
- **Weapons or dangerous articles** – under section 10 of the *Control of Weapons Act 1990*  
- **Firearms** – under section 149 of the *Firearms Act 1996*  
- **Graffiti implements** – in designated places, under section 13 of the *Graffiti Prevention Act*  

### Racial Appearance Categories

Victoria Police record the **perceived racial appearance** of individuals being searched, based on a predefined set of categories. These classifications reflect the officer’s perception rather than the individual’s self-identified ethnicity or ancestry.

### Missing Data

Despite being a mandatory field since 2019, **42% of search records (22,117 records)** have no racial appearance data. When officers were unsure of a person’s appearance, the entry was marked as "Other".

### Limitations in Data on Middle Eastern Appearance

Since 2020, Victoria Police have combined "Mediterranean" and "Middle Eastern" into a single category labeled **"Mediterranean/Middle Eastern"**. This change obscures the experiences of people specifically of Middle Eastern appearance and limits the ability to analyse targeted policing of this group.

---

## Interpreting Search Locations

The dataset includes the **station location** of the officer's unit, which may or may not reflect where the actual search occurred:

- **Uniformed police** typically conduct searches within the same Local Government Area (LGA) as their station.
- **Specialist units** (e.g., Highway Patrol, Divisional Response Unit, CIU, SOCIT) often operate across multiple LGAs within a broader Police Division, so searches may have taken place in surrounding areas.


