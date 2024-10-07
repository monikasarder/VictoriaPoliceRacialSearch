# Victoria Police Search Data 

This dataset, which contains the summary of over 50,000 searches carried out by Victoria Police in the years 2018,2019, 2022 and 2023, was prepared as part of [**The Racial Profiling Data Monitoring Project**](https://khaki-pepper-b3rf.squarespace.com/), a project of the [Centre Against Racial Profiling](https://www.centreagainstracialprofiling.au/).  For more information about this dataset, and the significant resources and effort required to obtain it, go to [Annual Data](https://khaki-pepper-b3rf.squarespace.com/annual-data), or to donate to the project [here](https://khaki-pepper-b3rf.squarespace.com/checkout/donate?donatePageId=666baedd575e81339bc863ff). 

## Purpose of this repository  

This repository was prepared to provide data and *R code* that can be used by stakeholders interested in police accountability with the means and methods to transform raw police search data into useable information to understand racial profiling in their communities. Station location information and Police Division hierarchy data not present in the raw dataset has also been added.  

The raw datasets that can be downloaded from the [**The Racial Profiling Data Monitoring Project**](https://khaki-pepper-b3rf.squarespace.com/).  

Repository inclusions are as follows:  

*  The clean dataset is available as the file *VicPol Racial Profiling Data.xlsx*   
*  **R Code** this folder includes R Code to transform the data  
*  **Primary datasets** this folder includes raw search data from VicPol  
*  **Secondary datasets**  this folder includes secondary datasets that enhanced station related information in the search dataset  
*  **Data and hit rate overview** this Rmarkdown includes a summary of key variables and racialised differences in searches and 'hit rates' between areas.  Government agencies, researchers, advocacy organisations and data journalists are encouraged to adapt and amend this code to ask targeted questions of the data.  


## Notes on data in scope 

A few notes on legal powers, racial categories and station location data information are included below.  

### Search types in scope  
The five types of searches without a warrant that are included in this dataset are summarised below. They all represent stop and searches that Victoria Police can undertake where there is 'reasonable grounds' for the officer to suspect that the person was in possession of a prohibited item. Note that almost ninety percent of searches are drug related. 

* **Drugs**: under the section 82 of the *Drugs Poisons and Controlled Substances Act 1981* 
* **Volatile substances for inhalation** for use by a person under 18: under sections 60E and 60F of the *Drugs Poisons and Controlled Substances Act 1981*  
* **Weapons or dangerous articles**: under *Section 10 Control of Weapons Act 1990*  
* **Firearms**: under section 149 of the *Firearms Act 1996*   
* **Graffiti implements** at a designated place: under section 13 of the *Graffiti Prevention Act*  

### Racial appearance categories created by Victoria Police  

Victoria Police records the ethnic appearance of the people that they search without warrant based on existing categories. These categories are not based on ancestry information, but on *perceived ethnicity* as considered by the officer. 

***Missing data***

A total of 22,117 search records, or 42% of all records, did not have any ethnic appearance data entered, despite this field being mandatory for 'stop and searches' since 2019. Records where the officer was unable to determine the ethnic appearance were marked as 'Other'.

***Middle Eastern appearance data limitations***  

There is a limitation on our ability to analyse police-targeting of people of Middle Eastern appearance, as in 2020 Victoria Police aggregated Mediterranean and Middle Eastern into a single category.  This presents a challenge to our analysis, as we would ordinarily expect persons of Mediterranean descent (eg people of Italian, Greek, Croatian ancestry) to be categorised as 'non-racialised' or 'White'.  Meanwhile we would ordinarily expect persons of the Middle Eastern appearance to be considered 'racialised'.  

### Station location versus location of search   

Station location information is provided where the unit which the officer conducting the search was a part of is based in a local police station. This data is available for searches conducted by uniform police as well as searches conducted by police working as part of a specific unit such as Highway Patrol, Divisional Response Unit (DRU), Criminal Investigation Unit (CIU) and Sexual Offences and Child Abuse Investigation Teams (SOCIT).  

Most searches conducted by uniform police will occur within the Local Government Area in which the station is located.  Meanwhile, searches conducted by police working in specific units may have occurred in surrounding areas within the broader Police Division (ie across several Local Government Areas).  

## Contact  

For further queries or notes as to how the accuracy of this work can be improved, please drop me a line on monikasarder@gmail.com 