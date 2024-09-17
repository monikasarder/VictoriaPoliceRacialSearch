* Encoding: UTF-8.
GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/2023 Data/HOPKINS-86053 Search/VicPol_Search_2023.sav'.


*Create found variable

NUMERIC Found (F1).
RECODE QuantityofitemFound (0 = 0) (1 THRU HI = 1) INTO Found.
EXECUTE.



*Create Operation_Type Variable


STRING Operation_Type (A20).
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'UNI') > 0 Operation_Type = 'Uniform'.
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'TRANSIT') > 0 AND CHAR.INDEX(UPCASE(ReportingStationDescription), 'PSO') = 0 Operation_Type = 'Transit'.
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'PSO') > 0 Operation_Type = 'PSO'.
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'CIU') > 0 Operation_Type = 'CIU'.
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'DRU') > 0 Operation_Type = 'DRU'.
IF CHAR.INDEX(UPCASE(ReportingStationDescription), 'HIGHWAY PATROL') > 0 OR CHAR.INDEX(UPCASE(ReportingStationDescription), 'HWY PATROL') > 0 Operation_Type = 'Highway Patrol'.
RECODE Operation_Type (' ' = 'Crime').
EXECUTE.


*Aggregate all searches of the same person at the same time into the one case
    

DATASET DECLARE Aggregate2023.
AGGREGATE
  /OUTFILE='Aggregate2023'
  /BREAK=RankofMember ReportingStationDescription ContactDate ContactTime FieldContactID 
    EthnicAppearance Age Gender IndigenousStatus HairColour HairStyle Complexion Operation_Type
  /ContactType_first=FIRST(ContactType) 
  /FieldContactSearchType_first=FIRST(FieldContactSearchType) 
  /FieldContactSearchType_A_first=FIRST(FieldContactSearchType_A) 
  /FieldContactCode_first=FIRST(FieldContactCode) 
  /FieldContactCodeDescription_first=FIRST(FieldContactCodeDescription) 
  /QuantityofitemFound_mean=MEAN(QuantityofitemFound) 
  /Found_mean=MEAN(Found).

*sort new file by date and time of search

DATASET ACTIVATE Aggregate2023.
SORT CASES BY ContactDate(A) ContactTime(A).


*create a new found variable

NUMERIC Found (F1).
RECODE Found_mean (0 = 0) (ELSE = 1) INTO Found.
EXECUTE.


CROSSTABS
  /TABLES=EthnicAppearance BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.


CROSSTABS
  /TABLES=Operation_Type BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=FieldContactCodeDescription_first BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.


*Create Aboriginal Total Variable (all perception and known Aboriginal people)


STRING ATSI_Total (A20).
IF CHAR.INDEX(UPCASE(EthnicAppearance), 'ABORIGINAL/T.S. ISLANDER') > 0 ATSI_Total = 'ATSI'.
IF CHAR.INDEX(UPCASE(IndigenousStatus), 'IND') > 0 ATSI_Total = 'ATSI'.
EXECUTE.




DATASET ACTIVATE Aggregate2023.
DATASET COPY  Personsearchcases2023.
DATASET ACTIVATE  Personsearchcases2023.
FILTER OFF.
USE ALL.
SELECT IF (ContactType_first="P").
EXECUTE.


SELECT IF ContactDate < 07-Oct-2023.
EXECUTE.

SELECT IF ContactDate < DATE.DMY(07, 10, 2023).
EXECUTE

CROSSTABS
  /TABLES=EthnicAppearance BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.
