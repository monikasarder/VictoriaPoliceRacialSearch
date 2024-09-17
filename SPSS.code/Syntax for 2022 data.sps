* Encoding: UTF-8.
GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/2022 Data/VicPol 2022 Search Data.sav'.

*Create found variable

NUMERIC Found (F1).
RECODE Quantity (0 = 0) (1 THRU HI = 1) INTO Found.
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
    

DATASET DECLARE Aggregate.
AGGREGATE
  /OUTFILE='Aggregate'
  /BREAK=RankofMember ReportingStationDescription ContactDate ContactTime FieldContactID 
    RacialAppearance Age Gender IndigenousStatus HairColour HairStyle Complexion Operation_Type
  /ContactType_first=FIRST(ContactType) 
  /FieldContactSearchType_first=FIRST(FieldContactSearchType) 
  /FieldContactSearchType_A_first=FIRST(FieldContactSearchType_A) 
  /FieldContactCode_first=FIRST(FieldContactCode) 
  /FieldContactCodeDescription_first=FIRST(FieldContactCodeDescription) 
  /Quantity_mean=MEAN(Quantity) 
  /Found_mean=MEAN(Found).

*sort new file by date and time of search

DATASET ACTIVATE Aggregate.
SORT CASES BY ContactDate(A) ContactTime(A).

*create a new found variable

NUMERIC Found (F1).
RECODE Found_mean (0 = 0) (ELSE = 1) INTO Found.
EXECUTE.


CROSSTABS
  /TABLES=RacialAppearance BY Found
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


STRING Aboriginal_Total (A20).
IF CHAR.INDEX(UPCASE(RacialAppearance), 'ABORIGINAL/T.S. ISLANDER') > 0 Aboriginal_Total = 'ABTSI'.
IF CHAR.INDEX(UPCASE(IndigenousStatus), 'IND') > 0 Aboriginal_Total = 'ABTSI'.
EXECUTE.

CROSSTABS
  /TABLES=Aboriginal_Total BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.



DATASET ACTIVATE Aggregate.
DATASET COPY  Personsearchcases2022.
DATASET ACTIVATE  Personsearchcases2022.
FILTER OFF.
USE ALL.
SELECT IF (ContactType_first="P").
EXECUTE.

