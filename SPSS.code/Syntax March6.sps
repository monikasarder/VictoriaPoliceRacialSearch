* Encoding: UTF-8.

*Use this code to create a dataset with the 2018 data.  NB you first need to save 'VicPol Search 2018/2019' as 'VicPol2018/2019' because the select if command deletes all the other data.

GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/VicPol2018:2019.sav'.
SELECT IF $CASENUM <= 13475.
SAVE OUTFILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/Vic_pol_search_2018.sav'.



*Use this code to create the folder with the 2019 data. 

GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/VicPol2018:2019.sav'.
SELECT IF CHAR.INDEX(Contact.Date, '2019') > 0.
SAVE OUTFILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/Vic_pol_search_2019.sav'.

*Use this code to create a crosstab of hit rate by race and hit rate by operation type

GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/2018 Data/Vic_pol_search_2018.sav'.

CROSSTABS
  /TABLES=RacialCat BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=Operation_Type BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.

GET FILE='/Users/tamarhopkins2011/Desktop/Report cards FOIs/2019 Data/Vic_pol_search_2019.sav'.
CROSSTABS
  /TABLES=RacialCat BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=Operation_Type BY Found
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.
