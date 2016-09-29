SELECT
Q_EST_A_Tows_Used_in_Est.Year,
Q_EST_A_Tows_Used_in_Est.BED,
Count(Q_EST_A_Tows_Used_in_Est.USED_IN_ESTIMATE) AS Count_of_Tows

INTO Q_EST_B_Summary_Count_Stations

FROM Q_EST_A_Tows_Used_in_Est

GROUP BY Q_EST_A_Tows_Used_in_Est.Year, Q_EST_A_Tows_Used_in_Est.BED

ORDER BY Q_EST_A_Tows_Used_in_Est.Year, Q_EST_A_Tows_Used_in_Est.BED;
