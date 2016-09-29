SELECT 
DATA_SCALLOP_SURVEY_STATIONS.BED,
Sum(DATA_SCALLOP_SURVEY_STATIONS.Area_Sq_NM)
	AS SumOfArea_Sq_NM

FROM DATA_SCALLOP_SURVEY_STATIONS

GROUP BY DATA_SCALLOP_SURVEY_STATIONS.BED

ORDER BY DATA_SCALLOP_SURVEY_STATIONS.BED;
