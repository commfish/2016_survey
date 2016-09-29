SELECT
Right([SURVEY_AND_YEAR],4) 						AS [YEAR],
DATA_SCALLOP_MEAT_RECOVERY.BED,
Sum(DATA_SCALLOP_MEAT_RECOVERY.WHOLE_WT_KG) 	AS WHOLE_WT,
Sum(DATA_SCALLOP_MEAT_RECOVERY.MEAT_WT_KG) 		AS MEAT_WT

INTO Q_EST_K_MR

FROM DATA_SCALLOP_MEAT_RECOVERY

GROUP BY Right([SURVEY_AND_YEAR],4), DATA_SCALLOP_MEAT_RECOVERY.BED

ORDER BY Right([SURVEY_AND_YEAR],4);
