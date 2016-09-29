SELECT DISTINCT
Q_EST_L_withMR.Year,
DATA_EVENTS_SCALLOP_DREDGE.PROJECT_CODE						AS PROJECT_CODE,
Q_EST_L_withMR.BED,
Round([Q_EST_L_withMR].[SQNM_in_Bed],2) 					AS Sq_NM_in_Bed,
Q_EST_L_withMR.Number_of_Tows, 
Round([Q_EST_L_withMR].[Total_NM_Towed],2) 					AS Total_NM_Towed,
Round([Q_EST_L_withMR].[AreaSwept],6) 						AS Area_Swept_Sq_NM,
Q_EST_L_withMR.Raw_Count_Scallops,
Round([Q_EST_L_withMR].[Wt_KG_Scal_per_NM],1) 				AS Wt_KG_Scal_per_NM,
Round([Q_EST_L_withMR].[Mean_Count_Scal_per_NM],2) 			AS Mean_Count_Scal_per_NM,
Round([Q_EST_L_withMR].[Mean_Wt_KG_Scal_per_NM],1) 			AS Mean_Wt_KG_Scal_per_NM,
Format([Q_EST_L_withMR].[Var_of_Count],'Standard') 			AS Var_of_Count,
Format([Q_EST_L_withMR].[Var_of_Weight],'Standard') 		AS Var_of_Wt,
Format(Round([Q_EST_L_withMR].[CI_95_meanCount]),'Standard')	AS CI_95_meanCount,
Format([Q_EST_L_withMR].[CI_95_meanWeight],'Standard') 		AS CI_95_meanWeight,
Format([Q_EST_L_withMR].[POP_EST_Weight_KG],'Standard') 	AS POP_EST_Weight_KG, 
Format([Q_EST_L_withMR].[POP_EST_Weight_LB],'Standard') 	AS POP_EST_Weight_LB, 
Format(Round([Q_EST_L_withMR].[POP_EST_Count]),'Standard') 	AS POP_EST_Count, 
Format([Q_EST_L_withMR].[Var_CBar_CT],'Standard') 			AS Variance_Cbar_Count, 
Format([Q_EST_L_withMR].[Var_CBar_WT],'Standard') 			AS Variance_Cbar_Weight, 
Format([Q_EST_L_withMR].[Variance_Pop_Count],'Standard')	AS Variance_Pop_Count, 
Format([Q_EST_L_withMR].[Variance_Pop_Wt],'Standard') 		AS Variance_Pop_Wt, 
Format(Round([Q_EST_L_withMR].[CI_95_POP_Count]),'Standard')AS CI_95_POP_Count, 
Format([Q_EST_L_withMR].[CI_95_POP_Weight],'Standard') 		AS CI_95_POP_Weight, 
Format(Round([Q_EST_L_withMR].[CV_Count],2),'Standard') 				AS CV_Count, 
Format([Q_EST_L_withMR].[CV_Weight],'Standard') 			AS CV_Weight, 
Round([Q_EST_L_withMR].[MR_RATE],3) 						AS MR_RATE, 

Round([POP_EST_Count]
	/([Q_EST_L_withMR].[SQNM_in_Bed]*3429904),3)			AS ScalDens,

Round([POP_EST_Weight_KG]*1000
	/[POP_EST_Count])										AS AvgWtScal,
										
Format([Q_EST_L_withMR].[MEAT_EST_KG],'Standard')			AS MEAT_EST_KG, 
Format([Q_EST_L_withMR].[MEAT_EST_LB],'Standard') 			AS MEAT_EST_LB 

INTO Q_EST_M_REFORMATTED_TABLE

FROM Q_EST_L_withMR

INNER JOIN DATA_EVENTS_SCALLOP_DREDGE
	ON (Q_EST_L_withMR.Year = DATA_EVENTS_SCALLOP_DREDGE.YEAR)
	AND (Q_EST_L_withMR.BED = DATA_EVENTS_SCALLOP_DREDGE.BED)

ORDER BY	
	PROJECT_CODE, Q_EST_L_withMR.BED, Q_EST_L_withMR.Year
;
