SELECT 
Mean_Wt_KG_Scal_per_NM]*2.2046			AS Mean_Wt_LB_per_NM,
[POP_EST_Weight_KG]*2.2046 				AS POP_EST_Weight_LB,
Q_EST_J_CI_and_CV.Year,
Q_EST_J_CI_and_CV.BED,
Q_EST_J_CI_and_CV.SQNM_in_Bed,
Q_EST_J_CI_and_CV.Number_of_Tows, 
Q_EST_J_CI_and_CV.Total_NM_Towed, 
Q_EST_J_CI_and_CV.AreaSwept, 
Q_EST_J_CI_and_CV.Raw_Count_Scallops, 
Q_EST_J_CI_and_CV.Raw_Wt_KG_Scallops, 
Q_EST_J_CI_and_CV.Count_Scal_per_NM, 
Q_EST_J_CI_and_CV.Wt_KG_Scal_per_NM, 
Q_EST_J_CI_and_CV.Mean_Count_Scal_per_NM, 
Q_EST_J_CI_and_CV.Mean_Wt_KG_Scal_per_NM, 
Q_EST_J_CI_and_CV.Var_of_Count, 
Q_EST_J_CI_and_CV.Var_of_Weight, 
Q_EST_J_CI_and_CV.CI_95_meanCount, 
Q_EST_J_CI_and_CV.CI_95_meanWeight, 
Q_EST_J_CI_and_CV.POP_EST_Weight_KG, 
Q_EST_J_CI_and_CV.POP_EST_Count, 
Q_EST_J_CI_and_CV.Var_CBar_CT, 
Q_EST_J_CI_and_CV.Var_CBar_WT, 
Q_EST_J_CI_and_CV.Variance_Pop_Count, 
Q_EST_J_CI_and_CV.Variance_Pop_Wt, 
Q_EST_J_CI_and_CV.CI_95_POP_Count, 
Q_EST_J_CI_and_CV.CI_95_POP_Weight, 
Q_EST_J_CI_and_CV.CV_Count, 
Q_EST_J_CI_and_CV.CV_Weight, 
[Q_EST_K_MR].[MEAT_WT]/[WHOLE_WT] 			AS MR_RATE, 
[POP_EST_Weight_KG]*[MR_RATE] 				AS MEAT_EST_KG, 
([POP_EST_Weight_KG]*[MR_RATE])*2.20462262 	AS MEAT_EST_LB 

INTO Q_EST_L_withMR
FROM Q_EST_J_CI_and_CV 
	INNER JOIN Q_EST_K_MR 
		ON (Q_EST_K_MR.BED = Q_EST_J_CI_and_CV.BED) 
		AND (Q_EST_J_CI_and_CV.Year = Q_EST_K_MR.YEAR);
