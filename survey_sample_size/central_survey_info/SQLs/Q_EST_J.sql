SELECT 
Q_EST_I_PopEst.Year,
Q_EST_I_PopEst.BED,
Q_EST_I_PopEst.SQNM_in_Bed,
Q_EST_I_PopEst.Number_of_Tows,
Q_EST_I_PopEst.Total_NM_Towed,
Q_EST_I_PopEst.AreaSwept, 
Q_EST_I_PopEst.Raw_Count_Scallops, 
Q_EST_I_PopEst.Raw_Wt_KG_Scallops, 
Q_EST_I_PopEst.Count_Scal_per_NM, 
Q_EST_I_PopEst.Wt_KG_Scal_per_NM, 
Q_EST_I_PopEst.Mean_Count_Scal_per_NM, 
Q_EST_I_PopEst.Mean_Wt_KG_Scal_per_NM, 
Q_EST_I_PopEst.Var_of_Count, 
Q_EST_I_PopEst.Var_of_Weight, 
Q_EST_I_PopEst.CI_95_meanCount, 
Q_EST_I_PopEst.CI_95_meanWeight, 
Q_EST_I_PopEst.POP_EST_Weight_KG, 
Q_EST_I_PopEst.POP_EST_Count, 
Q_EST_I_PopEst.Var_CBar_CT, 
Q_EST_I_PopEst.Var_CBar_WT, 
Q_EST_I_PopEst.Variance_Pop_Count, 
Q_EST_I_PopEst.Variance_Pop_Wt, 
[LUT_STUDENTS_T_DIST].[95_2tail]*Sqr([Variance_Pop_Count]) 	AS CI_95_POP_Count, 
[LUT_STUDENTS_T_DIST].[95_2tail]*Sqr([Variance_Pop_Wt]) 	AS CI_95_POP_Weight, 
(Sqr([Variance_Pop_Count]))/[POP_EST_Count] 				AS CV_Count, 
(Sqr([Variance_Pop_Wt]))/[POP_EST_Weight_KG] 				AS CV_Weight 

INTO Q_EST_J_CI_and_CV

FROM LUT_STUDENTS_T_DIST 

INNER JOIN Q_EST_I_PopEst 
	ON LUT_STUDENTS_T_DIST.N = Q_EST_I_PopEst.Number_of_Tows;
