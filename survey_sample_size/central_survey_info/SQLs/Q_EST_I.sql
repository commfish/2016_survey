SELECT 
Q_EST_H_varCbar.Year,
Q_EST_H_varCbar.BED,
Q_EST_H_varCbar.SQNM_in_Bed,
Q_EST_H_varCbar.Number_of_Tows,
Q_EST_H_varCbar.Total_NM_Towed,
Q_EST_H_varCbar.AreaSwept,
Q_EST_H_varCbar.Raw_Count_Scallops,
Q_EST_H_varCbar.Raw_Wt_KG_Scallops,
Q_EST_H_varCbar.Count_Scal_per_NM,
Q_EST_H_varCbar.Wt_KG_Scal_per_NM,
Q_EST_H_varCbar.Mean_Count_Scal_per_NM,
Q_EST_H_varCbar.Mean_Wt_KG_Scal_per_NM,
Q_EST_H_varCbar.Var_of_Count,
Q_EST_H_varCbar.Var_of_Weight,
Q_EST_H_varCbar.POP_EST_Weight_KG,
Q_EST_H_varCbar.POP_EST_Weight_LB,
Q_EST_H_varCbar.POP_EST_Count,
Q_EST_H_varCbar.Var_CBar_CT, 
Q_EST_H_varCbar.Var_CBar_WT,
[LUT_STUDENTS_T_DIST].[95_2tail]*Sqr([Var_CBar_CT]) 	AS CI_95_meanCount,
[LUT_STUDENTS_T_DIST].[95_2tail]*Sqr([Var_CBar_Wt]) 	AS CI_95_meanWeight,
([Var_CBar_CT])*([SQNM_in_Bed]*(6076/8))^2 				AS Variance_Pop_Count,
([Var_CBar_WT])*([SQNM_in_Bed]*(6076/8))^2 				AS Variance_Pop_Wt

INTO Q_EST_I_PopEst
FROM LUT_STUDENTS_T_DIST 
	INNER JOIN Q_EST_H_varCbar 
		ON LUT_STUDENTS_T_DIST.N = Q_EST_H_varCbar.Number_of_Tows;
