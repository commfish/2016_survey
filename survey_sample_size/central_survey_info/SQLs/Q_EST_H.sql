SELECT 
Q_EST_G_table.Year,
Q_EST_G_table.BED,
Q_EST_G_table.SQNM_in_Bed,
Q_EST_G_table.Number_of_Tows,
Q_EST_G_table.Total_NM_Towed, 
Q_EST_G_table.AreaSwept, 
Q_EST_G_table.Raw_Count_Scallops, 
Q_EST_G_table.Raw_Wt_KG_Scallops, 
Q_EST_G_table.Count_Scal_per_NM, 
Q_EST_G_table.Wt_KG_Scal_per_NM, 
Q_EST_G_table.Mean_Count_Scal_per_NM, 
Q_EST_G_table.Mean_Wt_KG_Scal_per_NM, 
Q_EST_G_table.Var_of_Count, 
Q_EST_G_table.Var_of_Weight, 
(6076/8)*[Mean_Wt_KG_Scal_per_NM]*[SQNM_in_Bed]					AS POP_EST_Weight_KG,
((6076/8)*[Mean_Wt_KG_Scal_per_NM]*[SQNM_in_Bed])*2.20462262 	AS POP_EST_Weight_LB,
(6076/8)*[Mean_Count_Scal_per_NM]*[SQNM_in_Bed] 				AS POP_EST_Count,
([Var_of_Count]/[Number_of_Tows])*(1-[Number_of_Tows]/[Big_N])	AS Var_CBar_CT,
([Var_of_Weight]/[Number_of_Tows])*(1-[Number_of_Tows]/[Big_N])	AS Var_CBar_WT

INTO Q_EST_H_varCbar

FROM Q_EST_G_table;
