SELECT
Q_EST_D_Standardized_Catch.Year,
Q_EST_D_Standardized_Catch.BED,

Count(Q_EST_D_Standardized_Catch.EVENT_ID)
	AS Number_of_Tows,

Sum(Q_EST_D_Standardized_Catch.TOW_LENGTH_DESIGNATED)
	AS Total_NM_Towed,

Sum(Q_EST_C_CC_Scallop_Summary.SumOfCOUNT)
	AS Raw_Count_Scallops,

Sum(Q_EST_C_CC_Scallop_Summary.SumOfSAMPLE_WT_KG)
	AS Raw_Wt_KG_Scallops,

Sum(Q_EST_D_Standardized_Catch.St_Count_Desig_Lngth) 
	AS Count_Scal_per_NM,

Sum(Q_EST_D_Standardized_Catch.St_Weight_Desig_Lngth) 
	AS Wt_KG_Scal_per_NM,

Avg(Q_EST_D_Standardized_Catch.St_Count_Desig_Lngth) 
	AS Mean_Count_Scal_per_NM,

Avg(Q_EST_D_Standardized_Catch.St_Weight_Desig_Lngth)
	AS Mean_Wt_KG_Scal_per_NM,

Var(Q_EST_D_Standardized_Catch.St_Count_Desig_Lngth) 
	AS Var_of_Count,

Var(Q_EST_D_Standardized_Catch.St_Weight_Desig_Lngth) 
	AS Var_of_Weight

INTO Q_EST_E_Summary_Using_Designated_Length

FROM Q_EST_D_Standardized_Catch 
	INNER JOIN Q_EST_C_CC_Scallop_Summary
		ON Q_EST_D_Standardized_Catch.EVENT_ID = Q_EST_C_CC_Scallop_Summary.EVENT_ID

GROUP BY
	Q_EST_D_Standardized_Catch.Year,
	Q_EST_D_Standardized_Catch.BED

ORDER BY 
	Q_EST_D_Standardized_Catch.Year,
	Q_EST_D_Standardized_Catch.BED;
