SELECT
DATA_EVENTS_SCALLOP_DREDGE.Year,
DATA_EVENTS_SCALLOP_DREDGE.EVENT_ID,
DATA_EVENTS_SCALLOP_DREDGE.BED,
DATA_EVENTS_SCALLOP_DREDGE.STATION_ID,
DATA_EVENTS_SCALLOP_DREDGE.USED_IN_ESTIMATE

INTO Q_EST_A_Tows_Used_in_Est

FROM DATA_EVENTS_SCALLOP_DREDGE

WHERE (((DATA_EVENTS_SCALLOP_DREDGE.USED_IN_ESTIMATE)="YES"))

ORDER BY DATA_EVENTS_SCALLOP_DREDGE.Year,
 DATA_EVENTS_SCALLOP_DREDGE.EVENT_ID DESC,
 DATA_EVENTS_SCALLOP_DREDGE.BED,
 DATA_EVENTS_SCALLOP_DREDGE.STATION_ID;