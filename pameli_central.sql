-- créer une vue de la base
create materialized view flat_data as (
	select m.context_id, cm.reason, cm.date , s.sensor as sensor_name,  p.name as property_name, numeric_value as property_value, um.code as unit_code, um.name as unit_name, 
		point_id, points.longitude, points.latitude, points.depth, points.geom_point, points.sensor_time, points.local_time, points.is_reference 
	from 
		central.observations_measures m, 
		central.property_type_parameter p, 
		central.unit_of_measure um, 
		central.process s, 
		central.feature_of_interest_points points,
		central.context_mission cm
	where property_id = p.id and unit_id = um.id and process_id = s.id and point_id = points.id and m.context_id = cm.id
		-- and is_reference = true
		--and sensor = 'ysiexo2'  
		and points.geom_point is not null 
	order by s.sensor,  points.local_time
)-- 3 milion de lignes, 30 s

select context_id, reason, date, sensor_name, property_name, property_value,
unit_code, unit_name, point_id, st_x(st_transform(geom_point, 4326)) as pt_longitude, st_y(st_transform(geom_point, 4326)) as pt_latitude, 
st_transform(geom_point, 4326) as geom_point, local_time, is_reference 
from flat_data where context_id=1 and sensor_name = 'ysiexo2'  

-- Please note that for your TP you should not limit the extract to context_id = 1. You should have all the missions together. 
-- An maybe you should not limit to sensor_name 'ysiexo2'  
select distinct context_id, reason, date from flat_data
2	Practical work outing on Aytré with students	2021-02-25
1	Test for new configuration	2019-04-24

