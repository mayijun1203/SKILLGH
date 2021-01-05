SELECT
    citibikesub.*,
    nybbwi.boroname
FROM
    dcptransportation.citibikesub,
    dcptransportation.nybbwi
WHERE
    ST_Intersects(citibikesub.the_geom, nybbwi.the_geom)
    and nybbwi.boroname = 'Queens'