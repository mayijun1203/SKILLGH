SELECT
    cartodb_id,
    the_geom,
    the_geom_webmercator,
    CASE
        WHEN e202010 >= 0
        AND e202010 <= 1000 THEN '0-1000'
        WHEN e202010 > 1000
        AND e202010 <= 2000 THEN '1001-2000'
        ELSE '>2000'
    END AS category
FROM
    dcptransportation.subwayridership