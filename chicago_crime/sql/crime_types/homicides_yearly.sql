-- homicide rate yearly

WITH crime_counts AS (
  SELECT
    year,
    COUNT(*) AS total_crime_count
  FROM `bigquery-public-data.chicago_crime.crime`
  GROUP BY 1
),

homicide_counts AS (
  SELECT
    year,
    COUNT(*) AS homicide_count
  FROM `bigquery-public-data.chicago_crime.crime`
  WHERE primary_type = 'HOMICIDE'
  GROUP BY 1
)

SELECT
  c.year,
  IFNULL(h.homicide_count, 0) AS homicide_count,
  c.total_crime_count,
  ROUND(IFNULL((h.homicide_count / c.total_crime_count) * 100, 0), 2) AS homicide_rate
FROM (
  SELECT DISTINCT year
  FROM `bigquery-public-data.chicago_crime.crime`
  ORDER BY year DESC
) years
LEFT JOIN homicide_counts h
ON years.year = h.year
LEFT JOIN crime_counts c
ON years.year = c.year
ORDER BY 4 desc
