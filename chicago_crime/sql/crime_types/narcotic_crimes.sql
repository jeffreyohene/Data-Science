-- narcotic crimes rate yearly

WITH crime_counts AS (
  SELECT
    year,
    COUNT(*) AS total_crime_count
  FROM `bigquery-public-data.chicago_crime.crime`
  GROUP BY 1
),
-- narcotic crimes jährlich

narcotics_counts AS (
  SELECT
    year,
    COUNT(*) AS narcotics_count
  FROM `bigquery-public-data.chicago_crime.crime`
  WHERE primary_type = 'NARCOTICS'
  GROUP BY 1
)

SELECT
  c.year,
  IFNULL(n.narcotics_count, 0) AS narcotics_count,
  c.total_crime_count,
  ROUND(IFNULL((n.narcotics_count / c.total_crime_count) * 100, 0), 2) AS narcotics_rate
FROM (
  SELECT DISTINCT year
  FROM `bigquery-public-data.chicago_crime.crime`
  ORDER BY 1 DESC
) years
LEFT JOIN narcotics_counts n
ON years.year = n.year
LEFT JOIN crime_counts c
ON years.year = c.year
ORDER BY 4 DESC