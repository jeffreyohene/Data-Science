-- Arrests by location and percentage total

SELECT
  location_description,
  COUNT(*) AS arrests_count,
  ROUND(COUNT(*) / SUM(COUNT(*)) OVER () * 100, 1) AS pct_total
FROM `bigquery-public-data.chicago_crime.crime`
WHERE arrest = TRUE
GROUP BY 1
ORDER BY 2 DESC