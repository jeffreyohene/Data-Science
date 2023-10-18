-- arrests by week day

SELECT
  CASE EXTRACT(DAYOFWEEK FROM date)
    WHEN 1 THEN 'Sunday'
    WHEN 2 THEN 'Monday'
    WHEN 3 THEN 'Tuesday'
    WHEN 4 THEN 'Wednesday'
    WHEN 5 THEN 'Thursday'
    WHEN 6 THEN 'Friday'
    WHEN 7 THEN 'Saturday'
    ELSE 'invalid'
  END AS day_of_week,
  COUNT(*) AS arrests_count
FROM `bigquery-public-data.chicago_crime.crime`
WHERE arrest = TRUE
GROUP BY day_of_week
ORDER BY arrests_count DESC