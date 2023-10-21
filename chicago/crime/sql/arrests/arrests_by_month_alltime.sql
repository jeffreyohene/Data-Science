-- Arrests by month(all time)

SELECT
  CASE EXTRACT(MONTH FROM date)
    WHEN 1 THEN 'January'
    WHEN 2 THEN 'February'
    WHEN 3 THEN 'March'
    WHEN 4 THEN 'April'
    WHEN 5 THEN 'May'
    WHEN 6 THEN 'June'
    WHEN 7 THEN 'July'
    WHEN 8 THEN 'August'
    WHEN 9 THEN 'September'
    WHEN 10 THEN 'October'
    WHEN 11 THEN 'November'
    WHEN 12 THEN 'December'
    ELSE 'unknown'
  END AS month,
  COUNT(*) AS arrests_count
FROM `bigquery-public-data.chicago_crime.crime`
WHERE arrest = TRUE
GROUP BY 1
ORDER BY 2 DESC