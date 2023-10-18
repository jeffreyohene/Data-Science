-- highest ranked crimes(primary_type and description) yearly

WITH ranked_crimes AS (
  SELECT
    year,
    primary_type,
    description,
    COUNT(*) AS arrests_count,
    RANK() OVER (PARTITION BY year ORDER BY COUNT(*) DESC) AS rank_within_year
  FROM `bigquery-public-data.chicago_crime.crime`
  WHERE arrest = TRUE
  GROUP BY 1,2,3
)

SELECT
  year,
  primary_type,
  description,
  arrests_count
FROM ranked_crimes
WHERE rank_within_year = 1
ORDER BY 1