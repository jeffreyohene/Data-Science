# Python alternative with BQ

from google.cloud import bigquery
import pandas as pd

# Authenticate to Google Cloud (make sure you have your credentials set up)
client = bigquery.Client()

# Set your project ID and dataset name
project_id = "chicago"
dataset_name = "bigquery-public-data.chicago_taxi_trips"

# Define the SQL query to retrieve all rows
sql_query = f"SELECT * FROM `{project_id}.{dataset_name}.taxi_trips`" # Bulk download will take a while. Query can be adjusted with LIMIT SQL keyword

# Execute the query
query_job = client.query(sql_query)

# Fetch the results into a Pandas DataFrame
results = query_job.result().to_dataframe()

# Save the DataFrame to a CSV file
results.to_csv("./chicago/taxi_trips/taxi_trips2016.csv", index=False)
