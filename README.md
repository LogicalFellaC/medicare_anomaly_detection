Medicare Billings Anomaly Detection Project Description
================
Christopher Oh
2017-12-18

### Goal

Use Medicare billings data (over 9 million data points) to build an anomaly detection algorithm using unsupervised learning methods.

### Algorithm

Local density-based outlier scores - LOF, LDF, RKOF, LPDF

### Advantages

-   Automated generation of outlier metrics that do not depend on arbitrary criteria.
-   Easily scalable.

### Possible usage

-   Outlier detection on the current records.
-   Building a classifier for future stream of billing records.

### To Do

-   Scale it up to the full dataset
    -   Save the processed dataset to a separate file.
    -   Write a script for the processing (use the existing code).
    -   Write a batch script file to run on SLURM.
    -   Submit the batch request (through Stanford Wheat - large memeory node).
