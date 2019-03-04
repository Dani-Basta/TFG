knnp : Time Series Prediction using K-Nearest Neighbors Algorithm (Parallel)
===========
First release has been developed as an End-of-Degree Project.

Purpose
----------
This package intends to provide R users or anyone interested in the field of time series prediction the possibility of aplying the k-nearest neighbors algorithm to time series prediction problems. Two main functionalities are provided:
- Time series prediction using this method.
- Optimization of parameteres *k* and *d* of the algorithm.

All the code involved has been optimized to:
- Parallelize critic components as the process of optimization of parameteres *k* and *d* or the calculation of distances.
- Use memory efficiently.

Authors
----------
- Daniel Bastarrica Lacalle
- Javier Berdecio Trigueros

License
----------
AGPL-3
