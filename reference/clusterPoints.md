# Identify clusters of points

`clusterPoints()` partitions points in a "points" `GVector` into
clusters.

## Usage

``` r
# S4 method for class 'GVector'
clusterPoints(x, method = "DBSCAN", minIn = NULL, maxDist = NULL)
```

## Arguments

- x:

  A "points" `GVector`.

- method:

  Character: Method used to identify clusters. Explanations of methods
  are provided in the help page for the **GRASS** tool `v.cluster`,
  available using `grassHelp("v.cluster")`.

  - `"DBSCAN"` (default): Density-Based Spatial Clustering of
    Applications with Noise.

  - `"DBSCAN2"`: A modification of DBSCAN.

  - `"density"`: Cluster points by relative density.

  - `"OPTICS"`: Ordering Points to Identify the Clustering Structure

  - `"OPTICS2"`: A modification of OPTICS.

  Case is ignored, but partial matching is not used.

- minIn:

  Integer, numeric integer, or `NULL` (default): Minimum number of
  points in a cluster. If `NULL`, then `minIn` is set to 3 for a
  2-dimensional vector and 4 for a 3-dimensional vector.

- maxDist:

  Numeric or `NULL` (default): Maximum distance between neighboring
  points in a cluster for DBSCAN, DBSCAN2, and OPTICS. If `NULL`, the
  maximum distance will be set to the 99th quantile of observed pairwise
  distances between points.

## Value

A vector of integers indicating the cluster to which each point belongs.

## See also

**GRASS** manual page for tool `v.cluster` (see
`grassHelp("v.cluster")`)
