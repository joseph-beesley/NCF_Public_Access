Path networks created in QGIS for ease.

Process:

1. Import access points and road shapefiles for AOI
2. Buffer access points 30m
3. Intersect buffer with road
4. Find centroid points
5. Run 'Service area (from layer)' with centroid as starting point and road as network. Calculate 'Shortest' path and specify distance under Travel cost