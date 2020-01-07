This is (/will be) a collection of scripts for evaluating how different drone imagery collection and processing parameters influence the quality of automatically generated tree stem maps.

## Workflow
1) **Fly the study site** with multiple missions with different mission parameters (e.g., flight altitude, gimbal angle). Fly each mission at high image overlap so that photosets can be thinned later to additionally evaluate the influence of image overlap.
2) **Thin mission photosets** by selecting every *i*th photo in every *j*th transect (done by scripts/thin_drone_photoset.R).
3) **Process each thinned photoset in Metashape** to produce a digital surface model (DSM) and point cloud using different sets of Metashape parameters. Outputs from Metashape are stored in `{data}/metashape_products/`. Logs/definitions of the flight and processig parameters associated with each set of Metashape outputs are in `{data}/parameter_set_definitions/`. Metashape is run using the [UC Davis Metashape scripts](https://github.com/ucdavis/metashape).
4) **Convert the Metashape DSM to a canopy height model** (CHM) by using a 10 m USGS DEM to define the terrain elevation and subtracting that from the DSM (first interpolate \[downscale\] the USGS DEM to the Metashape DSM) (done by scripts/metashape_dsm_to_chm.R). This is a reasonable approach because the UC Davis Metashape GCP workflow ties the drone imagery elevation to the USGS DEM elevation, and only GCPs at or very close to bare ground are used. The resulting "Metashape CHM" is in `{data}/post_metashape_products`.
5) **Process the digital surface model and/or point cloud into maps of estimated tree locations** using different tree detection algorithms. Scripts that do this are in `scripts/tree_detection`. Estimated tree stem map files are in `post_metashape_products/paramsetXXX/` where `paramsetXXX` is the name of the set of drone mission and metashape parameters yielding the data files from which the stem map was computed (defined in `{data}/parameter_set_definitions/`).
6) **Generate a map of tree stem locations from a ground-truth survey of the study area**. Done by `scripts/ground_survey_to_spatial.R`.
8) **Spatially warp the ground-truth stem map to move trees into their "true" locations** (TO DO).
7) **Compare the drone-derived stem maps to the ground-truth stem map** (TO DO).

Data are stored on Box at: https://ucdavis.box.com/s/yxgngz750ommo73kz5akeac0q7ci5wgi (contact me for access)
