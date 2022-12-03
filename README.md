## Optimizing aerial imagery collection and processing parameters for drone-based individual tree mapping in structurally complex conifer forests

This is a collection of scripts for evaluating how different drone imagery collection and processing parameters influence the quality of tree maps generated from drone imagery. The workflow outlined below describes the functions of all the scripts and the data files they depend on and produce. The data files supporting (and generated by) these scripts are available via [OSF](https://osf.io/kb3nj/). Throughout the workflow description, `{data}` refers to the root level of the data folder. All data references in the script are appended to that root data folder location.

## Workflow
1) **Collect drone imagery from the study site** with multiple missions with different mission parameters (e.g., flight altitude, gimbal angle). Fly each mission at high image overlap so that photosets can be thinned later to evaluate the influence of image overlap. The flight mission parameters used to acquire each photo set are recorded in `{data}/parameter_set_definitions/imagery-collection-log.csv`.
3) **Thin mission photosets** by selecting every *i*th photo in every *j*th transect (done by `scripts/thin_drone_photoset.R`). Thinned photosets are in `{data}/drone_image_sets_thinned/`. The nomenclature for the thinned photo sets is "setnumber_thin##", where the two thinning numbers, in order, are the forward thinning factor and the side thinning factor. For example, "set14_thin24" is photo set 14 thinned by a factor of 2 in the forward direction and a factor of 4 in the side direction.
4) **Create composite photosets** by combininng photo sets that have already been thinned (e.g., a nadir and oblique photo set). The individual photo sets used to create each composite photo set are recorded in `{data}/parameter_set_definitions/imagery-collection-log.csv`. Composite photo sets are also stored in `{data}/drone_image_sets_thinned/`. Composite photo sets have a four-digit thinning code. The first two numbers are the forward thinning factors of the two contributing photo sets (in the order specified in `{data}/parameter_set_definitions/imagery-collection-log.csv`, and the second two numbers are the side thinning factors of the two photo sets.
5) **Prepare photo sets for photogrammetric processing** by adding GCP and DEM data into the root folder of each photo set as specified in the documentation of the [UC Davis Metashape scripts](https://github.com/ucdavis/metashape).
6) **Photogrammetrically process each thinned photoset in Metashape** to produce a digital surface model (DSM) and point cloud using different sets of Metashape parameters. Definitions of the flight and processig parameters associated with each set of Metashape outputs are in `{data}/parameter_set_definitions/photoset-processing-params.csv`. The photoset thinning codes specified in that file coorespond to the photoset naming conventions as described in **Thin mission photosets* and **Create composite photosets** above. Metashape is run using the [UC Davis Metashape scripts](https://github.com/ucdavis/metashape). This workflow involves using detailed YAML configuration files for each Metashape run to document the processing parameters and enable reproduction. The configuration files used are in `{data}/metashape_configs`. Runs were performed using [v0.1.0](https://github.com/ucdavis/metashape/releases/tag/v0.1.0) of the scripts. [This step is performed on a GPU machine.]
7) **Post-process the photogrammetric products** by cropping the DSM to the focal area, normalizing by subtracting the terrain elevation (from a USGS DEM) to yield a canopy height model, and upscaling it to a 0.12 m resolution (done by `scripts/crop_and_noralize_dsm.R`); and by cropping the point cloud to the focal area, decimating it to 50 pts per sq m, and normalizing it by subtracting the terrain elevation (done by `scripts/crop_and_thin_las.R`). The normalization approach we use is reasonable because the UC Davis Metashape scripts GCP workflow ties the drone imagery elevation to the USGS DEM elevation, and only GCPs at or very close to bare ground are used. The resulting products are in `{data}/metashape_outputs_postprocessed/`. For the CHMs, the naming convention is {photo set}\_{processing parameters}\_{Metashape processed date-time}\_{layer type}. The processing parameters value is composed of four or five digits. The final two digits correspond to the metashape processing parameter set (defined in `{data}/parameter_set_definitions/photoset-processing-params.csv`). The initial one or two digits (following the "1" that precedes all values) indicates the photoset thinning factors used (also as defined in `{data}/parameter_set_definitions/photoset-processing-params.csv`). For the point clouds, the naming convention is the same except there is not a preceding "1". [This step is performed on a GPU machine.]
9) **Process the canopy height model and point clouds into maps of estimated tree locations** using different tree detection algorithms. Scripts for this step are in `scripts/tree-detection`. The tree detection scripts use the tree detection methods and their parameterizations as described in `{data}/parameter_set_definitions/vwfdefs_fullrange.csv` and `{data}/parameter_set_definitions/best_las.csv`. [This computationally intensive step is performed on a HPC cluster.]
10) **Generate a ground-truth map of tree locations** from a ground-based survey of the study area. The ground-based plot data are converted to a spatial map of tree locations by `scripts/ground_survey_to_spatial.R`. The ground survey plot locations recorded by GPS may be inaccurate, so for the purposes of stem map creation, the plot locations are manually placed into more appropriate locations based on the knowledge that they composed a square grid with regular spacing (corrected plot centers: `{data}/ground_truth/interpreted/plots_manuallyCorrectedComplete.geojson`). Some spatial error in plot locations remains, so the next step is to compare the resulting stem map against an initial "referene" drone-derived stem map (`{data}/reference_drone_stem_map/treetops_vwf001.geojson`) and manually shift the ground-survey locations of the trees trees that are clearly apparent in both the drone-based and ground-based stem map, compute summaries of the shifts by plot to get an optimal amount by which to spatially shift each plot (done by `scripts/check_ground_survey_rectification.R`, yielding `{data}/reference_alignment_eval/tree_shift_dir_summary.csv`), then shift all trees referenced from each plot center based on the optimal shift determined for each plot center (performend by `scripts/rectify_ground_survey.R`, yielding `{data}/ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson`). Finally, ground-truth trees are classified as "dominant" or "not dominant" based on their height and position relative to neighbors (performed by `scripts/ground_stem_map_detect_under_neighbors.R`, which updates the rectified stem map data file).
12) **Compare the drone-derived stem maps to the ground-truth stem map** and compute validation accuracy metrics. This is performed by `scripts/tree-map-comparison/run-tree-map-comparison.R` (with functions defined in the scripts in `scripts/tree-map-comparison/lib`), which produces a table of validation metrics for a specified drone stem map. This script can also optionally save a geospatial "stem map pairing lines" file, with lines that connect each drone tree to the ground tree it was paired with, and a data table listing all of the paired trees (one row for each paired tree), primarily for computing tree height prediction accuracy. The individual validation metrics files (one for each drone stem map) can then be combined into a single CSV table by `scripts/compile_comparison_stats.R`, to yield `{data}/drone_map_evals/compiled/comparison_stats_metashapeEval.csv`.
1) **Evaluate parameters influencing tree detection performance** and make figures and tables for manuscript. First, evaluate the influence of Metashape photogrammetry processing parameters (using `{scripts}/summarize_comparison_stats_metashapeEval.R`). Then, evaluate the influcnce of image overlap, flight altitude, and camera pitch (using `{scripts}/summarize_comparison_stats_alt_pitch_overlap.R`). Finally, evaluate drone-measured tree height accuracy (using `scripts/evaluate_drone_tree_heights.R`. The figures and tables generated by these scripts are stored in the `{data}/figures` and `{data}/tables` folders.

## General code notes
* All scripts source `scripts/convenience_functions.R`, which provides some convenience functions that are useful across multiple scripts.
* The scripts are set up to use the directory specified in `data_dir.txt` (in the root repo folder) as the root directory for data files to read and/or write. This functionality (provided by the `here` package combined with the `data()` function definition in `scripts/convenience_functions.R`) allows you to specify data sub-folders as arguments to the `data()` function, which then returns the full absolute system path.
* Some runnable scripts have a corresponding "functions" script that defines the functions used in the runnable script. These scripts are named the same as the runnable script, with "\_functions" appended to the filename.
