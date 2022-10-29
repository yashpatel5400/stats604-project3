plot*: permutations.csv make_plots.R
	Rscript make_plots.R

permutations: permutation_test.R
	Rscript permutation_test.R

aggregated_measurements: preprocessing.R
	Rscript preprocessing.R

clean:
	rm processed_results/aggregated_measurements.csv processed_results/aggregated_measurements_black.csv \
	processed_results/aggregated_measurements_yellow.csv processed_results/aggregated_measurements_brown.csv \
	processed_results/aggregated_measurements_rgb.csv permutations.csv
