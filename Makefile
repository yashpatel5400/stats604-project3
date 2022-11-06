plots/plot1.png plots/plot2.png plots/plot3.png plots/plot4a.png plots/plot4b.png plots/plot4c.png plots/plot4d.png plots/plot4e.png plots/plot4f.png plots/plot5.png plots/plot6.png plots/plot7.png: processed_results/aggregated_measurements_black.csv \
	processed_results/aggregated_measurements_yellow.csv processed_results/aggregated_measurements_rgb.csv \
	permutations.csv make_plots.R
	Rscript make_plots.R

permutations.csv: processed_results/aggregated_measurements_black.csv processed_results/aggregated_measurements_yellow.csv \
	processed_results/aggregated_measurements_rgb.csv \
	permutation_test.R
	Rscript permutation_test.R

processed_results/aggregated_measurements_black.csv processed_results/aggregated_measurements_yellow.csv processed_results/aggregated_measurements_brown.csv processed_results/aggregated_measurements_rgb.csv: preprocessing.R
	Rscript preprocessing.R

clean:
	rm processed_results/aggregated_measurements_black.csv processed_results/aggregated_measurements_yellow.csv \
	processed_results/aggregated_measurements_rgb.csv \
	permutations.csv plots/*.png
