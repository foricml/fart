To get the result for simulation, we just need submit simulation.R (on server, need to change number of cores if want to run on personal laptop).

The result will write to table_1.txt,..., table_6.txt.

To get the result for real data, we just need submit real_data.R. The result will be printed out.

Available data:

airfoil_self_noise.dat.txt: This is a NASA data set, obtained from a series of aerodynamic and acoustic tests of two and
three-dimensional airfoil blade sections conducted in an anechoic wind tunnel (Dua & Graff, 2017). Five features were selected to predict the aerofoil noise. We used 1000 observations as the training data set and 503 observations as test data.

Concrete_Data.xls: In civil engineering, concrete is the most important material (Yeh, 1998). This data set consists of eight features to predict the concrete compressive strength. We split it into a training set of size 750 and a test set of size 280.

Folds5x2_pp.xlsx: CCPP data. This data set contains 9568 data points collected from a Combined Cycle Power Plant over six years (2006-2011), when the power plant was set to work with full load (T¨ufekci, 2014; Kaya et al., 2012).

auto-mpg.data.txt: This data set contains eight features to predict city-cycle fuel consumption in miles per gallon (Asuncion & Newman, 2007; Dua & Graff, 2017). After discarded samples with missing entries, we split the rest of the observations into a training set of size 314 and a test set of size 78.

Boston House data is not included. It can be obtained from R package MASS.


