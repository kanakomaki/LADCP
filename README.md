# LADCP data processing program  (my original program)

Raw data processing programs for ocean current sensors, "Lowered acoustic Doppler current profilers (LADCPs)."
This set of programs is based on a processing and corrected method in the academic article by Komaki & Kawabe (2007).

LADCPs are standard sensors for measuring ocean currents in deeper layers, typically > 1000m deep, where ship-based acoustic current profilers cannot reach. However, the processing methods (to convert raw data to velocity profiles) are quite complex (e.g., Fischer and Visbeck 1993, Visbeck 2003) and most scientists are using "black-boxed" software packages. Also, these methods produce erroneous velocity results. My approach applies simple Fortan programs and a special "tuning" method for velocity "errors" using Echo Intensity data of original raw data.

