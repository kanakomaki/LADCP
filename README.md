# LADCP data processing program  (my original program)

Fotran-based data processing programs for ocean current sensors, "Lowered acoustic Doppler current profilers (LADCPs)."
This set of programs is based on a processing and corrected method in the academic article by Komaki & Kawabe (2007).

LADCPs are standard sensors for measuring ocean currents in deeper layers, typically > 1000m deep, where ship-based acoustic current profilers cannot reach. However, the processing methods (to convert raw data to velocity profiles) are quite complex (e.g., Fischer and Visbeck 1993, Visbeck 2003) and most scientists are using "black-boxed" software packages. Also, these methods produce erroneous velocity results. My approach applies simple Fortan programs and a special "tuning" method for velocity "errors" using Echo Intensity data of original raw data.


# Procedure  
The repo contains the following series of programs.   
1. ladcp1.f : The 1st program to execute. You need Input & Output data described below.   
2. ladcp2.f : The 2nd program to further process the 1st program's output. 
3. ladcp3.f : The 3rd program to further process the 2nd program's output. 
4. ladcpnaiso.f : Interpolate the velocity data (output from ladcp3.f) with depths. 
5. ladcpashear5m.f : Calculate "vertical velocity shears" at every 5m bin. 
6. ladcpvel_for_fitting.f : Fit the velocity profile to the bottom-tracked velocity at the sea-bottom. 

# Input & Output data
1. ladcp1.f
input     ./DAT/001.asc: LADCP ascii DATA (Processed by RDI's WinADCP software)  
input     ./DAT/ctd.asc: CTD ascii DATA (TIME, DEPTH)   
output    ./DAT/LADCP1/001.vdw,./DAT/LADCP1/001.vbt,./DAT/LADCP1/001.vup  :  output velocity profiles during downcast, upcast, and the sea-bottom  
output    ./DAT/LADCP1/001.edw,./DAT/LADCP1/001.ebt,./DAT/LADCP1/001.eup  :  output echo intensity profiles during downcast, upcast, and the sea-bottom  




Last update: 2023Sep24 1st uploaded to Github


