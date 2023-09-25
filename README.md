# LADCP data processing program  (my original program)

Fotran-based data processing programs for ocean current sensors, "Lowered acoustic Doppler current profilers (LADCPs)."
This set of programs is based on a processing and corrected method in the academic article by Komaki & Kawabe (2007).

LADCPs are standard sensors for measuring ocean currents in deeper layers, typically > 1000m deep, where ship-based acoustic current profilers cannot reach. However, the processing methods (to convert raw data to velocity profiles) are quite complex (e.g., Fischer and Visbeck 1993, Visbeck 2003) and most scientists are using "black-boxed" software packages. Also, these methods produce erroneous velocity results. My approach applies simple Fortan programs and a special "tuning" method for velocity "errors" using Echo Intensity data of original raw data.


# How to run  
## Programs  
The repo contains the following series of programs. Basically, execute one by one along with this file order.  
1. ladcp1_n.f : The 1st program to execute data cleaning. You need Input & Output data.   
2. ladcp2_n.f : The 2nd program executes further data clearing with the 1st program's output. Error velocity filter.  
3. ladcp3_n.f : The 3rd program to further process the 2nd program's output. W velocity filter.  
4. ladcp4_n.f : Interpolate the velocity data (output from ladcp3.f) for 1m depth bins.   
5. ladcp5_n.f : Calculate "vertical velocity shears" at every 5m depth bin.   
6. ladcp6_n.f : Calculate the "absolute" velocity profile fitted to the bottom-tracked velocity at the sea bottom.  
7. ladcp7_n.f : Calculated the "relative" velocity profile.  
8. ladcp8_n.f : Corrected the velocity profile using Echo Intensity data.  


## Setting of input/output directories and data
### Directories
Input data directory     ./DAT/  
Output data directory    ./ANA/LADCP1/ ~ ./ANA/LADCP8/, ./ANA/ECHO/  

### Sample data (./DAT/)   
input  
./DAT/001.asc: LADCP ascii DATA (Processed by RDI's WinADCP software)  
./DAT/ctd.asc: CTD ascii DATA (TIME, DEPTH)   

output  
./DAT/LADCP1/001.vdw,  
./DAT/LADCP1/001.vbt,  
./DAT/LADCP1/001.vup  :  output velocity profiles during downcast, upcast, and the sea-bottom   
./DAT/LADCP1/001.edw,  
./DAT/LADCP1/001.ebt,  
./DAT/LADCP1/001.eup  :  output echo intensity profiles during downcast, upcast, and the sea-bottom   


Last update: 2023Sep24 1st uploaded to Github


