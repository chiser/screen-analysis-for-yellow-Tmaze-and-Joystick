# screen-analysis-for-yellow-Tmaze-and-Joystick
These are all the scripts that I have been using lately for different reason to analyze different aspects from the screens and comparing them.

## Joystick
The important script for running the Joystick data is called Joystick_screen_analysis.R
It should run automatically once you set the following variables:

  - data_path
  
  - filename_data: by default it will look for the ./data/Joystick/all.txt which contains the experiments for this study.

Optional parameters to modify:

  - flat_thres: is a threshold to set in arbitrary units necessary for all_false function. Changing this threshold changes the strictness of flat signal condition.
  
  - PI_thres: it can go from 0 to 2, where two means that no fly is rejected and 0 means all are rejected unless their pretest PI is exactly 0.
  
  - Hysteresis: You can set the Hysteresis of your experiments here.
  
  - unblind: by default TRUE in case if you want to see the real names from the blinded experiment.

The output of this script consist of several tables/graphs but only one graph will be saved in eps
format in the current directory (barplot from Figure 3 in paper) with its data in a text file (joystick_screen_results.txt). All the plots are:

  - Line plot with individual fly PIs over the ten segments stratified by each group
  
  - Box plot with summarised PIs stratified by group, with each point indicating a single fly.
  
  - Boxplot only considering the PIs of the training periods (3,4,7,8) without normalising by pretest
  
  - Boxplot & barplot considering the PIs of the training periods (3,4,7,8) with pretest normalisation/substraction (1,2)
  
  - Boxplot & barplot of Wiggle difference between lights on and off
  
  - Reinforcement boxplot
  
  - Wiggle difference (between lights off and on) normalised
  
  - Overall PI boxplot for the whole dataset to check if there is any experimental bias
  
  - Barplot with the number of experiments for each group


## T-maze with yellow light 
The analysis is in the script Tmaze_yellow_screen_analysis.r. As an input it requires only to adapt the working directory variable to the path where your repository is. Then it will read automatically the data from /data/yellow_T_maze. There are no input parameter to change and as an output you get:

  - Two barplots showing the weighted average and the normal average. The latter will be saved in .eps format for the publication.

  - ATTENTION: Many csv files will be created with the name of the blinded names (eg. blue1.csv) in the working directory. You can use them if you need them or just delete them.
  


### Additional scripts from the unused folder:

plot_mean_trace_per_line.R is a script for the Joystick data for plotting the mean position across time as a line chart and a boxplot with the standard deviation of the position for each of the 10 experiment blocks. It needs as an input:

  - Set the right working directory so that it is in the main folder of the repository, otherwise the script won´t work.
  
  - Set the right file to read. By default it will point to ./data/Joystick/positive_control.txt
  
  - If you want to analyze different data you need to create a text file in the form as in ./data/Joystick/positive_control.txt
  
  - Change the names of the plot

The script takes into account the hysteresis which is by default 0 and it calculates the following steps:

  1. Save in state variable if the light is on or off taking care of hysteresis.
  
  2. Check for several data quality conditions in order to keep/discard the recording:
  
    - Condition 1: if the fly is inactive for too long shown as a flat line (inactive/no fly movement).
    
    - Condition 2: Calculate PIs and sort the ones with good pretest, that means when the pretest is not very biased.
    
    - Condition 3: Tally light encounters in the first training period
  3. Make line plots for the mean trace and a boxplot for the standard deviation of the traces.
  
  

