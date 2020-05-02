# screen-analysis-for-yellow-Tmaze-and-Joystick
These are all the scripts that I have been using lately for different reason to analyze different aspects from the screens and comparing them.

The important script for running the Joystick is called saurabh_modified_Christian.R.
It should run automatically once you set the following variables:
-data_path
-filename_data: this is a txt file with two columns separated by tab (the first one for the data filenames and the second for the group it belongs to). Here an example:

blue11.dat		blue1
blue12.dat		blue1
blue31.dat		blue3
blue32.dat		blue3
blue41.dat		blue4
blue42.dat		blue4
darkgreen21.dat		darkgreen2
darkgreen22.dat		darkgreen2
darkpink35.dat		darkpink3
darkpink41.dat		darkpink4
darkpink42.dat		darkpink4

-flat_thres: is a threshold to set in arbitrary units necessary for all_false function. Changing this threshold changes the strictness of flat signal condition.
-PI_thres: it can go from 0 to 2, where two means that no fly is rejected and 0 means all are rejected unless their pretest PI is exactly 0.
-Hysteresis: You can set the Hysteresis of your experiments here.

The output of this script consist of several tables/graphs:
-Line plot with individual fly PIs over the ten segments stratified by each group
-Box plot with summarised PIs stratified by group, with each point indicating a single fly.
-Boxplot only considering the PIs of the training periods (3,4,7,8) without normalising by pretest
-Boxplot & barplot considering the PIs of the training periods (3,4,7,8) with pretest normalisation/substraction (1,2)
-Boxplot & barplot of Wiggle difference between lights on and off
-Reinforcement boxplot
-Wiggle difference normalised
-Overall PI boxplot for the whole dataset to check if there is any experimental bias
-Barplot with the number of experiments for each group
