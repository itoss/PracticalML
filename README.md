###THE GOAL
This project can be treated as classification problem
A lot of algorithm can be used for solving but at this time we will use the simplest one : a tree classification for baseline prediction and random forest for testing and submission


###DATA PREPARATION AND CLEANING
Reading the training .csv file with a text editor can alarm us on structure and form of the data
although, we can see that inexisting data has two form : the standard NA and #DIV/0 (which might be an error generated by spreadsheet)

So we have to **change manually the #DIV/0 to NA** wich was done outside the R script

After that, the **first seven column can be ignored** because they represent only a data reference and can slow down the computation and generate inaccuracy 

Also, the summary() command tell us that some of variable had a **lot of NA** in their observation so we have to **get rid** of them


###METHOD AND ALGORITHM
As baseline method we can use tree prediction
But tree is not accurracy as we might think
After reading a lot, the random forest with cross-validation option might be the best way to deal with this Human Activity Recognition classification project. It raised up the accuracy but tended to take time on computation
The Data Cleaning above improve a lot the computational time.

Ten fold cross validation have been choosed and entire data have been used

For proof of concept, we can split data (example, taking 20% of training set) and do less fold (5 will be sufficient)

More detail can be viewed on [the annotated R script](https://github.com/itoss/PracticalML/blob/master/HAR_project.R)) 
