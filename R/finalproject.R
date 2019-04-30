### CollegeFindR uses the largest publically available dataset with over 200 college descriptors for over 6000 colleges in the USA.
### The purpose of the program is to ask various questions and subset from the main dataset to help identify which colleges
### meet all of the interested students preferences such as location, size, student population and more.

### Results include names of schools, the school's website and some visualizations to show results.


library("readxl")
library("taRifx")
library("ggplot2")


collegedata <<- read_excel("collegedata.xlsx")

survey <- function () {
  region()
  size()
  majoritystudents()
  price()
  scores()
  results()
  visualize()
}  

  
#region
#
#subset by region of school
#
#@return subset of original data frame where only remaining are those within the user specified region
#
#@export
#
region <- function() {
  print("What region would you prefer to go to school in? 1. New England 2. Mid East 3. Great Lakes 4. Plains 5. Southeast 6. Southwest 7. Rocky Mountains 8. Far West 9. Outlying Areas 10. Doesn't matter")
  
  region <- readline(prompt=">>> ")
  
  if (region == 1) {
    locationlist <<- subset(collegedata, REGION == 1)
  }  else if (region == 2) {
    locationlist <<- subset(collegedata, REGION == 2)
  }  else if (region == 3) {
    locationlist <<- subset(collegedata, REGION == 3)
  }  else if (region == 4) {
    locationlist <<- subset(collegedata, REGION == 4)
  }  else if (region == 5) {
    locationlist <<- subset(collegedata, REGION == 5)
  }  else if (region == 6) {
    locationlist <<- subset(collegedata, REGION == 6)
  }  else if (region == 7) {
    locationlist <<- subset(collegedata, REGION == 7)
  }  else if (region == 8) {
    locationlist <<- subset(collegedata, REGION == 8)
  }  else if (region == 9) {
    locationlist <<- subset(collegedata, REGION == 9)
  }  else if (region == 10) {
    locationlist <<- collegedata
  }  else {
    print("That is not a valid selection. Please try again")
    region()
}

}

#size
#
#subset by school size
#
#@return a subset of the region dataframe where only data remaining is that of the specified school size
#
#@export
#
size <- function() {  
  print("What size school are you looking for? 1. Very Small 2. Small 3. Medium 4. Large 5. Doesn't matter")
  
  size <- readline(prompt=">>> ")
  
  if (size == 1) {
    slschoollist <<- subset(locationlist, CCSIZSET == 1 | CCSIZSET == 6 | CCSIZSET == 7
                            | CCSIZSET == 8)
  } else if (size == 2) {
    slschoollist <<- subset(locationlist, CCSIZSET == 2 | CCSIZSET == 9 | CCSIZSET == 10
                           | CCSIZSET == 11)
  } else if (size == 3) {
    slschoollist <<- subset(locationlist, CCSIZSET == 3 | CCSIZSET == 12 | CCSIZSET == 13
                           | CCSIZSET == 14)
  } else if (size == 4) {
    slschoollist <<- subset(locationlist, CCSIZSET == 4 | CCSIZSET == 15 | CCSIZSET == 16
                            | CCSIZSET == 17 | CCSIZSET == 5)
  } else if (size == 5) {
    slschoollist <<- locationlist
  } else {
    print("That is not a valid selection. Please try again")
    size()
  }  
} 

#majority students
#
#subset by majority undergraduate/graduate students
#
#@return subset of size data frame where only remaining data is the specified type of student population
#
#@export
#
majoritystudents <- function() {
  print("What type of students would you prefer? 1. Majority undergraduate 2. Majority graduate 3. Doesn't matter")
  
  uggstud <<- readline(prompt=">>> ")
  
  midlist <- data.frame(lapply(slschoollist, function(x) {
    gsub("NA", NA, x)}))
  
  thelist <<- midlist[complete.cases(midlist[,122]),]
  actuallist <<- thelist[complete.cases(thelist[,83]),]
  
  
  thislist <<- as.numeric(actuallist$GRADS)
  finallist <<- as.numeric(actuallist$UG)
  
  UGGr <- finallist - thislist
  
  newslschoollist <- cbind(actuallist,UGGr)
    
  if (uggstud == 1) {
    uggslschools <<- subset(newslschoollist, UGGr >= 0)
  } else if (uggstud == 2) {
    uggslschools <<- subset(newslschoollist, UGGr < 0)
  } else if (uggstud == 3) {
    uggslschools <<- newslschoollist
  } else {
    print("That is not a valid selection. Please try again")
    majoritystudents()
  }  
  
}

#price
#
#subset by price
#
#return a subset of majority students data frame only including schools within the specified price range
#
#@export
#
price <- function() {
  
  print("What state can you claim in-state tuition in? (Please use the 2 letter state abbreviation.)")
  current <<- readline(prompt=">>> ")
  
  print("What is your tuition and fees limit per year? Answer in whole dollar amounts without a comma. Example: 13500")
  maxtuition <<- readline(">>> ")
   
  uggslschools <<- remove.factors(uggslschools)
   
  if (current == uggslschools$STABBR) {
    partone <<- subset(uggslschools, TUITIONFEE_IN <= maxtuition)
    parttwo <<- subset(uggslschools, TUITIONFEE_OUT <= maxtuition)
    pricelist <<- rbind(partone, parttwo)
  } else {
    pricelist <<- subset(uggslschools, TUITIONFEE_OUT <= maxtuition)
    }
  
}


#test score
#
#subset by test scores
#
#@return subset of price data frame only including schools where student score above midline on ACT or SAT
#
#@export
#
scores <- function() {
  print("Would you like to only show schools where your ACT & sAT score are above the midline? 1. ACT only 2. SAT only 3. Both 4. No")
  answer <<- readline(">>> ")
  
  if (answer == 1) {
    print("What is your cumulative ACT score?")
    act <<- readline(">>> ")
    
    scorelist <<- subset(pricelist, ACTCMMID <= act)
    
  } else if (answer == 2){
    print("What is your cumulative SAT score?")
    sat <<- readline(">>> ")
    
    scorelist <<- subset(pricelist, SAT_AVG <= sat)
    
  } else if (answer == 3){
    print("What is your cumulative ACT score?")
    act <<- readline(">>> ")
    
    print("What is your cumulative SAT score?")
    sat <<- readline(">>> ")
    
    firstlist <<- subset(pricelist, ACTCMMID <= act)
    scorelist <<- subset(firstlist, SAT_AVG <= sat)
    
  }  else if (answer == 4) {
     scorelist <<- pricelist
  }  else {
     print("That is not a valid selection. Please try again") 
     scores()
  }   

}

#results
#
#presents how many schools are left with the specified above criteria
#
#returns a character string
#
#@export
#
results <- function() {
  if (nrow(scorelist) < 1) {
    print("I'm sorry, no schools match this search criteria. Please start over.")
    survey()
  } else { 
    a = nrow(scorelist)
    sprintf("There are %i schools that match your search criteria.", a)
  }
  
}  


#visualize  
#  
#visualize results
#
#returns a bar chart of results and a list of schools with their website that meet all of the above specified criteria
#
#@export
#
visualize <- function () {
  
print("Here are your suggested schools!")

  
#region results
  
ggplot(data=scorelist) + geom_bar(mapping=aes(x=STABBR, fill="#000099", colour="black")) + ggtitle("Distribution of Results by State") +
  theme(legend.position="none") + ylab("Number of Schools") + xlab("State")


#prints table with name and website
scorelist[,(c(1,4))]

  
}



survey()

