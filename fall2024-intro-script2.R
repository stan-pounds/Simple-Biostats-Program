#########################################################
# Load the Simple Biostat Program (SBP) into this session of R
# Do this at the start of each R session you use SBP


sbp.link="https://raw.githubusercontent.com/stan-pounds/Simple-Biostats-Program/main/setup-SBP.R"
source(sbp.link)

#############################################################
# Read a data set into R

nki70=read.data() # open a windows browser (may be a hidden window on taskbar)
                  # user chooses a data file
                  # for an Excel file, user chooses a spreadsheet name
                  # for this exercise, choose "penalized.nki70"
                  # read that data set into R and call it nki70
                  # open a data preview window

############################################
# Describe the distribution of grade in this data set

describe(grade,nki70)   # simplest syntax: describe(variable.name,dataset.name)
                        
# output includes everything needed for a scientific paper
# draft narrative for the results
# a table 
# a figure
# a draft narrative for the methods
# bibliographic references

#################################
# Click the broom icon at the top of the plots window
# this will erase all the figures

###########################################
# fig option controls number of figures

describe(grade,nki70,fig=0) # fig=0 means zero figures
describe(grade,nki70,fig=1) # fig=1 means one figure (default)
describe(grade,nki70,fig=2) # fig=2 means two figures (or max number)
describe(grade,nki70,fig=3) # fig=3 means three figures (or max number)
describe(grade,nki70,fig=4) # fig=4 means four figures (or max number)

##########################################
# tbl option controls number of tables

describe(grade,nki70,tbl=0)  # option tbl=0 means no tables
describe(grade,nki70,tbl=1)  # option tbl=1 gives one table (default)
describe(grade,nki70,tbl=2)  # option tbl=2 gives two tables (or max number)

########################################
# txt option controls level of detail in the narrative text

describe(grade,nki70,txt=0) # option txt=0 gives no text
describe(grade,nki70,txt=1) # option txt=1 gives some text (default)
describe(grade,nki70,txt=2) # option txt=2 gives more text (if available)
describe(grade,nki70,txt=3) # option txt=3 gives even more text (if available)

########################################
# clr option controls colors

describe(grade,nki70,clr="red")                  # colors one bar red, others blank
describe(grade,nki70,clr=c("red","blue"))        # colors one bar red & one blue
describe(grade,nki70,clr=c("red","blue","gold")) # three colors, one for each bar

############################################
# More on colors

show.colors()    # produce a color name legend
show.palettes(3) # produce a legend of palletes of three colors

# a technical fix
par(mai=rep(0.25,4)) # make small margins (0.25 inches) for plots

###########################################
# use color palette for colors

describe(grade,nki70,clr="rainbow") # rainbow color palette
describe(grade,nki70,clr="Warm")    # Warm color palette (not warm, R is picky about capitalization)
describe(grade,nki70,clr="Cold")    # cold color palette
describe(grade,nki70,clr="Tropic")  # tropic color palette

##########################################
# use multiple options

describe(grade,nki70,  # describe the grade variable of the nki70 dataset
         txt=0,tbl=0,  # no text, no tables
         clr="Earth")  # use the Earth color palette for figures

describe(grade,nki70,       # describe the grade variable of the nki70 data set
         txt=0,tbl=0,       # no text, no tables
         fig=2,clr="Earth") # two figures with the Earth color palette


describe(grade,nki70,       # describe the grade variable of the nki70 data set
         txt=1,tbl=1,       # short text, one table
         fig=2,clr="Earth") # two figures with the Earth color palette

################################################
# now copy your results into Word

grade.result=describe(grade,nki70,  # name the result "grade.result"    
                      txt=1,tbl=1,  # we want text and tables
                      fig=2,        # we want two figures
                      clr="Earth")  # use the Earth color palette (or choose your own)

# highlight and copy text from R into Word
# To get Figure into word: click Export, Copy to Clipboard, paste in Word


word.table(grade.result) # outputs a table in a format to copy into Word
                         # **INSTRUCTIONS**
                         # 1. Copy the output into Word.
                         # 2. Highlight the output in Word.
                         # 3. Go to Insert>Table>Convert Text to Table.

################################################
# In-Class Task: describe age and get your results into Word
# Here's a start

describe(age,nki70,clr="skyblue")
describe(age,nki70,clr="skyblue",fig=2)




