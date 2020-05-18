# Cobol-Challenge-2---UNEMP
Unemployment Stats Challenge - Programs, JCL, Data
I love :coffee:.

My initial approach to Challenge #2, Unemployment Stats:

Step 1
	• Created JCLUSTER.jcl to create KSDS VSAM file
	• Created JCLUSDEL.jcl to delete it
	• Because I don't appear to have a utility to view the VSAM file, I created a COBOL program UNEMPDB to read and view the contents of it
	• Downloaded 5 stat files from www.data.gov (in CSV format for now) and created 5 separate datasets for input
	• Created UNEMP.cbl program to process the 5 input files and populate the VSAM file
	• NOTES:
		○ I used the UNSTRING function to extract the distinct fields from the CSV file and move them to the record elements.
		○ I used STRING to swap the order of the Record Key from  MMDDYYYY to YYYYMMDD so that it would sort properly.
		○ Each of the 5 input files populate a portion of the records so…
		○ When processing the first input file (claims by age) I used WRITE to populate the VSAM file because the written records are new
		○ When processing subsequent input files I used REWRITE because by then the records exist and need to be updated... not sure if there is a better/easier way to do this.

Step 2
	• Created simple dataset "UNEMPIN" to contain the user input selections of a Record Key and/or an All Records indicator (in the absence of an online app)
	• Created the main program (UNEMPMN) which reads UNEMPIN and passes these two user input data items to the first subroutine (UNEMPSB) which returns either a table (if All Records indicator is Y), or a single record (if All Records indicator is N and a valid record key is provided).
	• The first subroutine (UNEMPSB) calls one of two other subroutines (RTALLREC, or RTONEREC) depending on the AllRecords indicator.  
	• This is because if AllRecords is selected, I need to access the VSAM file sequentially in order to read through and retrieve all of the records (RTALLREC), but I need to access the VSAM file randomly in order to retrieve a single record based on the record key provided by the user (RTONEREC)… not sure if there is a better/easier way to do this.

Step 3
	• Main program (UNEMPMN) produces the output report in CSV format with headers and it is displayed through PRTLINE as well as printed to a dataset (UNEMPOUT) which can be brought into Excel for better formatting
	• Some basic error handling is included.

