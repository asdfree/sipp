# federal programs
# poverty oversample
# monthly dynamics
variables_to_keep <-
	c( 'ssuid' , 'pnum' , 'monthcode' , 'spanel' , 'swave' , 'erelrpe' , 'tlivqtr' , 'wpfinwgt' , 
	'rmesr' , 'thcyincpov' , 'tfcyincpov' , 'tehc_st' , 'rhicovann' , 'rfpov' , 'thnetworth' )
library(httr)
library(data.table)

main_tf <- tempfile()

main_url <-
	paste0(
		"https://www2.census.gov/programs-surveys/sipp/" ,
		"data/datasets/2022/pu2022_csv.zip"
	)

GET( main_url , write_disk( main_tf ) , progress() )

main_csv <- unzip( main_tf , exdir = tempdir() )

sipp_main_dt <- fread( main_csv , sep = "|" , select = toupper( variables_to_keep ) )

sipp_main_df <- data.frame( sipp_main_dt )

names( sipp_main_df ) <- tolower( names( sipp_main_df ) )

rw_tf <- tempfile()

rw_url <-
	paste0(
		"https://www2.census.gov/programs-surveys/sipp/" ,
		"data/datasets/2022/rw2022_csv.zip"
	)

GET( rw_url , write_disk( rw_tf ) , progress() )

rw_csv <- unzip( rw_tf , exdir = tempdir() )

sipp_rw_dt <- fread( rw_csv , sep = "|" )

sipp_rw_df <- data.frame( sipp_rw_dt )

names( sipp_rw_df ) <- tolower( names( sipp_rw_df ) )

sipp_df <-
	merge(
		sipp_main_df[ sipp_main_df[ , 'monthcode' ] %in% 12 , ] ,
		sipp_rw_df[ sipp_rw_df[ , 'monthcode' ] %in% 12 , ] ,
		by = c( 'ssuid' , 'pnum' , 'monthcode' , 'spanel' , 'swave' )
	)
	
stopifnot( nrow( sipp_df ) == sum( sipp_rw_df[ , 'monthcode' ] %in% 12 ) )
# sipp_fn <- file.path( path.expand( "~" ) , "SIPP" , "this_file.rds" )
# saveRDS( sipp_df , file = sipp_fn , compress = FALSE )
# sipp_df <- readRDS( sipp_fn )
library(survey)

sipp_design <- 
	svrepdesign(
			data = sipp_df ,
			weights = ~ wpfinwgt ,
			repweights = "repwgt([1-9]+)" ,
			type = "Fay" ,
			rho = 0.5
		)
	
rmesr_values <-
	c( 
		"With a job entire month, worked all weeks",
		"With a job all month, absent from work without pay 1+ weeks, absence not due to layoff",
		"With a job all month, absent from work without pay 1+ weeks, absence due to layoff",
		"With a job at least 1 but not all weeks, no time on layoff and no time looking for work",
		"With a job at least 1 but not all weeks, some weeks on layoff or looking for work",
		"No job all month, on layoff or looking for work all weeks",
		"No job all month, at least one but not all weeks on layoff or looking for work",
		"No job all month, no time on layoff and no time looking for work"
	)

sipp_design <- 
	
	update( 
		
		sipp_design , 
		
		one = 1 ,
		
		employment_status = factor( rmesr , levels = 1:8 , labels = rmesr_values ) ,
			
		household_below_poverty = as.numeric( thcyincpov < 1 ) ,
		
		family_below_poverty = as.numeric( tfcyincpov < 1 ) ,
		
		state_name =
			
			factor(
				
				as.numeric( tehc_st ) ,
				
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L, 60L, 61L) ,
		
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico",
					"Foreign Country")
			)
			
	)
sum( weights( sipp_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , sipp_design , unwtd.count )
svytotal( ~ one , sipp_design )

svyby( ~ one , ~ state_name , sipp_design , svytotal )
svymean( ~ tftotinc , sipp_design , na.rm = TRUE )

svyby( ~ tftotinc , ~ state_name , sipp_design , svymean , na.rm = TRUE )
svymean( ~ employment_status , sipp_design , na.rm = TRUE )

svyby( ~ employment_status , ~ state_name , sipp_design , svymean , na.rm = TRUE )
svytotal( ~ tftotinc , sipp_design , na.rm = TRUE )

svyby( ~ tftotinc , ~ state_name , sipp_design , svytotal , na.rm = TRUE )
svytotal( ~ employment_status , sipp_design , na.rm = TRUE )

svyby( ~ employment_status , ~ state_name , sipp_design , svytotal , na.rm = TRUE )
svyquantile( ~ tftotinc , sipp_design , 0.5 , na.rm = TRUE )

svyby( 
	~ tftotinc , 
	~ state_name , 
	sipp_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ tftotinc , 
	denominator = ~ rfpov , 
	sipp_design ,
	na.rm = TRUE
)
sub_sipp_design <- subset( sipp_design , rhicovann == 1 )
svymean( ~ tftotinc , sub_sipp_design , na.rm = TRUE )
this_result <- svymean( ~ tftotinc , sipp_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ tftotinc , 
		~ state_name , 
		sipp_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( sipp_design )
svyvar( ~ tftotinc , sipp_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ tftotinc , sipp_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ tftotinc , sipp_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ family_below_poverty , sipp_design ,
	method = "likelihood" )
svyttest( tftotinc ~ family_below_poverty , sipp_design )
svychisq( 
	~ family_below_poverty + employment_status , 
	sipp_design 
)
glm_result <- 
	svyglm( 
		tftotinc ~ family_below_poverty + employment_status , 
		sipp_design 
	)

summary( glm_result )
sipp_household_design <- subset( sipp_design , erelrpe %in% 1:2 & tlivqtr %in% 1:2 )

stopifnot( round( coef( svytotal( ~ one , sipp_household_design ) ) / 1000 , -2 ) == 132700 )
sipp_household_design <-
	update(
		sipp_household_design ,
		thnetworth_category =
			factor(
				findInterval( 
					thnetworth , 
					c( 1 , 5000 , 10000 , 25000 , 50000 , 100000 , 250000 , 500000 ) 
				) ,
				levels = 0:8 ,
				labels = c( "Zero or Negative" , "$1 to $4,999" , "$5,000 to $9,999" , 
				"$10,000 to $24,999" , "$25,000 to $49,999" , "$50,000 to $99,999" , 
				"$100,000 to $249,999" , "$250,000 to $499,999" , "$500,000 or over" )
			)
	)

results <- svymean( ~ thnetworth_category , sipp_household_design )

stopifnot( 
	all.equal( as.numeric( round( coef( results ) * 100 , 1 ) ) , 
	c( 11.3 , 6.9 , 3.5 , 5.9 , 6.4 , 8.2 , 15.3 , 13.9 , 28.7 ) ) 
)

stopifnot(
	all.equal( as.numeric( round( SE( results ) * 100 , 1 ) ) ,
	c( 0.3 , 0.2 , 0.2 , 0.2 , 0.2 , 0.2 , 0.3 , 0.3 , 0.3 ) )
)

library(convey)
sipp_design <- convey_prep( sipp_design )

svygini( ~ tftotinc , sipp_design , na.rm = TRUE )
library(srvyr)
sipp_srvyr_design <- as_survey( sipp_design )
sipp_srvyr_design %>%
	summarize( mean = survey_mean( tftotinc , na.rm = TRUE ) )

sipp_srvyr_design %>%
	group_by( state_name ) %>%
	summarize( mean = survey_mean( tftotinc , na.rm = TRUE ) )
