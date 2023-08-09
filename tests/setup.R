# federal programs
# poverty oversample
# monthly dynamics
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

sipp_main_dt <- fread( main_csv , sep = "|" )

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

sipp_df[ , 'one' ] <- 1
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
	
sipp_design <- 
	
	update( 
		
		sipp_design , 
		
		one = 1
			
	)
sum( weights( sipp_design , "sampling" ) != 0 )

svyby( ~ one , ~ state , sipp_design , unwtd.count )
svytotal( ~ one , sipp_design )

svyby( ~ one , ~ state , sipp_design , svytotal )
svymean( ~ first_fed_formula , sipp_design , na.rm = TRUE )

svyby( ~ first_fed_formula , ~ state , sipp_design , svymean , na.rm = TRUE )
svymean( ~ sex , sipp_design , na.rm = TRUE )

svyby( ~ sex , ~ state , sipp_design , svymean , na.rm = TRUE )
svytotal( ~ first_fed_formula , sipp_design , na.rm = TRUE )

svyby( ~ first_fed_formula , ~ state , sipp_design , svytotal , na.rm = TRUE )
svytotal( ~ sex , sipp_design , na.rm = TRUE )

svyby( ~ sex , ~ state , sipp_design , svytotal , na.rm = TRUE )
svyquantile( ~ first_fed_formula , sipp_design , 0.5 , na.rm = TRUE )

svyby( 
	~ first_fed_formula , 
	~ state , 
	sipp_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ bf_exclr06 , 
	denominator = ~ bf_endr06 , 
	sipp_design ,
	na.rm = TRUE
)
sub_sipp_design <- subset( sipp_design , p_utdpol == 1 )
svymean( ~ first_fed_formula , sub_sipp_design , na.rm = TRUE )
this_result <- svymean( ~ first_fed_formula , sipp_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ first_fed_formula , 
		~ state , 
		sipp_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( sipp_design )
svyvar( ~ first_fed_formula , sipp_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ first_fed_formula , sipp_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ first_fed_formula , sipp_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ dtap_3p , sipp_design ,
	method = "likelihood" )
svyttest( first_fed_formula ~ dtap_3p , sipp_design )
svychisq( 
	~ dtap_3p + sex , 
	sipp_design 
)
glm_result <- 
	svyglm( 
		first_fed_formula ~ dtap_3p + sex , 
		sipp_design 
	)

summary( glm_result )
# table 4 and table 4a
svymean( ~ factor( findInterval( thnetworth , c( 1 , 5000 , 10000 , 25000 , 50000 , 100000 , 250000 , 500000 ) ) ) , subset( sipp_design , erelrpe %in% 1:2 & tlivqtr %in% 1:2 ))

library(convey)
sipp_design <- convey_prep( sipp_design )

svygini( ~ vd4020n , sipp_design , na.rm = TRUE )
library(srvyr)
sipp_srvyr_design <- as_survey( sipp_design )
sipp_srvyr_design %>%
	summarize( mean = survey_mean( first_fed_formula , na.rm = TRUE ) )

sipp_srvyr_design %>%
	group_by( state ) %>%
	summarize( mean = survey_mean( first_fed_formula , na.rm = TRUE ) )
