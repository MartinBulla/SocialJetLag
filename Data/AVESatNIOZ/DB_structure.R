ADD INDEXES - ID, home_av, mass, feet

### START BELOW					
			DROP TABLE IF EXISTS AUTHORS; 
			CREATE TABLE AUTHORS ( 
						initials 	VARCHAR( 3 ) NULL DEFAULT NULL,
						name       	VARCHAR( 30 )   NULL DEFAULT NULL,
						surname    	VARCHAR( 30 )   NULL DEFAULT NULL,
						start_     	DATE        NULL DEFAULT NULL,
						end_       	DATE        NULL DEFAULT NULL,
						alias		ARCHAR( 100 )  NULL DEFAULT NULL,
						project    	VARCHAR( 100 )  NULL DEFAULT NULL,
						contact		VARCHAR( 100 )  NULL DEFAULT NULL,
						remarks    	VARCHAR( 100 )  NULL DEFAULT NULL,
						a_pk	INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);
					
			DROP TABLE IF EXISTS BIRDS; 
			CREATE TABLE BIRDS ( 
						year_      		INTEGER( 4 )    NULL DEFAULT NULL,  
						species			VARCHAR( 30 )   NULL DEFAULT NULL,
						subspecies		VARCHAR( 30 )   NULL DEFAULT NULL,
						bird_ID			VARCHAR( 30 )   NULL DEFAULT NULL,
						crc			VARCHAR( 10 )   NULL DEFAULT NULL,
						crc_now		VARCHAR( 10 )   NULL DEFAULT NULL,
						home_av			VARCHAR( 3 ) NULL DEFAULT NULL,
						current_av		VARCHAR( 3 ) NULL DEFAULT NULL,
						age				VARCHAR( 3 ) NULL DEFAULT NULL,
						sex       		VARCHAR( 1 )   NULL DEFAULT NULL,
						start_			DATETIME        NULL DEFAULT NULL,
						end_			DATETIME        NULL DEFAULT NULL,
						end_type		VARCHAR( 10 )   NULL DEFAULT NULL, 
						caught			DATETIME        NULL DEFAULT NULL,
						lat_c			REAL   NULL DEFAULT NULL,
						lon_c			REAL   NULL DEFAULT NULL,
						site_c			VARCHAR( 10 )   NULL DEFAULT NULL,
						lat_r 			REAL   NULL DEFAULT NULL,
						lon_r			REAL   NULL DEFAULT NULL,
						site_r			VARCHAR( 10 )   NULL DEFAULT NULL,
						muscle 			REAL    NULL DEFAULT NULL,
						height_1		REAL    NULL DEFAULT NULL,
						width_1			REAL    NULL DEFAULT NULL,
						height_2		REAL    NULL DEFAULT NULL,
						width_2			REAL    NULL DEFAULT NULL,
						mass_f			REAL    NULL DEFAULT NULL,
						mass_c			REAL    NULL DEFAULT NULL,
						wing			REAL    NULL DEFAULT NULL,
						bill			REAL    NULL DEFAULT NULL,
						totalhead		REAL    NULL DEFAULT NULL,
						tarsus			REAL    NULL DEFAULT NULL,
						tartoe			REAL    NULL DEFAULT NULL,
						blood			VARCHAR( 1 )   NULL DEFAULT NULL,
						sex_method		VARCHAR( 10 )   NULL DEFAULT NULL,
						bio_datetime	DATETIME        NULL DEFAULT NULL,
						bio_author 		VARCHAR( 2 )   NULL DEFAULT NULL,
						ful_datetime	DATETIME        NULL DEFAULT NULL,
						ful_author 		VARCHAR( 2 )   NULL DEFAULT NULL,
						project			VARCHAR( 100 )  NULL DEFAULT NULL,
						remarks    		VARCHAR( 500 )  NULL DEFAULT NULL,
						b_pk		INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
					);
					CREATE UNIQUE INDEX bird_ID ON BIRDS(bird_ID);
					
			DROP TABLE IF EXISTS CAPTURES; 
			CREATE TABLE CAPTURES ( 
						year_			INTEGER (4),
						capture     	DATETIME        NULL DEFAULT NULL,
						at			 	VARCHAR( 3 ) 	NULL DEFAULT NULL,
						[release]			DATETIME    NULL DEFAULT NULL,
						[where]			VARCHAR( 3 ) 	NULL DEFAULT NULL,
						bird_ID    		VARCHAR( 10 )   NULL DEFAULT NULL,
						what			VARCHAR( 30 )   NULL DEFAULT NULL,
						what_ID			VARCHAR( 10 )   NULL DEFAULT NULL,
						health			VARCHAR ( 30)	NULL DEFAULT NULL,
						feet			INTEGER (1)     NULL DEFAULT NULL,
						mass     		REAL        	NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )  NULL DEFAULT NULL,
						author       	VARCHAR( 2 )    NULL DEFAULT NULL,
						plum     	INTEGER (1)     NULL DEFAULT NULL,
						molt     		INTEGER (1)     NULL DEFAULT NULL,
						molt_col		VARCHAR( 1 )    NULL DEFAULT NULL,
						L01				INTEGER       	NULL DEFAULT NULL,
						L02				INTEGER       	NULL DEFAULT NULL,
						L03				INTEGER       	NULL DEFAULT NULL,	
						L04				INTEGER       	NULL DEFAULT NULL,
						L05				INTEGER       	NULL DEFAULT NULL,
						L06				INTEGER       	NULL DEFAULT NULL,
						L07				INTEGER       	NULL DEFAULT NULL,
						L08				INTEGER       	NULL DEFAULT NULL,
						L09				INTEGER       	NULL DEFAULT NULL,
						L10				INTEGER       	NULL DEFAULT NULL,
						R01				INTEGER       	NULL DEFAULT NULL,
						R02				INTEGER       	NULL DEFAULT NULL,	
						R03				INTEGER       	NULL DEFAULT NULL,	
						R04				INTEGER       	NULL DEFAULT NULL,	
						R05				INTEGER       	NULL DEFAULT NULL,	
						R06				INTEGER       	NULL DEFAULT NULL,	
						R07				INTEGER       	NULL DEFAULT NULL,	
						R08				INTEGER       	NULL DEFAULT NULL,	
						R09				INTEGER       	NULL DEFAULT NULL,	
						R10				INTEGER       	NULL DEFAULT NULL,	
						crc_now			VARCHAR( 10 )   NULL DEFAULT NULL, -- THIS ONE IS LIKELY NOT NECESSARY AS WE WILL WORK WITH BIRD ID INSTEAD OF CRC AND CRC AND CRC_NOW WILL BE IN THE BIRD METADATA FILE
						pic				VARCHAR(250) NULL DEFAULT NULL,
						with			REAL NULL DEFAULT NULL,
						c_pk		INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);	
						CREATE INDEX ID ON CAPTURES(bird_ID);
						CREATE INDEX aviary ON CAPTURES([where]);

			DROP TABLE IF EXISTS VISITS; 
			CREATE TABLE VISITS ( 
						who				VARCHAR( 10 )   NULL DEFAULT NULL,
						[where] 		VARCHAR( 3 )   	NULL DEFAULT NULL,
						start_			DATETIME        NULL DEFAULT NULL,
						what 			VARCHAR( 10 )   NULL DEFAULT NULL,
						end_			DATETIME        NULL DEFAULT NULL,
						general_check	VARCHAR( 1 )   NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						v_pk			INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL --INTEGER         PRIMARY KEY 
						);				
						
			DROP TABLE IF EXISTS SAMPLES; 
			CREATE TABLE SAMPLES ( 
						datetime_  		DATETIME        NULL DEFAULT NULL,
						type 		VARCHAR( 3 )   NULL DEFAULT NULL,
						ID 		VARCHAR( 10 )   NULL DEFAULT NULL,
						[where] 		VARCHAR( 10 )   NULL DEFAULT NULL,
						remarks		VARCHAR( 100 )   NULL DEFAULT NULL,
						sample_pk		INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						); 
						
			DROP TABLE IF EXISTS DEVICES; 
			CREATE TABLE DEVICES ( 
						datetime_  		DATETIME        NULL DEFAULT NULL,
						device 			VARCHAR( 3 )   NULL DEFAULT NULL,
						ID 				VARCHAR( 10 )   NULL DEFAULT NULL,
						what 			VARCHAR( 10 )   NULL DEFAULT NULL,
						batt			REAL    NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						devices_pk		INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL --INTEGER         PRIMARY KEY 
						);	
			
			DROP TABLE IF EXISTS ULTRASOUND; 
			CREATE TABLE ULTRASOUND ( 
						author 			VARCHAR( 2 )   NULL DEFAULT NULL,
						capture			DATETIME        NULL DEFAULT NULL,
						bird_ID    		VARCHAR( 10 )   NULL DEFAULT NULL,
						muscle 			REAL    NULL DEFAULT NULL,
						height_1		REAL    NULL DEFAULT NULL,
						width_1			REAL    NULL DEFAULT NULL,
						height_2		REAL    NULL DEFAULT NULL,
						width_2			REAL    NULL DEFAULT NULL,	
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						ultra_pk		INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);
			
			DROP TABLE IF EXISTS DBLOG; 
			CREATE TABLE DBLOG  ( 
							`pk` INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
							`db` VARCHAR(50) NULL DEFAULT NULL,
							`table` VARCHAR(50) NULL DEFAULT NULL,
							`datetime_` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP NOT NULL,
							`author` VARCHAR(50) NULL DEFAULT NULL,
							`type` VARCHAR(100000) NULL DEFAULT NULL, --weekly, major, minor
							`script` LONGTEXT NULL,
							`remarks` LONGTEXT NULL
							);
			
			DROP TABLE IF EXISTS TO_DO; 				
			CREATE TABLE TO_DO ( 
						capture			DATETIME        NULL DEFAULT NULL,
						bird_ID    		VARCHAR( 10 )   NULL DEFAULT NULL,
						what 			VARCHAR( 10 )   NULL DEFAULT NULL,
						datetime_solved	DATETIME        NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						todo_pk		    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);
			
			DROP TABLE IF EXISTS CONT_OBS; 			
			CREATE TABLE CONT_OBS ( 
						author 			VARCHAR( 2 )   NULL DEFAULT NULL,
						aviary    		VARCHAR( 2 )   NULL DEFAULT NULL,
						session    		VARCHAR   		NULL DEFAULT NULL,
						bird_ID    		VARCHAR( 10 )   NULL DEFAULT NULL,
						datetime_		DATETIME        NULL DEFAULT NULL,
						beh 			VARCHAR( 10 )   NULL DEFAULT NULL,
						sure 			VARCHAR( 1 )   NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						cont_pk		    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);			
				
			DROP TABLE IF EXISTS AVIARIES; 			
			CREATE TABLE AVIARIES ( 
						author 			VARCHAR( 2 )   NULL DEFAULT NULL,
						datetime_		DATETIME        NULL DEFAULT NULL,
						aviary    		VARCHAR( 2 )   NULL DEFAULT NULL,
						light_cycle 	VARCHAR( 10 )   NULL DEFAULT NULL,
						maxL			REAL   		NULL DEFAULT NULL,
						minL			REAL   		NULL DEFAULT NULL,
						T_cycle    		VARCHAR( 10 )   NULL DEFAULT NULL,
						Troom   		REAL   		NULL DEFAULT NULL,
						Tr_change   	REAL   		NULL DEFAULT NULL,
						Twater   		REAL   		NULL DEFAULT NULL,
						Tw_change   	REAL   		NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						av_pk		    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);			
				
			DROP TABLE IF EXISTS TAGS; 			
			CREATE TABLE TAGS ( 
						tag 			VARCHAR( 2 )   NULL DEFAULT NULL,
						type    		VARCHAR( 10 )   NULL DEFAULT NULL,
						coating			VARCHAR( 10 )   NULL DEFAULT NULL,
						memmory    		VARCHAR( 10 )   NULL DEFAULT NULL,
						battery    		VARCHAR( 10 )   NULL DEFAULT NULL,
						mass			REAL   		NULL DEFAULT NULL,
						mass_ready		REAL   		NULL DEFAULT NULL,
						[where]    		VARCHAR( 10 )   NULL DEFAULT NULL,
						remarks			VARCHAR( 100 )   NULL DEFAULT NULL,
						tag_pk		    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);			
			DROP TABLE IF EXISTS HARN; 			
			CREATE TABLE HARN ( 
						capture		DATETIME        NULL DEFAULT NULL,
						bird_ID 	VARCHAR( 30 )   NULL DEFAULT NULL,
						what 		VARCHAR( 30 )   NULL DEFAULT NULL,
						what_ID    	VARCHAR( 10 )   NULL DEFAULT NULL,
						
						tilt			REAL   		NULL DEFAULT NULL,
						neck			REAL   		NULL DEFAULT NULL,
						armpit    		REAL   		NULL DEFAULT NULL,
						back    		REAL   		NULL DEFAULT NULL,
						size    		REAL   		NULL DEFAULT NULL,
						harn_pk		    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
						);	
		
		DROP TABLE IF EXISTS BIO_TRAIN; 
		CREATE TABLE BIO_TRAIN ( 
						year_			INTEGER (4),
						author       	VARCHAR( 2 )    NULL DEFAULT NULL,
						datetime_     	DATETIME        NULL DEFAULT NULL,
						bird_ID    		VARCHAR( 10 )   NULL DEFAULT NULL,
						wing			REAL    NULL DEFAULT NULL,
						bill			REAL    NULL DEFAULT NULL,
						totalhead		REAL    NULL DEFAULT NULL,
						tarsus			REAL    NULL DEFAULT NULL,
						tartoe			REAL    NULL DEFAULT NULL,
						muscle 			REAL    NULL DEFAULT NULL,
						height_1		REAL    NULL DEFAULT NULL,
						width_1			REAL    NULL DEFAULT NULL,
						height_2		REAL    NULL DEFAULT NULL,
						width_2			REAL    NULL DEFAULT NULL,
						remarks    		VARCHAR( 500 )  NULL DEFAULT NULL,
						bio_pk			INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
					);
			
						
					
	vacuum

### END ABOVE							
			CREATE TABLE BMR ( 
						BMR_pk		INTEGER         PRIMARY KEY 
						);							
			
			
			}	
			