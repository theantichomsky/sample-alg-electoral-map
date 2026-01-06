--Run--
Register Census key: census_api_key("YOUR_KEY", install=TRUE). Execute script. Wait for.png output. 
Target: 269 districts. Ideal size: total_pop / 269. SF & Sacramento Cities: Fixed at 8. Others: round(pop / ideal). Start spatial merging.

--Dependencies-- 
R with packages: --sf, ggplot2, tigris (as a note, this will run pretty slow because of how large the map files are), tidycensus, dplyr, cowplot, classInt-- Also Census API key from https://api.census.gov/data/key_signup.html. You set the key at tidycensus::census_api_key("YOUR_KEY", install = TRUE).
Before you run the algorithm it'll check that you have all the dependecies installed. If you're missing any, it'll install them automatically. Note that running this involves downloading large shapefiles and performing intensive spatial calculations, so it goes without saying that you should have a stable internet connection and sufficient RAM.

--Personal Note-- 
This is a repository for a script that runs a sample algorithm that districts for a nondescript 269-seat parliament in a fictional country that is roughly the shape of the general Northern California area.

Let me say: I am by no means a seasoned developer. This is not very useful, nor did I make it with much reservations. It is as long as it is because I kept adding more and more stupid things afterwards and only went back to debug when the script literally wouldn't run. I really also should make it clear that I'm not trying to reflect any genuine political convictions with this project. I find the electoral process to be one of the most magnificent, radical, terrifying, truely-byzantine political formula for a mode of government. Having made physical maps myself for a long time, I started to play around with some packages while I was going over the most recent census data for my home state of California. I encountered a lot of problems and learned a lot from debugging, and while this is the less-than-satisfying product of my little investigation, I am still quite pleased with the overall experience.

Anyways, some electoral information: The number of seats are decided based on a fixed-variable schema. The Constitution provides that every national territory must receive at least one seat. This de facto would have fixed the number of seats at 93 given a single-member system, although it gave no de jure precedence for the exact number of national territories that could be created. The Organic Law on the General Electoral System passed in conjunction provided a statutory fix on the number of seats at 300 and used an Adam’s divisor to apportion two to six member seats amongst the 93 national territories. All 300 members would be elected through a single non-transferable vote.

The 1998 electoral reforms raised the number of seats to 450. The 1999 electoral and redistricting reforms aimed to resolve hyperpersonalism by increasing proportional representation. The acting Prime Minister at the time proposed separating the seats evenly between 225 single-member national territories and 225 multi-member functional territories; the majority party argued that it would have a negligible impact on the political contests and threatened to petition for a motion of no confidence. To resolve the impasse, the opposition party agreed to approving 300 single-member national territories and 150 proportional representational functional territories. In 2004, the urban-rural divide was rapidly growing and the growing population of urban citizens became dramatically underrepresented, and the decision was made to begin cutting some of the seats.

As of 2030, there are 406 members of the Legislative Court elected to serve four-year terms:

269 national territorial seats elected by first-past-the-post voting
137 functional territorial seats elected by party-list proportional representation voting with seats apportioned using the greatest divisor method. 

This script only accounts for the 269 national territorial seats, but future implementation and map interaction is considered.

Email me: ctrieu0408@gmail.com
