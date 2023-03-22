#' Distance between two coordinates
#' 
#' Put in point A and point B, get back miles or km.
#' Further info: http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates
#' 
#' @name arcdist
#' @keywords latitude longitude latlon distance geocode
#' @export arcdist
#' 
#' @param lat1 Latitude of first coordinate
#' @param lon1 Longitude of first coordinate
#' @param lat2 Latitude of second coordinate
#' @param lon2 Longitude of second coordinate
#' @param R The radius of the Earth. Default is in miles, 3959. Change it to 6371 if you want the result in km.

arcdist <- function(lat1, lon1, lat2, lon2, R=3959) {
	## convert to radians (now vectorized for great justice!)
	lat1*pi/180->lat1
	lon1*pi/180->lon1
	lat2*pi/180->lat2
	lon2*pi/180->lon2
	
	## calculate distance to return:
	return(acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon1-lon2))*R)
}