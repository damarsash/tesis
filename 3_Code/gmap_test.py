import googlemaps

gmaps = googlemaps.Client(key="AIzaSyAdNOaRAzzRFTl_spTUbvm9O_yz8qBByew")

result = gmaps.distance_matrix(
    origins=[(-6.535158, 106.799133)],
    destinations=[(-6.6, 106.8)],
    mode="driving"
)

print(result)