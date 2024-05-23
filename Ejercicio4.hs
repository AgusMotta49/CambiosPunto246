viajesInteresantes = filter esViajeInteresante
nombresYAñosViajesInteresantes :: [Viaje] -> [(String, Int)]
nombresYAñosViajesInteresantes viajes = [(nombreViaje viaje, anioAlQViajan viaje) | viaje <- viajes]