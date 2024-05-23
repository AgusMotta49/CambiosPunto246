listaRecuerdosViajeroTransformado :: Viajero -> [Recuerdos]
listaRecuerdosViajeroTransformado viajero = (obtenerRecuerdosViajero.viajeroTransformado) viajero

{------------|Sub funciones|------------}
viajeroTransformado :: Viajero -> Viajero
viajeroTransformado viajero = aplicarTransformaciones (listaTransformaciones viajero) viajero

obtenerRecuerdosViajero :: Viajero -> [Recuerdos]

obtenerRecuerdosViajero (Viajero _ _ recuerdos _) = recuerdos

aplicarTransformaciones :: [String] -> Viajero -> Viajero
aplicarTransformaciones [] viajero = viajero
aplicarTransformaciones _ (Viajero _ _ [] _) = error []
aplicarTransformaciones (x:xs) viajero@(Viajero nombre edad recuerdos viajes)
    | x == "lejano oeste" = Viajero nombre edad (filtrarRecuerdosVocal recuerdos) viajes
    | x == "futuro" = Viajero nombre (edad + 10) recuerdos viajes
    | otherwise = aplicarTransformaciones xs viajero


listaTransformaciones :: Viajero -> [String]
listaTransformaciones viajero = obtenerTransformaciones (viajero)

filtrarRecuerdosVocal :: [Recuerdos] -> [Recuerdos]
filtrarRecuerdosVocal recuerdos = filter (\(Recuerdos nombre _) -> not(comienzaVocal nombre)) recuerdos

comienzaVocal :: String -> Bool
comienzaVocal (x:_) = x `elem` "aeiouAEIOU"


obtenerRecuerdosDeViajes :: Viajero -> [Recuerdos]
obtenerRecuerdosDeViajes (Viajero _ _ recuerdos viajes) = foldr (\viaje recuerdos -> recuerdo viaje ++ recuerdos) [] viajes -- deberia devolver una lista de listas con {nombreDelRecuerdo, lugarOrigen}
-- es como si le pasara x e y, pero y es una lista y voy concatenandole recuerdos 

obtenerTransformaciones :: Viajero -> [String]
obtenerTransformaciones (Viajero _ _ _ viajes) = foldr (\viaje acumulador -> transformaciones viaje ++ acumulador) [] viajes
{-------------------------------------}
