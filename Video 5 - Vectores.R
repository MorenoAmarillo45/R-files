####################
#                  #
# Copiar todo esto #
#                  #
####################
# Hecho con gusto por Rafa @GonzalezGouveia

# video 05 - vectores

# Objetivo: estudiar qu� es una vector en R.
# --------------------------------------------
# En este ejercicio vamos a:
# 1. Crear vectores en un script
# 2. Realizar opraciones aritm�ticas con vectores
# 3. Seleccionar elementos en un vector


#####################################
# pr�ctica 1: creando vectores en R #
#####################################

# crear vector car�cter con nombre de las pel�culas
nombre <- c("Shrek", "Shrek 2", "Shrek Tercero", "Shrek: Felices por siempre")

# crear vector num�rico con puntuaci�n de las pel�culas
puntuaci�n <-c(7.9, 7.2, 6.1, 6.3)

# crear vector l�gico sobre si la pel�cula es posterior a 2015
posterior_2005 <-c(FALSE, FALSE, TRUE, TRUE)

####################################################
# pr�ctica 2: operaciones aritm�ticas con vectores #
####################################################

# sumar 2 a la puntuaci�n
puntuaci�n+2

# dividir la puntuaci�n entre 2
puntuaci�n/2

# crea la puntuaci�n de rafa
puntuaci�n_de_Perla <-c(10, 10, 6.1, 5)

# calcular diferencia entre puntuaciones
puntuaci�n - puntuaci�n_de_Perla
puntuaci�n_de_Perla - puntuaci�n
# calcular la longitud del vector
length(puntuaci�n)
length(posterior_2005)
length(nombre)
# calcular el promedio del vector puntuacion
mean(puntuaci�n_de_Perla)
mean(puntuaci�n)
###################################################
# pr�ctica 3: selecci�n de elementos de un vector #
###################################################

## selecci�n basada en posici�n
# seleccionar la tercera pel�cula
nombre[3]

# seleccionar la primera y la �ltima pel�cula
nombre[c(1,4)]

## selecci�n basada en condici�n l�gica
# crear condici�n l�gica
puntuaci�n_baja <- puntuaci�n <7
print(puntuaci�n_baja)
# mostrar condici�n para ver TRUE/FALSE
puntuaci�n_baja

# mostrar puntuaciones bajas
puntuaci�n[puntuaci�n_baja]

# mostrar nombres de pel�culas con puntuaciones bajas
nombre[puntuaci�n_baja]

# Hecho con gusto por Rafa @GonzalezGouveia
# Suscribete para m�s c�digo en R https://bit.ly/2WNDhNR

## Secci�n para completar dudas

##mostrar si las puntuaciones bajas son posteriores al 2005
posterior_2005[puntuaci�n_baja]
