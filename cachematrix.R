## Las siguientes funciones almacenan en caché la matríz inverza  
## y evitan recalcularla cuando esta ya ha sido calculada.

## Esta función crea un "objeto" que graba una matriz 
## y puede almacenar en caché su inversa.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Variable para almacenar la inversa en caché
        
        # Función para actualizar la matriz y reiniciar la caché
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Función para obtener la matriz original
        get <- function() x
        
        # Función para almacenar la inversa en caché
        setinverse <- function(inverse) inv <<- inverse
        
        # Función para recuperar la inversa almacenada
        getinverse <- function() inv
        
        # Retorna una lista con todas las funciones anteriores
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Esta función calcula la inversa de la matriz almacenada en el objeto 
## creado por makeCacheMatrix. Si la inversa ya fue calculada 
## previamente y la matriz no ha cambiado, se recupera el valor 
## almacenado en caché para evitar el recálculo.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # Si la inversa ya fue calculada y está en caché, se retorna
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Si no hay inversa en caché, se calcula y almacena
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}