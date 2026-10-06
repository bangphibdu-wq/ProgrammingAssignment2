## Hàm tạo một đối tượng "ma trận" đặc biệt có khả năng lưu bộ nhớ đệm (cache) giá trị nghịch đảo của chính nó
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Hàm tính toán ma trận nghịch đảo. Nếu đã tính trước đó, hàm sẽ lấy kết quả từ bộ nhớ đệm ra thay vì tính lại
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
