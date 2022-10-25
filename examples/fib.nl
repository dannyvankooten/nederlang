functie fib(n) {
    als n < 2 {
        n
    } anders {
        fib(n - 1) + fib(n - 2)
    }    
}

fib(35)