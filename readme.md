# sea 🌊

**WIP (🚧)**

A dynamic progamming language that transpiles to C.

```rust
fn fib(n) {
    if n < 2 {
        return n;
    }

    return fib(n - 1) + fib(n - 2);
}

println(fib(40));
```

```
-------------- Running --------------
102334155

-------------------------------------
Time: 1.594973513s
```
