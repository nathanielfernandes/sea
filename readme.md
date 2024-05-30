# sea 🌊

**WIP (🚧)**

A dynamic progamming language that transpiles to C.

currently working on the parser, and IR generation.

```rust
let a = 20;
let a = 30 + a;

let b = {
    let b = {
        a
    };

    b
};
```

ir output:

```rust
fn __main() -> Unit {
  let _0: Any;
  let _1: Any;
  let _2: Any;
  let _3: Any;

  bb0: {
    _0 = const 20;
    _1 = add(const 30, _0) -> [goto bb1];
  }

  bb1: {
    _3 = <upvalue: _1> -> [goto bb2];
  }

  bb2: {
    _2 = _3 -> [goto bb3];
  }

  bb3: {
    [return];
  }
}
```
