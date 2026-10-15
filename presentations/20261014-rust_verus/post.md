# An Introduction to OS-Level Verification with Asterinas and Verus

I gave a short talk at *Rust、何もわからない… #15* about formal verification at the operating-system level. The slides are below.

[Asterinas](https://github.com/asterinas/asterinas) is a Linux-compatible kernel written in Rust. Most of its code uses safe Rust, while low-level operations that require `unsafe` are isolated in OSTD. [VOSTD](https://github.com/asterinas/vostd) uses [Verus](https://github.com/verus-lang/verus) to verify this low-level layer.

For the demo, I deliberately introduced a bug into `align_down`. The function should return the greatest multiple of a power-of-two alignment that is no greater than the input. Changing

```rust
self & !(align - 1)
```

to

```rust
self & (align - 1)
```

breaks that guarantee. Verus caught the bug because it could no longer prove the postcondition `ret % align == 0`. Unlike a test, this check is about satisfying the specification, not just passing selected inputs.

Writing the proof for the correct implementation is much harder than spotting this bug. The proof needs lemmas connecting bitwise operations, masks, and alignment. [KVerus](https://github.com/asterinas/KVerus) explores using LLMs to generate and repair Verus proofs. Perhaps one day, developers will only need to write preconditions and postconditions, while an LLM drafts the proof code and Verus checks it. That is a direction to explore, not yet a reason to treat proof generation as a solved problem.

## Resources

- [Asterinas](https://github.com/asterinas/asterinas)
- [VOSTD](https://github.com/asterinas/vostd)
- [Verus](https://github.com/verus-lang/verus)
- [KVerus](https://github.com/asterinas/KVerus)
