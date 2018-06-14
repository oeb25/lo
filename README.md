# lo

lo is a tiny but expressive compiled programming language.

```d
struct Dog {
    name: str,
    age: int,
}

fun factorial(x: int): int {
    if x > 1 {
        x * factorial(x - 1)
    } else {
        x
    }
}

fun main(): int {
    let fiddo: Dog;
    fiddo.name = {
        let name = 'Fiddo';
        print("We'll call him %!\n", name);
        name
    };
    fiddo.age = 5;

    print("%'s age is % in human years and % in dog years!\n", fiddo.name, fiddo.age, factorial(fiddo.age));

    for i in 0..fiddo.age + 1 {
        print('factorial(%) = %\n', i, factorial(i));
    };

    print('Who knew dog years where calculated using factorial!\n');
    print('% is now % years old!\n', fiddo.name, {
        print('Happy birthday %!\n', fiddo.name);
        fiddo.age += 1;
        fiddo.age
    });
    0
}

/*
	We'll call him Fiddo!
	Fiddo's age is 5 in human years and 120 in dog years!
	factorial(0) = 0
	factorial(1) = 1
	factorial(2) = 2
	factorial(3) = 6
	factorial(4) = 24
	factorial(5) = 120
	Who knew dog years where calculated using factorial!
	Happy birthday Fiddo!
	Fiddo is now 6 years old!
*/
```

## Building lo

Building lo requires a Rust compiler running nightly (currently written using `rustc 1.28.0-nightly`). Creating the binary is as simple as running

```bash
$ cargo build --release
```

Now the lo compiler can be found at `./target/release/lo`, and moved to your path if you so desire.

## Running a program

To run a program written in lo, one must first run it trough the previously built compiler. The only current backend is C, but more are planed.

```bash
$ ./target/release/lo hello.lo --gcc hello
$ ./hello
```

This first compiles the file using the `gcc` backend, and the runs the newly created program. Multiple files can be specified at once.
