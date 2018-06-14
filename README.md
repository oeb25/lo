# Lez

Lez is a tiny but expressive compiled programming language.

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
        printf('We\'ll call him %s!\n', name);
        name
    };
    fiddo.age = if 1 > 2 {
        4
    } else {
        5
    };

    printf('%s\'s age is %d in human years and %d in dog years!\n', fiddo.name, fiddo.age, factorial(fiddo.age));

    for i in 0..fiddo.age + 1 {
        printf('factorial(%d) = %d\n', i, factorial(i));
    };

    printf('Who knew dog years where calculated using factorial!\n');
    printf('%s is now %d years old!\n', fiddo.name, {
        printf('Happy birthday %s!\n', fiddo.name);
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

## Building Lez

Building Lez requires a Rust compiler running nightly (currently written using `rustc 1.28.0-nightly`). Creating the binary is as simple as running

```bash
$ cargo build --release
```

Now the Lez compiler can be found at `./target/release/lez`, and moved to your path if you so desire.

## Running a program

To run a program written in Lez, one must first run it trough the previously built compiler. The only current backend is C, but more are planed.

```bash
$ ./target/release/lez hello.lez --gcc hello
$ ./hello
```

This first compiles the file using the `gcc` backend, and the runs the newly created program. Multiple files can be specified at once.
