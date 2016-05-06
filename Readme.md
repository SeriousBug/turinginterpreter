# turinginterpreter

turinginterpreter is a
[Turing Machine](https://en.wikipedia.org/wiki/Turing_machine)
simulator, and at the same time an esolang.

# Example Turing Machine

Let's examine a sample machine as accepted by this simulator. This
machine reads 1's from the tape, then copies them over to the right,
leaving one space between the original and the copy.

```
# If there is a 1 at the start, mark it with an X, otherwise halt.
s1 {
  1 -> X, L, s2
  _ -> _, L, HALT
}
```

The first line is a comment, any lines starting with a `#` are
skipped. Then, we define a state called `s1`. This state has two
transitions. If the machine reads `1` from the tape, it will replace
that with an `X`, move the tape to the left, and switch to the `s2`
state. If the tape under the head is empty (empty spaces are denoted
with `_`), then it will leave it empty, move the tape to the left, and
halt the machine.

The first state defined in the file is the starting state of the
machine, so this machine will start in `s1` state.

Also note that the directions here refer to the movement of the tape,
`L` means that the tape in being moved left, thus the head moves
right.

```
# Search for the space between the original and the copy.
s2 {
  1 -> 1, L, s2
  _ -> _, L, s3
}

# Search for the end of the copy, and append a 1.
s3 {
  1 -> 1, L, s3
  _ -> 1, R, s4
}

# Go back to the marked 1 in the original, unmark that and start again
# from the symbol to its right.
s4 {
  1 -> 1, R, s4
  _ -> _, R, s4
  X -> 1, L, s1
}

# A dummy state with no transitions, to explicitly mark the halting.
HALT {}
```

The machine will halt when there are no transitions that apply. So for
example, if the machine was in state `s2`, and read an `X` from the
tape, it would halt because there are no transitions that apply when
an `X` is read in this state. However, it is generally better to
explicitly mark when the machine halts by adding a dummy `HALT` state
that has no transitions.

# Installing

First, you will need [stack](http://haskellstack.org/) to be
installed. Once you have that, run `stack install` in the source
directory, stack will compile the project and install
it.

If you want to use the tools without installing it, you can also run
`stack build`, however you will need to prepend `stack exec` when
following the usage examples.

# Usage

After installing, you can try out the various example machines in the
`machines` folder. You need to supply the name of the machine file as
an argument, and the starting tape as the input.

Let's try running the machine we just examined:

```bash
$ echo 111 | turinginterpreter-exe machines/copy
111 111
$ echo 111111 | turinginterpreter-exe machines/copy
111111 111111
```

If you are on a *nix environment, the machines are actually
executable:

```bash
$ echo 1111 | ./machines/copy
1111 1111
```

There are other example machines in the machines directory as well,
such as some binary operations and
[busy beaver](https://en.wikipedia.org/wiki/Busy_beaver)s.

```bash
$ echo 1101 1001 | ./machines/xor
0100
$ echo | ./machines/busybeaver-4
1 111111111111
```
