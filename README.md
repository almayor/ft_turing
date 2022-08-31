# ft_turing – a Turing Machine simulator written in Haskell

* A student project implementing a single-headed, doubly-infinite-tape Turing machine in Haskell
* The goal is to
    * parse a machine description ([example](machines/00-unary_sub.json))
    * validate it
    * run the machine and log all transitions
* For complete instructions please refer to [docs](https://github.com/fpetras/42-subjects/blob/master/ft_turing.en.pdf)

## Usage

To use the machine you need to have installed [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). You can then run

```sh
$ stack run <machine> <input>
```
where
- `machine` is a file describing the machine
- `input` is a string of symbols, representing initial tape contents.

For example,
```sh
$ stack run machines/00-unary_sub.json "111-11="
```

## Provided machines

A few machines capable of executing simple programs can be found in `machines/`:
* `00-unary_sub` -- unary subtraction


## Acknowledgements

I'm grateful to the entire team behind [School 21](https://21-school.ru) for the opportunity to do these interesting projects.

