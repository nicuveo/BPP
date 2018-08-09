Brain++
=======

Brain++ is the result of a stupid thought: what if I preprocessed Brainfuck files with the C preprocessor? What if I used BOOST_PP? I could define functions? Constants?

The result is Brain++: a collection of utility functions and constants for Brainfuck. The goal is to provide some basic stack-based instructions to simplify Brainfuck usage.

Features include:
  * numerical constants from 0 to 255,
  * ascii constants covering most of the ascii table,
  * main mathematical binary operators,
  * memory operations (cp, mv...) **(deprecated, because not stack-based)**,
  * boolean logic,
  * string operations,
  * stack manipulation,
  * a few small test files.

For great good.
