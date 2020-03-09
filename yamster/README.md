Yamster
=======
> Eats YAML for breakfast.

Ideas for making my Istio config & code gen less messy, save a bit on
manual testing, avoid weary debug sessions, and all that jazz.

At the moment we generate all the YAML in the `deployment` dir except
for `orionadapter.yaml` and `template.yaml`—you still need the shell
scripts for these. The program takes care of dependency tracking and
encodes the basic rules to get a stable Boost mesh. From that you can
easily tweak params to produce derived config sets—debug, QA, etc.

Install [Stack][stack], then

    $ cd yamster
    $ stack run yamster




[stack]: https://haskellstack.org/
    "The Haskell Tool Stack"
